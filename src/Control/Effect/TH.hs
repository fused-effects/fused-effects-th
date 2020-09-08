{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Defines splices that cut down on boilerplate associated with declaring new effects.
module Control.Effect.TH
  ( makeSmartConstructors,
  )
where

import Control.Algebra
import Control.Monad (join)
import Data.Char (toLower)
import Data.Foldable
import Data.Traversable
import Optics.Core
import Language.Haskell.TH.Optics
import Language.Haskell.TH as TH

data PerEffect = PerEffect
  { typeName :: TH.Name,
    forallConstructor :: TH.Con,
    effectTyVars :: [TyVarBndr]
  }

data PerDecl = PerDecl
  { ctorName :: TH.Name,
    functionName :: TH.Name,
    ctorArgs :: [TH.Type],
    returnType :: TH.Type,
    perEffect :: PerEffect,
    extraTyVars :: [TyVarBndr],
    extraConstraints :: [TH.Type]
  }

-- | Given an effect type, this splice generates functions that create per-constructor request functions.
--
-- That is to say, given the standard @State@ type
--
-- @
--   data State s m k where
--     Get :: State s m s
--     Put :: s -> State s m ()
-- @
--
-- an invocation of @makeSmartConstructors ''State@ will generate code that looks like
--
--
-- >   get ::
-- >     forall (s :: Type) sig (m :: Type -> Type).
-- >     Has (State s) sig m =>
-- >     m s
-- >   get = send Get
-- >   {-# INLINEABLE get #-}
-- >    put ::
-- >     forall (s :: Type) sig (m :: Type -> Type).
-- >     Has (State s) sig m =>
-- >     s ->
-- >     m ()
-- >   put a = send (Put a)
-- >   {-# INLINEABLE put #-}
--
--
-- The type variables in each declared function signature will appear in the order
-- they were defined in the effect type.
--
makeSmartConstructors :: Name -> TH.DecsQ
makeSmartConstructors typ =
  -- Lookup the provided type name
  TH.reify typ >>= \case
    -- If it's a type constructor, record its type name
    TH.TyConI (TH.DataD _ctx typeName effectTyVars _kind constructors _derive) ->
      join <$> traverse (\forallConstructor -> makeDeclaration PerEffect {typeName, forallConstructor, effectTyVars}) constructors
    other -> fail ("Can't generate definitions for a non-data-constructor: " <> pprint other)

makeDeclaration :: PerEffect -> TH.DecsQ
makeDeclaration perEffect@PerEffect {..} = do
  (names, ctorArgs, extraConstraints, returnWithResult, extraTyVars) <- case forallConstructor of
    TH.ForallC vars ctx (TH.GadtC names bangtypes returnType) ->
      pure (names, fmap snd bangtypes, ctx, returnType, vars)
    _ ->
      fail ("BUG: expected forall-qualified constructor, but didn't get one")
  returnType <- case returnWithResult of
    AppT _ final -> pure final
    _ -> fail ("BUG: Couldn't get a return type out of " <> pprint returnWithResult)
  fmap join . for names $ \ctorName -> do
    let downcase = \case
          x : xs -> toLower x : xs
          [] -> []
        functionName = TH.mkName . downcase . TH.nameBase $ ctorName
    let decl = PerDecl {..}
    sign <- makeSignature decl
    func <- makeFunction decl
    prag <- makePragma decl
    pure [sign, func, prag]

makePragma :: PerDecl -> TH.DecQ
makePragma PerDecl {..} =
  TH.pragInlD functionName TH.Inlinable TH.FunLike TH.AllPhases

makeFunction :: PerDecl -> Q Dec
makeFunction d =
  TH.funD (functionName d) [makeClause d]

makeClause :: PerDecl -> ClauseQ
makeClause PerDecl {..} = TH.clause pats body []
  where
    body = TH.normalB [e|send ($(applies))|]
    pats = fmap TH.varP names
    applies = foldl' (\e n -> e `appE` varE n) (conE ctorName) names
    names = fmap (mkName . pure) (take (length ctorArgs) ['a' .. 'z'])

makeSignature :: PerDecl -> TH.DecQ
makeSignature PerDecl {perEffect = PerEffect {..}, ..} =
  let sigVar = plainTV (mkName "sig")
      rest = init extraTyVars
      monadTV = last extraTyVars
      getTyVar = view (name @TH.TyVarBndr)
      monadName = varT (getTyVar monadTV)
      invocation = foldl' appT (conT typeName) (fmap (varT . getTyVar) (take (length effectTyVars - 2) rest))
      hasConstraint = [t|Has $(parensT invocation) $(varT (mkName "sig")) $(monadName)|]
      foldedSig = foldr (\a b -> arrowT `appT` pure a `appT` b) (monadName `appT` pure returnType) ctorArgs
   in TH.sigD functionName (TH.forallT (rest ++ [monadTV, sigVar]) (TH.cxt (hasConstraint : fmap pure extraConstraints)) foldedSig)
