{-# LANGUAGE LambdaCase #-}
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
import Data.Monoid (Ap (..))
import Data.Traversable
import Language.Haskell.TH (appT, arrowT, mkName, varT)
import qualified Language.Haskell.TH as TH

data PerEffect = PerEffect
  { effectType :: TH.TypeQ,
    effectTyVarCount :: Int,
    forallConstructor :: TH.Con
  }

data PerDecl = PerDecl
  { ctorArgs :: [TH.TypeQ],
    ctorConstraints :: [TH.TypeQ],
    ctorName :: TH.Name,
    ctorTyVars :: [TH.TyVarBndr],
    functionName :: TH.Name,
    gadtReturnType :: TH.TypeQ,
    perEffect :: PerEffect
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
makeSmartConstructors :: TH.Name -> TH.DecsQ
makeSmartConstructors typ =
  -- Lookup the provided type name.
  TH.reify typ >>= \case
    -- If it's a type constructor, record its type name.
    TH.TyConI (TH.DataD _ctx tn tvs _kind constructors _derive) ->
      let perEffect = PerEffect (TH.conT tn) (length tvs)
       in getAp (foldMap (Ap . makeDeclaration . perEffect) constructors)
    -- Die otherwise.
    other ->
      fail ("Can't generate definitions for a non-data-constructor: " <> TH.pprint other)

makeDeclaration :: PerEffect -> TH.DecsQ
makeDeclaration perEffect@PerEffect {..} = do
  -- Start by extracting the relevant parts of this particular constructor.
  (names, ctorArgs, constraints, returnType, ctorTyVars) <- case forallConstructor of
    TH.ForallC vars ctx (TH.GadtC names bangtypes (TH.AppT _ final)) ->
      pure (names, fmap snd bangtypes, ctx, final, vars)
    _ ->
      fail ("BUG: expected forall-qualified constructor, but didn't get one")
  -- Then iterate over the names of the constructors, emitting an injected
  -- method per name.
  fmap join . for names $ \ctorName -> do
    let downcase (x : xs) = mkName (toLower x : xs)
        downcase [] = error "attempted to downcase empty name"
        decl =
          PerDecl
            { ctorName = ctorName,
              functionName = downcase . TH.nameBase $ ctorName,
              ctorArgs = fmap pure ctorArgs,
              gadtReturnType = pure returnType,
              perEffect = perEffect,
              ctorTyVars = ctorTyVars,
              ctorConstraints = fmap pure constraints
            }
    sign <- makeSignature decl
    func <- makeFunction decl
    prag <- makePragma decl
    pure [sign, func, prag]

makePragma :: PerDecl -> TH.DecQ
makePragma PerDecl {..} =
  TH.pragInlD functionName TH.Inlinable TH.FunLike TH.AllPhases

makeFunction :: PerDecl -> TH.DecQ
makeFunction d =
  TH.funD (functionName d) [makeClause d]

makeClause :: PerDecl -> TH.ClauseQ
makeClause PerDecl {..} = TH.clause pats body []
  where
    body = TH.normalB [e|send ($(applies))|]
    pats = fmap TH.varP names
    -- Glue together the parameter to 'send', fully applied
    applies = foldl' (\e n -> e `TH.appE` TH.varE n) (TH.conE ctorName) names
    -- A source of a, b, c... names for function parameters.
    names = fmap (mkName . pure) (take (length ctorArgs) ['a' .. 'z'])

makeSignature :: PerDecl -> TH.DecQ
makeSignature PerDecl {perEffect = PerEffect {..}, ..} =
  let sigVar = mkName "sig"
      (rest, monadTV) = (init ctorTyVars, last ctorTyVars)
      getTyVar =
        varT . \case
          TH.PlainTV n -> n
          TH.KindedTV n _ -> n
      monadName = getTyVar monadTV
      -- Build the parameter to Has by consulting the number of required type parameters.
      invocation = foldl' appT effectType (fmap getTyVar (take (effectTyVarCount - 2) rest))
      hasConstraint = [t|Has ($(invocation)) $(varT sigVar) $(monadName)|]
      -- Build the type signature by folding with (->) over the function arguments as needed.
      foldedSig = foldr (\a b -> arrowT `appT` a `appT` b) (monadName `appT` gadtReturnType) ctorArgs
      -- Glue together the Has and the per-constructor constraints.
      allConstraints = TH.cxt (hasConstraint : ctorConstraints)
   in TH.sigD functionName (TH.forallT (rest ++ [monadTV, TH.plainTV sigVar]) allConstraints foldedSig)
