{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Defines splices that cut down on boilerplate associated with declaring new effects.
module Control.Effect.TH
  ( makeSmartConstructors,
  )
where

import Control.Algebra
import Control.Monad (join)
import Data.Char (toLower)
import Data.Foldable
import qualified Data.List as List
import Data.Monoid (Ap (..))
import Data.Traversable
import Language.Haskell.TH (appT, mkName, varT)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Datatype.TyVarBndr as THC
import Optics

data PerEffect = PerEffect
  { _perEffectName :: TH.TypeQ,
    _perEffectTypeVars :: [THC.TyVarBndrVis]
  }

makeFieldLabels ''PerEffect

data PerCtor = PerCtor
  { _perCtorArgs :: [TH.TypeQ],
    _perCtorConstraints :: [TH.TypeQ],
    _perCtorTypeVars :: [THC.TyVarBndrSpec],
    _perCtorUppercaseName :: TH.Name,
    _perCtorLowercaseName :: TH.Name,
    _perCtorReturnType :: TH.TypeQ
  }

makeFieldLabels ''PerCtor

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
      let perEffect = PerEffect (TH.conT tn) tvs
       in join <$> traverse (makeDeclaration perEffect) constructors
    -- Die otherwise.
    other ->
      fail ("Can't generate definitions for a non-data-constructor: " <> TH.pprint other)

makeDeclaration :: PerEffect -> TH.Con -> TH.DecsQ
makeDeclaration perEffect forallConstructor = do
  -- Start by extracting the relevant parts of this particular constructor.
  (names, ctorArgs, constraints, returnType, ctorTyVars) <- case forallConstructor of
    TH.ForallC vars ctx (TH.GadtC names bangtypes (TH.AppT _ final)) ->
      pure (names, fmap snd bangtypes, ctx, final, vars)
    _ ->
      fail ("BUG: expected forall-qualified constructor, but didn't get one, got: " <> TH.pprint forallConstructor)
  -- Then iterate over the names of the constructors, emitting an injected
  -- method per name.
  fmap join . for names $ \ctorName -> do
    let downcase (x : xs) = mkName (toLower x : xs)
        downcase [] = error "attempted to downcase empty name"
        decl =
          PerCtor
            { _perCtorUppercaseName = ctorName,
              _perCtorLowercaseName = downcase . TH.nameBase $ ctorName,
              _perCtorArgs = fmap pure ctorArgs,
              _perCtorReturnType = pure returnType,
              _perCtorTypeVars = ctorTyVars,
              _perCtorConstraints = fmap pure constraints
            }
    sign <- makeSignature perEffect decl
    func <- makeBody decl
    prag <- makePragma decl
    pure [sign, func, prag]

-- generates {-# INLINEABLE $name #-}
makePragma :: PerCtor -> TH.DecQ
makePragma ctor =
  TH.pragInlD (ctor ^. #lowercaseName) TH.Inlinable TH.FunLike TH.AllPhases

-- generates $name [args...] = send ($Name args)
makeBody :: PerCtor -> TH.DecQ
makeBody ctor = TH.funD (ctor ^. #lowercaseName) [TH.clause pats body []]
  where
    body = TH.normalB [e|send ($(applies))|]
    pats = fmap TH.varP names
    -- Glue together the parameter to 'send', fully applied
    applies = foldl' TH.appE (TH.conE (ctor ^. #uppercaseName)) (fmap TH.varE names)
    -- A source of a, b, c... names for function parameters.
    names = fmap (mkName . pure) (take (length (ctor ^. #args)) ['a' .. 'z'])

-- generates $name :: forall [vars...] sig m => Has ($Name vars) sig m => m $result
makeSignature :: PerEffect -> PerCtor -> TH.DecQ
makeSignature eff ctor = do
  -- Can't use List.unsnoc here because it was added fairly recently.
  (rest, monadVar) <-
    if List.null (ctor ^. #typeVars)
      then fail "Error: not enough variables in effect constructor (needs at least two)"
      else pure (ctor ^. #typeVars % to init, ctor ^. #typeVars % to last)
  let sigVar = THC.plainTVSpecified $ mkName "sig"
      var = varT . THC.tvName
      -- Look up any required type variable from the effect type, excluding `m` and `k`.
      relevantEffectTyVars = take (length (eff ^. #typeVars) - 2) rest
      -- Build the parameter to Has by consulting the number of required type parameters.
      invocation = foldl' appT (eff ^. #name) (var <$> relevantEffectTyVars)
      -- Build the Has constraint by applying the above to `sig` and `m`.
      hasConstraint = [t|Has ($(invocation)) $(var sigVar) $(var monadVar)|]
      -- Build the type signature by folding with (->) over the function arguments as needed.
      foldedSig = foldr (\a b -> [t|$a -> $b|]) [t|$(var monadVar) $(ctor ^. #returnType)|] (ctor ^. #args)
      -- Glue together the Has and the per-constructor constraints.
      allConstraints = TH.cxt (hasConstraint : (ctor ^. #constraints))
      -- Apply the above constraints to the type signature.
      withForall = TH.forallT (rest <> [monadVar, sigVar]) allConstraints foldedSig
   in TH.sigD (ctor ^. #lowercaseName) withForall
