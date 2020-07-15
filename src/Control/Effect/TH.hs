{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Effect.TH
  ( makeEffectDefinitions,
  )
where

import Control.Algebra
import Control.Monad (join)
import Data.Char (toLower)
import Data.Foldable
import Data.Traversable
import Language.Haskell.TH as TH

data Blueprint = Blueprint
  { typeName :: TH.Name,
    effectTypeVars :: [TH.TyVarBndr],
    monadTypeVar :: TH.TyVarBndr,
    forallConstructor :: TH.Con
  }

data PerDecl = PerDecl
  { ctorName :: TH.Name,
    functionName :: TH.Name,
    ctorArgs :: [TH.Type],
    returnType :: TH.Type,
    blueprint :: Blueprint
  }

makeEffectDefinitions :: Name -> TH.DecsQ
makeEffectDefinitions typ =
  TH.reify typ >>= \case
    TH.TyConI (TH.DataD _ctx typeName tyvars _kind cons _derive) -> do
      -- Pick out the `m` argument. We can drop `k` on the floor.
      (effectTypeVarsWithoutSig, monadTypeVar) <- case reverse tyvars of
        _cont : monad : rest -> pure (reverse rest, monad)
        _ -> fail ("Effect types need at least two type arguments: a monad `m` and continuation `k`.")
      -- Continue, recording the various relevant data from the type in question.
      let effectTypeVars = effectTypeVarsWithoutSig ++ [TH.PlainTV (mkName "sig")]
      join <$> traverse (\forallConstructor -> makeDeclaration Blueprint {..}) cons
    other -> fail ("Can't generate definitions for a non-data-constructor: " <> pprint other)

makeDeclaration :: Blueprint -> TH.DecsQ
makeDeclaration blueprint@Blueprint {..} = do
  (names, ctorArgs, returnWithResult) <- case forallConstructor of
    TH.ForallC _vars _ctx (TH.GadtC names bangtypes returnType) ->
      pure (names, fmap snd bangtypes, returnType)
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
makeSignature PerDecl {blueprint = Blueprint {..}, ..} =
  let sigVar = last effectTypeVars
      rest = init effectTypeVars
      getTyVar = \case
        TH.PlainTV t -> t
        TH.KindedTV t _ -> t
      monadName = varT (getTyVar monadTypeVar)
      invocation = foldl' (\a b -> a `appT` b) (conT typeName) (fmap (varT . getTyVar) rest)
      hasConstraint = [t|Has $(parensT invocation) $(varT (mkName "sig")) $(monadName)|]
      folded = foldr (\a b -> arrowT `appT` pure a `appT` b) (monadName `appT` pure returnType) ctorArgs
   in TH.sigD functionName (TH.forallT (rest ++ [sigVar, monadTypeVar]) (TH.cxt [hasConstraint]) folded)
