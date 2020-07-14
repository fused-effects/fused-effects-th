{-# LANGUAGE LambdaCase #-}
module Control.Effect.TH
  ( makeEffectDefinitions
  ) where

import Language.Haskell.TH as TH
import Data.Char (toLower)
import Control.Monad (join)

makeSignature :: [TH.Type] -> TH.Type -> TH.DecQ
makeSignature = error "not implemented"

makeClause :: [TH.Type] -> TH.Type -> [TH.ClauseQ]
makeClause = error "not implemented"

makeDeclaration :: TH.Con -> TH.DecsQ
makeDeclaration = \case
  TH.GadtC [name] bangtypes ret -> do
    let downcase = \case
          x:xs -> toLower x : xs
          []   -> []
        lowered = mkName . downcase . TH.nameBase $ name
        types = fmap snd bangtypes
    sig <- makeSignature types ret
    bod <- TH.funD lowered (makeClause types ret)
    pure [sig, bod]


  TH.GadtC names _ _ -> fail ("Got too many names: " <> show names)
  other -> fail ("All effects need to be GADTs, was provided: " <> pprint other)

makeEffectDefinitions :: Name -> TH.DecsQ
makeEffectDefinitions typ =
  TH.reify typ >>= \case
    TH.TyConI (TH.DataD _ctx _name _vars _kind cons _derive) -> join <$> traverse makeDeclaration cons
    other -> fail ("Can't generate definitions for a non-data-constructor: " <> pprint other)
