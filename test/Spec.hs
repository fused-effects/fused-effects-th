{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -ddump-splices #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}

module Main
  ( main,
  )
where

import Control.Carrier.State.Strict (evalState)
import Control.Effect.State hiding (get, put)
import Control.Effect.Reader hiding (ask, local)
import Control.Effect.Writer hiding (tell, listen, censor)
import Control.Effect.TH
import Control.Exception (SomeException, try)
import Data.Either
import Language.Haskell.TH (runQ)
import System.IO
import Test.Tasty
import Test.Tasty.HUnit
import Data.Kind (Type)

data Go (m :: Type -> Type) k where
  App :: String -> Go m ()

data Same (m :: Type -> Type) k where
  Same :: Same m ()

data Kinded (s :: Type) (m :: Type -> Type) k where
  Kinded :: s -> Kinded s m ()

makeSmartConstructors ''Go
makeSmartConstructors ''State
makeSmartConstructors ''Same
makeSmartConstructors ''Kinded

-- Need to ensure that if a constructor introduces a new type variable,
-- that it is introduced in the corresponding invocation. The question is
-- where we can put it. Probably before `sig m`.

assertThrows :: String -> IO a -> Assertion
assertThrows msg go =
  fmap isLeft (try @SomeException go) >>= assertBool msg

testState :: TestTree
testState = testCase "Generated State functions" $ do
  assertEqual "get" (run $ evalState "hello" get) "hello"
  assertEqual "get-put" (run $ evalState "bad" (put "good" *> get)) "good"

data BadGADT where
  BadGADT :: BadGADT

testDataErrors :: TestTree
testDataErrors =
  testCase "Bad data types" $ do
    assertThrows "Bool" (runQ (makeSmartConstructors ''Bool))
    assertThrows "Ill-formed GADT" (runQ (makeSmartConstructors ''BadGADT))

main :: IO ()
main = do
  hClose stderr -- silences TH warnings
  defaultMain $
    testGroup
      "Language.Haskell.TH"
      [ testGroup
          "Behavior"
          [ testState
          ],
        testGroup
          "Template Haskell errors"
          [ testDataErrors
          ]
      ]
