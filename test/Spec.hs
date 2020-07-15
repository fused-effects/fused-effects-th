{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main
  ( main,
  )
where

import Control.Carrier.State.Strict (evalState)
import Control.Effect.State hiding (get, put)
import Control.Effect.TH
import Control.Exception (SomeException, try)
import Data.Either
import Language.Haskell.TH (runQ)
import System.IO
import Test.Tasty
import Test.Tasty.HUnit

makeEffectDefinitions ''State

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
    assertThrows "Bool" (runQ (makeEffectDefinitions ''Bool))
    assertThrows "Ill-formed GADT" (runQ (makeEffectDefinitions ''BadGADT))

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
