{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Main
  ( main,
  )
where

import Control.Carrier.State.Strict (evalState)
import Control.Effect.State hiding (get, put)
import Control.Effect.TH
import Test.Tasty
import Test.Tasty.HUnit

makeEffectDefinitions ''State

main :: IO ()
main = defaultMain $ do
  testCase "Generated State functions" $ do
    assertEqual "get" (run $ evalState "hello" get) "hello"
    assertEqual "get-put" (run $ evalState "bad" (put "good" *> get)) "good"
