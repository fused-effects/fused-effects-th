{- |
Copyright: (c) 2020 Patrick Thomson
SPDX-License-Identifier: BSD-3-Clause
Maintainer: Patrick Thomson <patrickt@github.com>

Template Haskell helpers for fused-effects.
-}

module FusedEffectsTh
       ( someFunc
       ) where


someFunc :: IO ()
someFunc = putStrLn ("someFunc" :: String)
