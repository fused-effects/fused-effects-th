# fused-effects-th

[![GitHub CI](https://github.com/fused-effects/fused-effects-th/workflows/CI/badge.svg)](https://github.com/fused-effects/fused-effects-th/actions)
[![Hackage](https://img.shields.io/hackage/v/fused-effects-th.svg?logo=haskell)](https://hackage.haskell.org/package/fused-effects-th)
[![BSD-3-Clause license](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE)

This package provides Template Haskell helpers for fused-effects. The `makeSmartConstructors` splice, given the name of a GADT defining an effect, iterates through the possible constructors and generates functions that construct effects using `send`. That is to say, given the standard `State` type:

``` haskell
data State s m k where
  Get :: State s m s
  Put :: s -> State s m ()
```

calling `makeSmartConstructors ''State` generates the following code (cleaned up a little from the native Template Haskell output):

``` haskell
get ::
  forall s sig m
  Has (State s) sig m =>
  m s
get = send Get
{-# INLINEABLE get #-}
 put ::
  forall s sig m.
  Has (State s) sig m =>
  s ->
  m ()
put a = send (Put a)
{-# INLINEABLE put #-}
```

Bug reports are welcome on the [issue tracker](https://github.com/fused-effects/fused-effects-th/issues).
