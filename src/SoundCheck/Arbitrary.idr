{-|
Module      : SoundCheck.Arbitrary
Description : An interface for generating arbitrary test data
Copyright   : (c) Aaron Harris, 2016
License     : BSD3
-}

||| This module defines the `Arbitrary` interface, which allows
||| SoundCheck to generate arbitrary test data for types that
||| implement it.
module SoundCheck.Arbitrary

%access export
%default total

||| A much-simplified version of QuickCheck's `Arbitrary` typeclass
||| that can be used to generate test data.
|||
||| The `Show` constraint is required so that failing tests can
||| display the data that caused them to fail.
public export
interface Show ty => Arbitrary ty where

  ||| A list of arbitrary data.
  |||
  ||| Note that the list returned may have fewer than `k` elements;
  ||| this usually means that the type implementing this interface has
  ||| fewer than `k` inhabitants.
  ||| @ k The number of pieces of data requested
  arbitrary : (k : Nat) -> List ty

Arbitrary () where
  arbitrary k = take k [()]

Arbitrary Bool where
  arbitrary k = take k [False, True]

Arbitrary Nat where
  arbitrary k = take k [0..]

||| Combine two streams by alternating elements.
private
alternate : Stream a -> Stream a -> Stream a
alternate (x::xs) (y::ys) = x :: y :: alternate xs ys

Arbitrary Int where
  arbitrary k = take k $ 0 :: alternate [1..] (negate <$> [1..])

Arbitrary Integer where
  arbitrary k = take k $ 0 :: alternate [1..] (negate <$> [1..])
