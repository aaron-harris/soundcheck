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

-- Arbitrary Interface
----------------------

||| A much-simplified version of QuickCheck's `Arbitrary` typeclass
||| that can be used to generate test data.
|||
||| Because failing tests need to display the data that caused them to
||| fail, types implementing this interface also need to implement
||| `Show`.  If that's impossible, implement `ArbShow` instead of
||| `Arbitrary`.
public export
interface Arbitrary ty where

  ||| A list of arbitrary data.
  |||
  ||| Note that the list returned may have fewer than `k` elements;
  ||| this usually means that the type implementing this interface has
  ||| fewer than `k` inhabitants.
  ||| @ k The number of pieces of data requested
  arbitrary : (k : Nat) -> List ty

-- Implementations
------------------

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

-- ArbShow Interface
--------------------

||| This is a weaker version of `Arbitrary` for use with types where
||| implementing `Show` is difficult or impossible (e.g., `Type` or
||| any kind of function type).
|||
||| Unfortunately, it is currently impossible to write an unnamed
||| implementation for this interface.  The default implementation
||| that combines existing `Arbitrary` and `Show` implementations is
||| otherwise completely polymorphic, and the typeclass constraints do
||| not prevent it from overlapping with other `ArbShow`
||| implementations, even for types that do not implement `Show`.  To
||| deal with this limitation, use named instances and wrap calls to
||| `check` in `using implementation` blocks.
public export
interface ArbShow ty where

  ||| A list of pairs `(x,s)`, where `x` is a piece of arbitrary data
  ||| and `s` is a string representing `x`.
  arbShow : (k : Nat) -> List (ty, String)

||| Given a function `f` and a list `xs`, zip together `xs` and its
||| image under `map f`, returning a list of pairs `(x, f x)`.
|||
||| @f A function to map over the list
||| @xs The list
private
zipMap : (f : a -> b) -> (xs : List a) -> List (a,b)
zipMap f xs = zip xs (f <$> xs)

(Arbitrary ty, Show ty) => ArbShow ty where
  arbShow k = zipMap show $ arbitrary k
