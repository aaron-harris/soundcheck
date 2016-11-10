{-|
Module      : SoundCheck.Core
Description : Core module for SoundCheck
Copyright   : (c) Aaron Harris, 2016
License     : BSD3
-}

||| This module implements the central core of SoundCheck.
module SoundCheck.Core

import Effects
import Effect.Exception
import Effect.Select
import Effect.State

import public SoundCheck.Arbitrary

%access export
%default total

-- Test parameters
------------------

||| A record type encoding the parameters to be used for the current
||| tests.
record TestParameters where
  constructor MkTestParameters
  ||| The number of test cases to try for each test.
  testDepth : Nat

||| The default test parameters are:
|||
||| * testDepth: 20
testDefaults : TestParameters
testDefaults = MkTestParameters 100

-- Property types
-----------------

||| A datatype representing a possibly-parametrized property.  All
||| parameters must be instances of `ArbShow`, so that appropriate
||| test data can be automatically generated.
|||
||| While this type represents the "canonical" form for a property,
||| your properties don't need to have this exact type.  Most
||| functions that can be viewed as properties should already have a
||| `Testable` implementation, and can be tested directly.
private
data Prop : List Type -> Type where
  ||| A test that always succeeds.
  Succeeds : Prop []

  ||| A test that always fails.
  Fails : Prop []

  ||| A parametrized test.
  ||| @ a The type of the parameter, which must implement `ArbShow`
  ||| @ f Test code, a function of `a` that returns a simpler `Prop`
  Param : ArbShow a => (f : a -> Prop as) -> Prop (a::as)

||| The `Testable` interface generalizes the `Prop` typeclass so that
||| more "ordinary" code (e.g., functions returning booleans) can be
||| tested.
|||
||| @ as The list of parameter types for the test.  As with `Prop`,
||| these must implement `ArbShow`; this is enforced by the type of
||| the `property` method.
interface Testable (as : List Type) ty | ty where

  ||| Canonicalize a `Testable` into a `Prop`.
  property : ty -> Prop as

private
Testable as (Prop as) where
  property = id

||| A boolean test succeeds when it is `True` and fails when it is
||| `False`.
Testable [] Bool where
  property True  = Succeeds
  property False = Fails

(ArbShow a, Testable as ty) => Testable (a::as) (a -> ty) where
  property f = Param (property . f)

-- Result type
--------------

||| A datatype representing the result of a test.
data TestResult : Type where

  ||| A successful test carries no further information.
  Success : TestResult
   
  ||| A failing test includes information about the parameters that
  ||| caused it to fail.
  |||
  ||| @ l A list of strings, each describing a parameter
  Failure : (l : List String) -> TestResult

Show TestResult where
  show Success      = "Success"
  show (Failure []) = "Failure"
  show (Failure xs) = "Failure: " ++ concat (intersperse ", " xs)

||| Combine `TestResults`s left-to-right, taking the first failure.
Semigroup TestResult where
  Success <+> q = q
  p       <+> _ = p

Monoid TestResult where
  neutral = Success

-- Running tests
----------------

||| Test the given property, returning a `TestResult`.
|||
||| This is an effectful computation, so in principle it could be used
||| in a variety of contexts.  The two default contexts supporting all
||| of the effects used are `Maybe` and `List`.  SoundCheck uses the
||| `Maybe` context to report a single failing test; if the `List`
||| context were used instead, all failing tests (up to the current
||| `testDepth`) would be reported.
|||
||| Counter-intuitively, this function returns only `Failure` and
||| raises an exception on `Success`.  This makes sense if you
||| consider that running a test is a search for failing test cases,
||| not successful ones.
|||
||| @ params The `TestParameters` to use
||| @ p The property to test
|||     
||| @ l A list of strings, describing parameters already bound.  This
||| is mostly used recursively; top-level invocations should use an
||| empty list.
private
runProp :  (params : TestParameters)
        -> (p      : Prop as)
        -> (l      : List String)
        -> Eff TestResult [SELECT, EXCEPTION TestResult]
runProp _      Succeeds  _                = raise Success
runProp _      Fails     xs               = pure $ Failure $ reverse xs
runProp params (Param f) xs {as = (a::_)} = do
  (x,s) <- select $ arbShow {ty=a} $ testDepth params
  runProp params (f x) (s :: xs)

||| Run a test, returning a `TestResult` object.
|||
||| Non-standard test parameters can be supplied as the implicit
||| argument `params`.
|||
||| @params The test parameters to use
||| @test The test to run
runTest :  {default testDefaults params : TestParameters}
        -> Testable as ty => (test : ty)
        -> TestResult
runTest test {params} = fromMaybe Success result where
  result = run $ runProp params (property test) []

||| Return `True` if the given test fails, and `False` if it succeeds.
|||
||| Nonstandard test parameters can be supplied as the implicit
||| argument `params`.
|||
||| @ params The `TestParameters` to use for the test
||| @ test The test to run
fails :  {default testDefaults params : TestParameters}
      -> Testable as ty => (test : ty)
      -> Bool
fails test {params} with (runTest test {params=params})
  | Success   = False
  | Failure _ = True
