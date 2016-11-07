{-|
Module      : Main
Description : Self-tests for SoundCheck
Copyright   : (c) Aaron Harris, 2016
License     : BSD3
-}

||| This module defines tests for the SoundCheck package.
|||
||| Because neither the test suite feature nor executing IO actions in
||| the REPL currently work on Windows, this module is configured as
||| an executable.
module Main

import SoundCheck
import SoundCheck.Arbitrary

%access public export
%default total

-- Type-parametrized tests
--------------------------

||| A test to ensure that the `Arbitrary` instance for the specified
||| type is infinite.  Of course, we can't actually test for an
||| infinite structure, so we settle for checking that it never "runs
||| short"; i.e., `arbitrary k` always has `k` elements.
fullArbitrary : (t : Type) -> Arbitrary t => Nat -> Bool
fullArbitrary t k = length (arbitrary k {ty=t}) == k

||| A test to ensure that the `Arbitrary` instance for the specified
||| numeric type contains elements with all possible signs.
arbitrarySigns : (t : Type) -> (Arbitrary t, Num t, Ord t) => Bool
arbitrarySigns t = all fails tests where
  tests : List (t -> Bool)
  tests = [(0 <=), (<= 0), (/= 0)]
  
-- Single-purpose tests
-----------------------

||| A test to ensure that the `Arbitrary` instance for `()` is
||| sensibly defined.  The type is so simple that the only way we
||| could screw this up is by letting the list be empty, so we just
||| need to find any test that fails.
arbitraryUnit : Bool
arbitraryUnit = fails $ const False {b=()}

||| A test to ensure that test parameters are not synchronized; i.e.,
||| if a test calls for multiple parameters, all combinations of those
||| parameters are tested, not just the diagonal.
paramsNotSync : Bool
paramsNotSync = all fails tests where
  tests : List (Nat -> Nat -> Bool)
  tests = [(==), (>), (<)]

-- Test runner
--------------

||| The main test runner.
runTests : IO ()
runTests = do
  putStrLn "Testing SoundCheck:"
  putStrLn "-------------------"

  -- The basics
  check "True succeeds"                    True
  check "False fails"                      (fails False)
  -- Test that the Arbitrary instances are relatively thorough.
  check "Arbitrary () is complete"         arbitraryUnit
  check "Arbitrary Boolean is complete"    (fails not)
  check "Arbitrary Nat is infinite"        (fullArbitrary Nat)
  check "Arbitrary Int is infinite"        (fullArbitrary Int)
  check "Arbitrary Int has all signs"      (arbitrarySigns Int)
  check "Arbitrary Integer is infinite"    (fullArbitrary Integer)
  check "Arbitrary Integer has all signs"  (arbitrarySigns Integer)
  -- Other tests
  check "Params not synchronized"          paramsNotSync
  check {params = record {testDepth = 1} testDefaults}
    "Non-standard test parameters"         not
  
  putStrLn ""

||| An alias for `runTests`, so this module can be used as an
||| executable.
main : IO ()
main = runTests
