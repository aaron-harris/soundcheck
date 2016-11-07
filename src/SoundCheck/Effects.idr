{-|
Module      : SoundCheck.Effects
Description : Effects wrapper for SoundCheck
Copyright   : (c) Aaron Harris, 2016
License     : BSD3
-}

||| This module defines an alternate front-end to SoundCheck that uses
||| the Effects library instead of basic IO.  This allows programs to
||| more easily incorporate tests into an existing effectful program.
module SoundCheck.Effects

import Effects
import Effect.StdIO

import public SoundCheck.Core

%access export
%default total

||| Report the results of a test to the console.
|||
||| @ name The name of the test (an arbitrary string)
||| @ res The result to report
private
reportTest : (name : String) -> (res : TestResult) -> Eff () [STDIO]
reportTest name res = do
  putStrLn $ "Test " ++ show name ++ ":"
  putStrLn $ "  " ++ show res

||| Run a test, and report the result to the console.
|||
||| Nonstandard test parameters can be supplied as the implicit
||| argument `params`.
|||
||| @ params The `TestParameters` to use for the test
||| @ name The name of the test (an arbitrary string)
||| @ test The test to run
check :  {default testDefaults params : TestParameters}
      -> (name : String)
      -> Testable as ty => (test : ty)
      -> Eff () [STDIO]
check {params} name test = reportTest name result where
  result = runTest test {params=params}
