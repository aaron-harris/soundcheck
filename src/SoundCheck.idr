{-|
Module      : SoundCheck
Description : Basic property testing for Idris
Copyright   : (c) Aaron Harris, 2016
License     : BSD3
-}

||| SoundCheck is a property testing library for Idris.  It is
||| inspired by Haskell's QuickCheck library, but is not nearly so
||| ambitious.
module SoundCheck

import Effects
import Effect.StdIO

import public SoundCheck.Arbitrary
import public SoundCheck.Core
import SoundCheck.Effects as SCE

%access export
%default total

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
      -> IO ()
check {params} name test = run $ SCE.check name test {params=params}
