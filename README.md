# soundcheck

SoundCheck is a rudimentary testing library for Idris, along the lines
of Haskell's QuickCheck.

To use SoundCheck, import the `SoundCheck` module, write a test (any
boolean-valued function with `Arbitrary` parameters will do), and pass
your test to the `check` IO action.  The results of the test will be
printed to the console.

Alternatively, you can use the `SoundCheck.Effects` module, which
exposes the same `check` function with the result type `Eff ()
[STDIO]`, for use with the Idris Effects package.

`Arbitrary` instances are only provided for the types `()`, `Bool`,
`Nat`, `Int`, and `Integer`.  When implementing your own `Arbitrary`
instances, note that this interface only supports parameter types that
have `Show` instances.  For parameter types that don't, you should
implement `ArbShow` instead of `Arbitrary`; however, such an
implementation will overlap with SoundCheck's default implementation,
and you will need to name your implementation and wrap your tests in a
`using implementation` block to get this to work.

Finally, note that development is not currently ongoing on this
project, since I am not currently doing much with Idris.
