# Plain instances stringify by identity; `.open` keeps an IO::Handle subclass

Found by taking Test::Builder through the ecosystem roulette. A plain user-class
instance stringified as `Name()`, so `isnt $a, $b` on two distinct objects with
equal attributes failed; it now stringifies as `Name<identity>` like Rakudo's
`Mu.Str`. Separately, `.open` on an `IO::Handle` subclass returned a bare
`IO::Handle`, dropping the subclass and its attributes; it now returns the
subclass instance with the opened handle's state overlaid. Both Test::Builder
test files reach parity locally.
