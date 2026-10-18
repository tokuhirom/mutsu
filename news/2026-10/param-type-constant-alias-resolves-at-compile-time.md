# A `constant` type alias in a signature is resolved at compile time

`constant time = int64; sub f(time \year) { ... }` now gives `f` the signature
`:(int64 \year)` when it is compiled, as rakudo does, and `my time $t` is an
`int64` variable from the start. The alias used to be looked up by name when the
call bound its arguments, through the frames that called it, so a caller with a
same-named sigilless variable (`my \time = 1577934245`) broke every call with
"expected time but got Int". This was the failure in Timezones::ZoneInfo's
`t/01-accuracy.t` and `t/02-roundtrip.t`; all seven of its test files now pass,
as under rakudo (#13728).
