# A default on a slurpy parameter is X::Parameter::Default everywhere

`-> *@ = 42 {}`, `-> |c = 42 {}` and `sub f(| = 1) {}` now throw
`X::Parameter::Default` as rakudo does: pointy blocks run the same signature
validator as `sub`, and a default after a capture parameter is read instead of
being a syntax error (the anonymous capture is reported as an anonymous
slurpy). `'a' x Inf` now returns a `Failure` wrapping `X::NYI` ("Cat object
not yet implemented. Sorry.") instead of throwing `X::Numeric::CannotConvert`.
With both, `roast/APPENDICES/A02-some-day-maybe/misc.t` passes 6/6 and is
whitelisted; three stale `TODO_roast/BLOCKERS.md` rows (this file, plus
`S06-operator-overloading/infix.t` and `S09-multidim/XX-POS-on-undimensioned.t`,
both already whitelisted) are removed (#16393).
