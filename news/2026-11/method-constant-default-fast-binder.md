# A method call that omits a constant-defaulted parameter stays on the fast binder

The method fast binder sent a call to the general binder whenever a positional
parameter with a default was left out, even when the default was a constant
the sub light path already binds directly (`$n = 5`, `$s = "x"`, `$w = *`,
`$p = \()`, checked against the parameter's type once by
`CompiledFunction::const_fill_for_param`). Methods now share that fill: the
omitted parameter binds the constant on the fast binder, and an `is copy`
parameter gets a fresh container for it.

Also, a method call that asks for a container -- the right side of `:=`, or
the last statement of an `is raw` routine -- was refused by the plain-method
lane outright. It now keys the lane apart from a plain call (the request is
part of the key), so it can replay its call plan like any other call.

Per call (callgrind slope, profiling build): `$c.d(1)` with
`method d($a, $p = 5)` 25,957 -> 13,258 Ir; `my $z := $c.r($y)` with
`method r(Mu $x is raw) is raw` 34,050 -> 27,713 Ir. AttrX::Mooish does not
move (6.005M -> 6.001M Ir per round): its defaulted methods also have a named
parameter with an enum default and `is copy` (`invoke-opt`, #14854) or a
non-constant default (`attr-var`, #18320). Stage 5 of #18267.
