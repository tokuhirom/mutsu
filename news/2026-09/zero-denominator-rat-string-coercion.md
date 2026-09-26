# Every string coercion of a zero-denominator Rat dies

`~(1/0)`, `"a" ~ 1/0`, `1/0 eq "x"`, `"$x"`, `"{1/0}"`, `"@a[]"`, `[1/0].join(",")`
and `join(",", 1/0)` printed `Inf` where Rakudo dies with
`Attempt to divide 1 by zero when coercing Rational to Str`. Only the `.Str`/`.gist`
method calls and `say`/`put` checked; the coercions that never go through method
dispatch (the `StrCoerce` and `StringConcat` opcodes, the infix `~`/string-comparator
operand coercion, every `join` entry point, and the compile-time folding of `~`)
rendered through the infallible pure stringifier.

They now share one guard, `runtime::utils::check_str_coercion_zero_denominator`,
built on the aggregate walk from #9608. The exception also gained Rakudo's `details`
attribute and message suffix (`when coercing Rational to Str`), and a zero numerator
is left out of the message as in Rakudo (`0/0` reads `Attempt to divide by zero ...`).
The hand-built copy of that exception in the interpreter's `.Str` fallback is gone.
(#9621)
