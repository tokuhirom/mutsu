# `f() + 1` in sink context is a useless use, as on rakudo

The "Useless use of ... in sink context" worry used to name a pure operator
only when every operand was itself side-effect free, so `f() + 1;`,
`-f();` and `$x.abs + 1;` passed silently. rakudo reports them all: the
operator's value is discarded whatever its operands do (#18973).

mutsu now does too. A whole statement is named by its own source text, an
element of a sunk list (`f() + 1, 2;`) by the rendering, which now spells a
parenthesized call. A parenthesized expression (`(f() + 1);`) and a routine's
last statement (its return value) stay quiet, as on rakudo.
