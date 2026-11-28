# Rat arithmetic takes a fast path in the arithmetic opcodes

The `+`, `-`, `*` and `/` opcodes now answer a plain `Rat`/`Int` pair (or, for
`/`, two `Int`s) directly when the result's parts fit an i64 `Rat` (#19312).
Before, such a pair left the Int and Num fast paths, looked up `$*RAT-OVERFLOW`
dynamically, went through the junction-aware binary evaluator and the numeric
bridge coercion, and only then reached `arith_add`. The fast path calls the same
shared formula `rat_*_checked` uses (`rat_parts` in `src/builtins/arith/rat.rs`,
now the one place the four formulas live), so it gives the identical reduced
value. A zero divisor, a FatRat operand, a user operator, and any result whose
parts do not fit (whose `BigRat`/`Num`/`FatRat` degradation `$*RAT-OVERFLOW`
decides) keep the general path.

`my $t = 0; for ^N -> $i { $t = $t + 1/3 }` went from 8,241 to 2,244
instructions per iteration (callgrind slope, release build); rakupp v5.9.0 runs
it in 562. Most of what remains is the generic loop around it: a Rat slot is
not admitted by loop kernels.
