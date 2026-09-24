# Every form of an integer operator now shares one routine, and three interpreter panics are gone

After ADR-0117 merged the drifting `Str` method / `nqp::` / TRIR copies of each string
primitive, an audit of the other families found the same pattern for Raku's integer operators.
`div`, `mod`, `+&`/`+|`/`+^`, `+<`/`+>`, prefix `-`, `abs` and `succ`/`pred` were each written
separately by the VM opcode, the reduction fold (which the routine, hyper, triangle and `R` forms
go through), the methods, the `abs()` builtin, and -- for `++`/`--` and `.=succ`/`.=pred` -- two
more copies.

The copies disagreed with rakudo and with each other. Three of them crashed the interpreter
outright: `$min div -1`, `$min mod -1` and `abs($min)` (with `$min` = i64::MIN) hit Rust overflow
panics in the one copy that lacked a guard the nqp path already had. `[div] 7, -2` used Euclidean
division (-3) where `7 div -2` floors (-4); `-$min` became a Num; `$min.abs` and
`9223372036854775807.succ` wrapped to negatives; `(2**70).succ` returned its invocant and
`abs(-2**70)` returned 0; `"²".succ` ignored the superscript digits `++` understood; `.=pred` on
`"a"` kept the string where `--` produced a Failure; and `5.5 +& 3` / `7.5 mod 2` answered 0 / 1
while their reductions answered 1 / 1.5.

ADR-0118 makes `src/builtins/arith/` the single home: `int_div`, `arith_mod` / `int_mod_i64`,
`int_bitop`, `int_shift_left` / `int_shift_right`, `int_negate` / `int_abs`, and
`value_succ` / `value_pred` (a number's successor is literally `arith_add($n, 1)`). Every form now
calls it. The check from ADR-0117 is generalized into `make check-prims`, which also fails on a
hand-written floored division outside that home and bans the deleted copies by name, and
`t/types/numeric/int-operator-forms-parity.t` pins 43 rakudo-measured values across the forms.
