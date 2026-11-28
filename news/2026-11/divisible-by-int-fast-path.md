# `%%` on two Ints skips the numeric coercion

`$a %% $b` with two plain `Int`s and a non-zero divisor now answers directly
from the shared floored modulus, `int_mod_i64` (ADR-0118). Before, it built
coerced copies of both operands (`coerce_numeric`) and dispatched through the
general `arith_mod`, which came to about 600 instructions per `%%`. Every
other shape keeps the general path: a zero divisor (a soft `Failure`), a Rat
or Num operand, a big Int, or a user `infix:<%%>`.

On `benchmarks/array-ops.raku` (callgrind, JIT on, second run) this takes the
program from 357,012,328 to 323,801,224 instructions (-9.3%). That is about
2,430 instructions per callback invocation, against about 5,480 when #19323
started and about 3,200 for rakupp v5.9.0, and it meets #19323's goal.
