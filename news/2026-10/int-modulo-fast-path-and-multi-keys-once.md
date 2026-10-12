# `Int % Int` gets a fast path, and a multi call keys its arguments once

Two per-call costs on multi-dispatch loops, both found while profiling the
subset/where section of `bench-multi-dispatch` (#10111):

- The modulo opcode was the one integer operator with no `Int`/`Int` fast path.
  Every `$a % $b` went through the junction wrapper and the numeric bridge, about
  1.5k instructions for one floored modulus. It now calls the shared
  `int_mod_i64` directly, unless the divisor is zero (the general path builds
  the divide-by-zero `Failure`) or a user `infix:<%>` exists.
- A multi call built its argument type keys (`multi_arg_type_keys`) twice: once
  for the resolve cache, and again for the bare-multi plan key. The keys are
  now handed down. `Int`, `Num`, `Str` and `Bool` arguments also key through the
  process-wide well-known type ids instead of an intern per argument.

On the section (30,000 calls of `size(($i * 37) % 20_000)`), callgrind went
from 623.7M to 608.7M instructions for the keys, and then to 565.4M for the
modulus.
