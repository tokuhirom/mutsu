# `%%` and `%` by zero now return a soft Failure like Rakudo

`div` already routed a zero divisor through mutsu's lazy-`Failure` machinery,
but `%%` (`infix:<%%>`, "is divisible by") and `%` (modulo) still threw an
eager `X::Numeric::DivideByZero` straight out of the operator. That made the
common idiom `(^10).grep: 10 %% *` — "all divisors of 10" — crash on the `0`
element instead of skipping it, and any other `grep`/`so`/boolean use of a
`%%`/`%` expression that could hit a zero divisor died instead of answering
`False`.

Fixed in two places:

- `arith_mod` (`src/builtins/arith/mul_div_mod.rs`) built its `Int`/`BigInt`
  by-zero result with `Err(...)`, which propagates as an immediate `?`
  throw. Rakudo's `%` treats this exactly like `div`: a soft `Failure`,
  returned rather than thrown. Changed the four integer by-zero arms (and
  the `Num % Int(0)` combination, previously missing a zero guard entirely
  and silently returning `NaN`) to `Ok(RuntimeError::divide_by_zero_failure(...))`.
- `is_divisible` (`src/vm/vm_smartmatch_ops.rs`, backing `%%`) had its own
  eager `Err(...)` for a zero right-hand side, independent of `arith_mod`.
  Reworked it into `divisible_by_values`, returning the `Failure` value
  directly (mirroring `arith_mod`'s own zero handling now that it is fixed)
  instead of converting to a `Result<bool, _>` that could only carry an
  eager throw. `not_divisible_by_values` (`!%%`) negates through the
  `Failure`: boolifying it for `not` marks it handled and reads `False`, so
  `6 !%% 0` now answers a plain `True` `Bool`, matching Rakudo, instead of
  raising or propagating the `Failure` itself.

mutsu's existing sink/demand machinery (the same one that makes `div`'s
`Failure` throw on `.sink`, `say`, or any other value-demanding context)
required no changes — it already handles a `Failure` returned from any
producer uniformly. Pinned by `t/issue-7752-modulo-divisible-failure.t`,
verified against both mutsu and `raku`; the existing `t/divisible-rational.t`
and `t/divide-by-zero-message.t` continue to pass unchanged.

Closes #7752.
