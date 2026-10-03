# The numeric, transcendental and random `nqp::` ops

The first slice of the `nqp::` coverage campaign (#11488) adds the 30
numeric ops tracked by #11490. They used to die with
`Unsupported nqp:: op`:

- native int: `gcd_i`, `lcm_i`, `pow_i`
- native num: `mod_n`, `pow_n`, `ceil_n`, `floor_n`, `exp_n`, `log_n`,
  `sqrt_n`, `inf`, `neginf`, `nan`
- trigonometric: `sin_n`, `cos_n`, `tan_n`, `asin_n`, `acos_n`, `atan_n`,
  `atan2_n`, `sinh_n`, `cosh_n`, `tanh_n`
- big integer: `div_In`, `base_I`, `expmod_I`
- random numbers: `rand_n`, `rand_i`, `rand_I`, `srand`

Every answer matches MoarVM's, including the edge cases:

- `lcm_i`'s sign follows the signed gcd (`lcm_i(-4, 6)` is -12 and
  `lcm_i(4, -6)` is 12).
- `pow_i` wraps, and answers 0 for any negative exponent.
- `mod_n` is floored, and a zero divisor gives back the dividend.
- `div_In` is the exact quotient as a Num, so `div_In(10**400, 10**399)` is
  10 even though neither operand fits in a Num.

The wrapping native helpers live in `runtime::nqp_native` beside
`div_i`/`mod_i`. `nqp::base_I` and `Int.base` now render through one routine,
`builtins::int_to_base`, which replaces the two hand-written digit loops the
`Int.base` method used to have (one for `i64`, one for BigInt). `nqp::expmod_I`
is the routine behind `expmod`. `rand_*` and `srand` draw from the same
generator as Raku's `rand` and `srand`.

`docs/nqp-op-coverage.md` now reads 266 of 586 reachable ops implemented.
