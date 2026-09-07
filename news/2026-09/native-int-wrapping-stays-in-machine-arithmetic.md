# Native integer wrapping stays in machine arithmetic

Eleventh perf slice for `todo/deep/vendor-real-test-module.md`. The callgrind
profile of a vendored-`Test` assertion carried ~7k instructions of
`num_bigint` division -- `Rem`, `div_rem`, `div_rem_ref` -- under
`wrap_native_int`, the helper that gives a store into a native integer
variable its C-style modular wrap. It computed `((v % m) + m) % m` on heap
integers regardless of the value's size: three `BigInt` divisions and several
allocations, and its callers first boxed the `Int` they held into a `BigInt`
to call it (and `is_in_native_range` allocated the type's two `BigInt` bounds
per check). `Test.rakumod` counts its tests in `my int` lexicals, so
`$num_of_tests_run = $num_of_tests_run + 1` paid all of that once per
assertion; so does every `my int`/`uint8`/... store and every native-typed
parameter binding in any program.

Every native integer type is at most 64 bits wide, so both its modulus and
its bounds fit an `i128`, and so does every value an `Int` (`i64`) store ever
sees. `native_types` gains `wrap_native_int_i128`, `wrap_native_int_value` and
`native_int_bounds_i128`; `wrap_native_int` and `is_in_native_range` take the
machine path whenever the `BigInt` fits an `i128` (a `BigInt` that does not is
out of every native range and keeps the old path), and the three `Value`-level
callers (`maybe_wrap_native_int` on the read-modify-write store,
`wrap_native_int_by_constraint` on assignment, `wrap_native_int_for_binding`
on parameter binding) answer their `Int` arm without ever building a `BigInt`.
A unit test checks the machine paths against the `BigInt` definitions across
every width and both signednesses, including the `u64::MAX` / `i64::MIN`
edges.

Behaviour is unchanged (the smoke comparison against rakudo shows the same
pre-existing difference as before: mutsu wraps a native-typed *parameter*'s
argument where rakudo binds the unwrapped Int; that is not touched here).

## Measured

Callgrind, 300 `ok 1, "x"` in a loop under `MUTSU_REAL_TEST=1`, one-assertion
baseline subtracted, release build:

| | before | after |
| --- | --- | --- |
| per assertion | 250,138 Ir | 244,707 Ir |
| `num_bigint` division rows | ~7.4k | 0 |
| `exec_atomic_compound_var_op` (the `my int` RMW) | 8.0k | 4.6k |
| `store_named_scalar_rmw_result` | 6.5k | 3.2k |

**-2.2% per assertion**; -27.2% since the session opened at 335,929.
