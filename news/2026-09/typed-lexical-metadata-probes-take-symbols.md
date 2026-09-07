# Typed-lexical metadata probes take symbols

Thirteenth perf slice for `todo/deep/vendor-real-test-module.md`. Once a
program declares one typed lexical anywhere (`Test.rakumod` counts its tests
in `my int` variables), every store consults the env-scoped
`__mutsu_type::<name>` constraint lane: `SetLocal` probes it up to seven
times per store, `SetGlobal` three times, the RMW and binder paths again.
`var_type_constraint(name: &str)` answered each probe by interning `name`
(a string hash into the thread-local symbol cache) to look up the memoized
meta-key symbol, then copied the constraint out of the `Arc<String>` guard
through `Display`. A vendored-`Test` assertion paid ~7.2k instructions in it,
plus ~3.4k in `set_var_type_constraint_impl`, which every `my` declaration
runs to clear a possible stale constraint: two `format!`s and two interning
`env.remove(&str)`s once the typed-lexical latch is set.

The callers that already hold the name as a symbol now pass it:
`var_type_constraint_sym` / `var_type_constraint_for` (the `SetLocal` slot
symbol from `code.locals_sym`, the `SetGlobal` constant interned once per
store) reach the `Symbol -> Symbol` memo directly, and the constraint is
copied as bytes. The hash-key twin `__mutsu_hash_key_type::<name>` gets the
same per-symbol memo (`hash_key_meta_key_for_sym`), so the declaration-time
clear and `var_hash_key_constraint` stop formatting and interning keys.

`resolve_constraint_alias`, which the coercion path asks twice per
`Bool(Mu)` parameter and once per typed store, returns `Cow::Borrowed` for
the common non-alias case instead of allocating a copy of the constraint,
probes the env through `Symbol::lookup` (a spelling nobody interned cannot be
an env key, and asking about it must not grow the symbol table), and
`try_coerce_value_for_constraint` skips the subset walk when the registry has
no subsets.

## Measured

Callgrind, 300 `ok 1, "x"` in a loop under `MUTSU_REAL_TEST=1`, one-assertion
baseline subtracted, release build:

| | before | after |
| --- | --- | --- |
| per assertion | 237,744 Ir | 234,580 Ir |
| `var_type_constraint` (all probes, `_sym`/`_for` after) | 7.2k | 4.2k |
| `set_var_type_constraint_impl` | 3.4k | 1.5k |
| `try_coerce_value_for_constraint` | 4.3k | 3.0k |
| `Symbol::intern` | 15.1k | 12.7k |
| `format_inner` | 5.0k | 3.6k |

**-1.3% per assertion**; -30.2% since the session opened at 335,929.
