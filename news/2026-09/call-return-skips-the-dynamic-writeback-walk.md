# A call return skips the dynamic-variable writeback walk, and `Mu` is accepted up front

Fourth perf slice for `todo/deep/vendor-real-test-module.md`. Three
independent fixes on what remained of the per-assertion profile under the
vendored `Test.rakumod`:

- **`pop_caller_env_with_writeback` walked the whole caller env on every
  return.** For each key it resolved the symbol to a string, stripped the
  sigil and asked `is_var_dynamic`, whose answer -- past the `$_`/`$/`/`$!`
  trio the loop skips first -- is the `is dynamic` flag table. With no such
  declaration in the program the walk is a guaranteed no-op, over a whole
  lexical scope per return once a method dispatch has flattened the env
  (~92 keys per `Test` call). It is skipped outright when that table is
  empty; the walk is unchanged when it is not.
- **`type_matches_value` reached `Mu` only at the end of its walk.** `Mu`
  is the root of the hierarchy, so it accepts every value, container, type
  object, junction and failure, and it is the constraint every assertion
  routine in `Test.rakumod` declares (`ok(Mu $cond, ...)`, `proclaim(Bool(Mu)
  $cond, ...)`). Accepted right after the tag fast path, gated -- like that
  fast path -- on no user `subset` shadowing the name.
- **Every typed-lexical probe built its env key with `format!`.**
  `var_type_constraint` and the bind/set writers formatted
  `__mutsu_type::<name>` and interned the fresh `String` per probe; once a
  program declares one typed lexical that ran on every `SetLocal`,
  `SetGlobal` and parameter bind in it. `type_meta_key_sym` memoizes the
  `name -> key` symbol per thread (symbols are append-only, so the mapping
  never changes) and the probes use the symbol-keyed env accessors.

## Measured

Callgrind, 300 `ok 1, "x"` under `MUTSU_REAL_TEST=1`, one-assertion
baseline subtracted:

| | per assertion |
| --- | --- |
| before (after the free-variable-read slice) | 378,452 Ir |
| after | 352,748 Ir |

-6.8% on this slice, -28.3% since the session started at 492,188.
`roast/S03-buf/write-int.t` under the real module: 9.6 s (median of three)
on the same box, 13.8 s at the start of the session.
