# The named call path stops re-resolving symbols and re-walking type checks per call

Second perf slice for `todo/deep/vendor-real-test-module.md`. After the
literal-default fix (`news/2026-09/literal-param-defaults-bind-directly.md`)
the callgrind profile of an `ok 1, "x"` under the vendored `Test.rakumod`
was dominated by symbol traffic on `call_compiled_function_named_inner` and
by `type_matches_value` walks that answered a question the caller could
have answered by a tag compare. Five independent fixes, all on that path:

- **The return merge compared every key against four strings.** The
  scoped-overlay merge loop asked `*k == "_" || *k == "@_" || *k == "%_"`
  and `*k == "__mutsu_callable_id"` of every key in the callee env, and a
  `Symbol == &str` compare resolves the symbol through the thread-local
  string cache first. On a callee env that a method dispatch in the body
  has flattened to the whole lexical scope (~106 keys for a `Test`
  assertion) that was ~400 thread-local accesses per call. The four are now
  integer compares against pre-interned `wk` symbols (`@_` and `<anon>`
  joined the well-known table).
- **The frame seeded its locals through `env.get(&String)`**, interning each
  local's name on every call. It reads through the chunk's pre-interned
  `locals_sym` twin instead.
- **`fn_package` / `fn_name` were interned three and two times per call**
  (the `callframe().code` Sub, the routine frame, the state scope). Once
  each now; the `$!` reset and the fresh-topic seed use `wk` symbols too.
- **`type_matches_value` sent an `int` constraint through the full
  checker.** The tag fast-accept only knew the boxed names, so
  `my int $num_of_tests_run = $num_of_tests_run + 1` -- the vendored
  module counts its tests in `my int` lexicals -- paid the gauntlet twice
  per assertion. `int`/`num`/`str` are accepted alongside `Int`/`Num`/`Str`,
  exactly as the general checker's alias rule already answered them.
- **The native method fast path probed `Real` and `Numeric` before asking
  whether the class had an interpreter-side handler.** `IO::Handle.say`
  reached `None` either way, but only after two full `type_matches_value`
  walks on an instance that cannot be numeric. The `is_native_method`
  decision moved ahead of the numeric-bridge probes; every branch involved
  declines the native path, so the outcome is unchanged.
- **`function_key_base_name` built a two-way searcher per key** (`rfind("::")`)
  on the per-dispatch candidate walk. A byte scan now; pinned by a unit
  test against the shapes the registry produces.

## Measured

Callgrind, 300 `ok 1, "x"` under `MUTSU_REAL_TEST=1`, one-assertion
baseline subtracted:

| | per assertion |
| --- | --- |
| before (after the literal-default fix) | 454,016 Ir |
| after | 397,305 Ir |

-12.5% on this slice, -19.3% since the session started at 492,188.
`roast/S03-buf/write-int.t` under the real module: 11.6 s -> 10.3 s on the
same box (13.8 s at the start of the session).
