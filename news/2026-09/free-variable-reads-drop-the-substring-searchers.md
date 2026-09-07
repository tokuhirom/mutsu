# Free-variable reads stop building substring searchers

Eighth perf slice for `todo/deep/vendor-real-test-module.md`. After the two
previous slices the callgrind profile of a vendored-`Test` assertion showed
`<&str as Pattern>::is_contained_in` at ~13.7k instructions per assertion,
~6k of it under `package_scope_lexical` alone. A routine body's free-variable
read (`$num_of_tests_run`, `$indents`, `$output`, ... -- a `Test.rakumod`
assertion makes about ten) resolves through the package-lexical, unit-lexical
and `our`-mirror stores in turn, and each of those asked
`str::contains("::")` / `contains("::&")` of the variable name and the running
package. `str::contains(&str)` builds a two-way searcher per call; on names
this short the setup is the whole cost.

Two changes, both general:

- **`package_scope_lexical` gates on an empty store first.** It answers a
  bare `package P { my $x; ... }` block's lexical from inside `P`'s subs;
  most programs never run such a block, so `package_lexicals` is empty and
  nothing below the gate can resolve. The package probe and both name scans
  now run only when the store has something to find.
- **The fixed ASCII markers are byte scans.** `runtime::utils::str_scan`
  provides `has_double_colon`, `has_routine_scope_marker` (`::&`) and
  `has_anon_marker` (`__ANON`) as plain `windows().any()` scans, and the
  resolvers on the read/write hot paths use them (`vm_env_helpers`,
  `vm_our_package_vars`, `vm_var_get_ops`, `exec_set_local_op`, the
  container-identity store, the routine-package entry). A unit test pins
  them against `str::contains` on the shapes the resolvers see.

## Measured

Callgrind, 300 `ok 1, "x"` in a loop under `MUTSU_REAL_TEST=1`, one-assertion
baseline subtracted, release build:

| | before | after |
| --- | --- | --- |
| per assertion | 285,180 Ir | 276,814 Ir |
| `<&str as Pattern>::is_contained_in` | 13.7k | 0 |
| `unit_lexical_slot` | 12.4k | 7.5k |
| `get_env_with_main_alias` | 11.6k | 8.2k |

**-2.9% per assertion**; -17.6% since the session opened at 335,929
(`news/2026-09/nqp-ops-and-str-gist-skip-the-call-machinery.md`,
`news/2026-09/env-pure-method-dispatch-skips-the-scoped-env-flatten.md`).
