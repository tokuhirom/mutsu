# The binder stops re-deriving the implicit `Any` check and probing for absent meta keys

Tenth perf slice for `todo/deep/vendor-real-test-module.md`. With the call
machinery, the method-dispatch flatten, the substring searchers and the
`callframe().code` object out of the way, `bind_function_args_values` was the
largest single row of a vendored-`Test` assertion at ~39k instructions for its
two calls (`ok(Mu $cond, $desc = '')` and
`proclaim(Bool(Mu) $cond, $desc is copy, $unescaped-prefix = '')`). Four
things it did per parameter turned out to be answerable up front:

- **The implicit `Any` check walked the whole type gauntlet.** An untyped `$`
  parameter is implicitly `Any` (to reject a Junction, which is a `Mu` but not
  an `Any`), so the binder asks `type_matches_value("Any", value)` for every
  such argument. `type_matches_value` fast-accepts an exact tag match and `Mu`,
  but `Any` had to fall through the MRO dispatch, the role-key resolution and
  the parametric/coercion parsers (~3.3k instructions) to reach the answer a
  concrete `Int`/`Str`/`Num`/`Bool`/`Rat` value has by construction. Those
  native scalar tags now accept `Any` directly, gated on the subset registry
  like the existing fast accepts; a type object, an instance of a class that
  `is Mu`, a junction or a container keeps the full checker.
- **`has_type_capture_binding` built and interned a marker key per probe.**
  `resolved_type_capture_name` asks it for every typed constraint and again
  for the constraint's base (`Bool(Mu)`, `Mu`), each a `format!` +
  interning env lookup. A process-global monotonic latch set by
  `bind_type_capture` -- the only writer of the marker -- answers `false`
  until a `::T` capture has ever been bound (the same pattern as
  `ENV_TYPE_CONSTRAINT_SEEN`, and process-global for the same adoption
  reasons).
- **An `is copy` parameter removed two sigilless meta keys that cannot
  exist.** Both `__mutsu_sigilless_alias::` and `__mutsu_sigilless_readonly::`
  are written only by sigilless-parameter bindings; every such arm now latches
  `sigilless_alias_seen`, and the `is copy` cleanup skips its two `format!` +
  interning removes until the latch is set.
- **`set_env_plain_lexical` formatted the `^name` placeholder twin per
  store.** Once a program declares one placeholder parameter anywhere, every
  by-name local store probed `env` for `^<name>` with a fresh `format!` +
  intern. The key is memoized per name symbol (`placeholder_key_sym`, the
  `type_meta_key_sym` pattern) and probed by symbol.

## Measured

Callgrind, 300 `ok 1, "x"` in a loop under `MUTSU_REAL_TEST=1`, one-assertion
baseline subtracted, release build:

| | before | after |
| --- | --- | --- |
| per assertion | 261,339 Ir | 250,138 Ir |
| `bind_function_args_values_inner` | 39.4k | 28.9k |
| `type_matches_value` | 7.0k | 0.9k |
| `resolved_type_capture_name` | 4.7k | 1.1k |

**-4.3% per assertion**; -25.5% since the session opened at 335,929
(`news/2026-09/nqp-ops-and-str-gist-skip-the-call-machinery.md`,
`news/2026-09/env-pure-method-dispatch-skips-the-scoped-env-flatten.md`,
`news/2026-09/free-variable-reads-drop-the-substring-searchers.md`,
`news/2026-09/callframe-code-object-is-built-on-demand.md`).
