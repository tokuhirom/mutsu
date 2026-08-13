# `is test-assertion` (and any user `trait_mod:<is>`) is now genuinely introspectable

Under the vendored upstream `Test` module (`MUTSU_REAL_TEST=1`),
`t/test-assertion-line-number.t`-style failures used to report the
*assertion helper's own* line instead of its caller's:

```
$ cat inner.raku
use Test;
plan 1;
sub foo-ok() is test-assertion { flunk "foo-ok" }
foo-ok;

$ raku inner.raku                |  $ MUTSU_REAL_TEST=1 mutsu inner.raku
# Failed test 'foo-ok'           |  # Failed test 'foo-ok'
# at inner.raku line 4           |  # at inner.raku line 3
```

`Test.rakumod` implements the "blame the caller" trick itself, entirely in
user-land Raku: `trait_mod:<is>(Routine:D $r, :$test-assertion!)` mixes an
`is-test-assertion` marker role onto the routine, and its backtrace walk
skips any `callframe` whose `code` answers `True` to
`nqp::can(code, 'is-test-assertion')`. Three separate gaps kept that from
working:

1. **The user `trait_mod:<is>` was never reached.** mutsu's parser consumed
   `is test-assertion` itself as a builtin flag and never queued it as a
   custom trait, so `Test.rakumod`'s handler never ran. The same ordering gap
   silently swallowed any `Err` from *inside* a handler that did match (a
   `die` was discarded instead of propagating).
2. **`.^can` / `nqp::can` couldn't see a role mixed onto a value via
   `but`/`does`.** They walked the class registry MRO keyed off the
   receiver's underlying class, never consulting a `Mixin`'s own roles — so
   `.can` found a mixed-in method but `.^can`/`nqp::can` did not.
3. **A role composed onto a named routine did not survive the routine's next
   rebuild.** A `sub`'s code object is reconstructed fresh from the registry
   at every call (for `callframe(N).code`) and at every bare `&name` mention
   from a different scope, rather than kept as one persistent object — so
   `.^mixin(Role)`/`$r does Role` on one instance vanished on the very next
   lookup. `.^mixin` also keyed its mixin map by the bare role name instead
   of the `__mutsu_role__<name>` marker every other role-aware consumer
   expects, so even the instance it *did* return was invisible to
   `.^can`/`nqp::can`.

A fourth, smaller gap: a `role` expression in term/argument position (e.g.
`.^mixin(role is-x { ... })`, the exact shape `Test.rakumod` uses) required
an uppercase or `_`-prefixed name, rejecting a lowercase/kebab-cased one with
"Two terms in a row" even though the same name parses fine at statement
position.

All four are fixed:

- The parser now also queues a natively-recognised trait name (currently
  just `test-assertion`) as a custom trait, so a matching user
  `trait_mod:<is>` handler runs in addition to the builtin meaning. The
  "no candidate matched" verdict still falls back to the builtin trait
  silently, but a real error from inside a handler that *did* match now
  propagates instead of being swallowed.
- `.^mixin(Role)` composes an actual role argument through the same path
  `but`/`does` use, producing a structurally identical `__mutsu_role__<name>`
  mixin instead of a bare-name-keyed one.
- `collect_can_methods` (backing `.^can` and `nqp::can`) gained a `Mixin`
  arm that also collects methods from the wrapped value's mixed-in roles,
  bringing it in line with `.can`.
- A process-wide, monotonic record (mirroring the existing
  `TEST_ASSERTION_DECLS` pattern) tracks which named routines have ever been
  composed with a role. Every site that rebuilds a routine's code object
  (the compiled call path, the interpreted call path, and `&name`
  resolution from a non-declaring scope) now re-applies those roles, so
  `callframe(N).code` and `&name` see them consistently — at the cost of one
  relaxed atomic load per call when no routine anywhere has ever been mixed
  with a role.
- `callframe`/backtrace introspection call sites that pattern-matched
  `ValueView::Sub` directly were taught to look through a `Mixin` wrapper.
- The inline `role` expression parser now accepts any valid identifier as a
  name (not just uppercase/`_`-led), matching statement position.

With all four fixed, `is test-assertion` under `MUTSU_REAL_TEST=1` now
reports the caller's line exactly like `raku` does.

New/updated tests: `t/role-expr-hyphenated-name.t`,
`t/classhow-can-mixin-role.t`, `t/sub-trait-mod-user-dispatch.t`,
`t/routine-mixin-survives-rebuild.t`, and a new subtest in
`t/vendored-real-test-module.t` exercising the exact scenario end-to-end.
