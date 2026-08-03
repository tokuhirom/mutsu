# `use strict` no longer rejects two forms of declared binding

`use strict` decides "is this variable declared?" at the `SetGlobal` opcode by
asking whether `env` holds the name. Two declarations do not put their name in
`env` at all, so writing them was reported as a write to an undeclared variable:

- **A multi-parameter `for` head.** `for %h.kv -> $k, $v { … }` desugars to two
  plain assignments at the top of the loop body (`build_for_bind_stmts`); a
  single-parameter head is bound by the `ForLoop` opcode itself, which is why
  only the multi-parameter form failed. Every sigil was affected —
  `for @pairs -> @x, @y` died the same way.
- **A module's own file-scope `my`, written from a routine two frames deep.**
  Since `news/2026-08/module-file-scope-lexical-is-not-the-callers.md` such a
  scalar lives in the compunit-lexical store rather than under an `env` key, so
  the `env`-only test cannot see it. The write itself already went to the right
  place; only the declaration check disagreed.

Both are now exempt: the compiler records a multi-parameter loop head's names in
`CompiledCode::param_bind_names` (compile-time data, consulted only on the cold
strict path, so no extra opcode runs per iteration), and the check consults the
compunit-lexical store through `has_unit_scope_lexical` before deciding a name
is undeclared. An undeclared write is still rejected, and loop parameters are
still block-scoped.

Found under `MUTSU_REAL_TEST=1`, where both bugs fire at once:
`roast/S02-names/strict.t` and `roast/S02-lexical-conventions/comments.t` aborted
mid-file with `Variable '$time_after' is not declared` raised *inside*
`Test.rakumod`'s own `_diag` — `$time_after` being the module's file-scope `my
int`, reached from `throws-like` → `subtest` → `_diag`, while the test file's own
`use strict` block was in effect. `strict.t` now passes all 7 tests under the
real module; `comments.t` runs its full plan of 51 instead of aborting at 3.

Pin: `t/strict-declared-bind-forms.t` (with `t/lib/StrictNestedLexical.rakumod`).
