# `use` inside a block now scopes its `env` aliases, not just the registry

An import is lexical to the block that asked for it. `push_import_scope` /
`pop_import_scope` (`runtime/runtime_module.rs`) already snapshotted and
restored the function/class registries around a `use`-containing block
(`news/2026-08/do-block-import-scope-and-imported-protos.md`), but
`import_module` also writes the imported symbol straight into `env` — a bare
`&name` for a code-sigil export, `$NAME`/`@NAME`/`%NAME` for others — and
that half was never rolled back. So a module-exported `&`-sigil sub or
`$`-sigil constant stayed callable/readable by its bare name after the block
that imported it exited:

```raku
{
    use SomeModule;   # exports &thing, $CONST
}
say &thing.defined;   # was True after the block; raku dies "Undeclared routine"
say $CONST;           # was still readable; raku dies "not declared"
```

## The fix

`ImportScopeSnapshot` gained an `env` field — the set of `env` keys present
before the `use` — captured in `push_import_scope` alongside the existing
registry key sets. `pop_import_scope` now diffs `env` against it and drops
every key added since the push, with the same keep-rule the registry side
already used: a module's own package-qualified entry (`Foo::name`, written
when the module's mainline first ran `our &name = ...`) persists, because a
sibling block's later `use Foo` re-imports by reading that qualified value
(see the `vars` loop in `import_module`). Only the *importing package's*
alias (bare `name`, or `GLOBAL::name` — a sigil may prefix the qualifier, so
it is stripped before the check) is what gets removed.

A side effect worth naming: a block containing a `use` statement was never
running through the ordinary `BlockScope` env-restore machinery that a
`use`-free block gets (the compiler emits `PushImportScope`/
`PopImportScope` instead, specifically so registry state can be scoped) — so
*any* new bare `env` entry created inside such a block, not just an
import's, used to leak too (`{ my $x = 5; use Foo; } say $x` answered `5`
outside the block). The same diff-and-drop now closes that for free, since a
plain `my` declaration also lands as a bare (non-qualified) new `env` key.

Pin: `t/use-in-block-env-scope.t`, with fixture `t/lib/ImportEnvScope.rakumod`
(exports `&greet` and `$GREETING` as variables, matching the exact mechanism
that was leaking — a plain `sub` export does not touch `env` this way).
Covers both the clean scope-exit case and a sibling block re-importing the
same module after the first block's scope closed.

## What is still open

The ticket's own minimal repro (`{ use Test; } say &ok.defined;`) still
answers `True`, unchanged — that one was never about the import scope.
mutsu's **native TAP provider** intercepts `Test`'s exports independently of
any lexical scoping, gated on `loaded_modules`, which is deliberately never
rolled back (it goes away only with the native provider itself, step 3 of
`todo/tickets/vendor-real-test-module.md`). Verified against a non-`Test`
module instead (`ImportEnvScope` above), which is unaffected by that gate.

`roast/S32-list/skip.t` under `MUTSU_REAL_TEST=1` (the real vendored
`Test.rakumod`, not the default `make roast` configuration) was the original
motivating case for the registry-side fix, and this env-side half was
expected to let it reach its `skip()` assertions. It still does not: fixing
the leak surfaces an *unrelated*, pre-existing stack overflow in how a
captured `&name` reference to a popped proto/multi import re-dispatches by
name — filed as
`todo/tickets/routine-value-self-recursion-after-import-scope-pop.md`.
Confirmed pre-existing by reproducing the same crash on the unmodified
branch tip before this fix. `roast/S32-list/skip.t` under the default
native provider (`make roast`'s actual configuration) is unaffected and
still passes 55/55.
