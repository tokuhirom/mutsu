# An `EVAL`'d snippet's nested parse gets a type preseed but no constant preseed

Split out of PR #6840 (`news/2026-08/when-undeclared-bareword-gobbles-block.md`), which closed
`todo/deep/when-undeclared-bareword-gobbles-block-needs-cross-file-type-index.md` by teaching the
parse-time index about imported constants. Its author flagged this one narrowing as knowingly left
open; filing it so it is tracked as an open finding rather than only as a note in a merged PR.

## The gap

The gobbled-block diagnostic (`when SomeUndeclaredType { ... }` → parse-time error) decides whether
a bareword matcher is a *known* name from the parse-time index. For an `EVAL`'d snippet the nested
parse gets a **type** preseed via `collect_eval_user_type_names`, but no **constant** preseed, and
`reset_user_subs` clears the incompleteness flag that would otherwise stand the diagnostic down.

So this can still misfire:

```raku
constant Foo = 1;
EVAL 'given 1 { when Foo { say "matched" } }';
```

## Status when filed

Not observed anywhere real: it did not appear in PR #6840's instrumented corpus sweep (`modules/`,
`vendor/zef/`, `t/` 3336 files, `roast/` 1464 files, every fire enumerated via an
`MUTSU_DEBUG_GOBBLE`-gated print at the firing site), nor in roast or `t/`. The same exposure
already existed before #6840 for locally-declared `constant` / `my \x`, so this is a pre-existing
narrowing the fix did not widen — it just became the last one standing.

## Verification note for whoever picks this up

**Do not grep for the diagnostic's message text to check whether it fires.** PR #6840 established
that the gobble `PError` can be swallowed by backtracking and resurface as an unrelated message
("Unexpected block in infix position" for a `when` inside an expression-position `do given`), so a
message grep reports a clean sweep while the check is in fact firing. Gate a print inside
`gobbled_block_error` behind an env var instead, and remove it before committing.

## Priority

Low. It needs a constructed repro to hit, and `EVAL` of code referencing an outer `constant` from a
`when` matcher is a narrow shape. Worth doing when someone is next in this code, and cheap if the
constant preseed can reuse `collect_eval_user_type_names`' plumbing.
