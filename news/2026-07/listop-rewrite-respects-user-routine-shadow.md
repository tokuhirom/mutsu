# The listop → method rewrite no longer hides a user `push`/`pop`

`TODO_dist` ticket T-054 (`P5push`) is closed at 14/14. The distribution exports
Perl 5 semantics for `push` and `pop` — `push` returns the new element count, and
`pop` on an empty array returns `Nil` instead of a Failure — and mutsu returned
the builtin's answers instead.

## Root cause

The compiler rewrites the container listops into method calls at compile time so
that array mutation lands in the caller's container: `pop(@a)` → `@a.pop()`,
`push(@a, v)` → `@a.push(v)`, and the same for `shift`, `unshift`, `append`,
`prepend`, `splice` (`compiler/expr_call.rs`). That rewrite is unconditional, so
it baked the builtin in before dispatch ever ran — the runtime's
builtin-vs-user-routine preference (fixed earlier for `String::Rotate`) could not
help, because by then there was no call left to dispatch.

The four rewrite branches are now guarded by
`listop_shadowed_by_user_routine`, which suppresses the rewrite when a user
routine of that name is visible; the call then falls through to the generic call
path, which resolves it. Both halves of the predicate are load-bearing:

- the importing script sees `push`/`pop` as **imported**
  (`parser::is_imported_function`), and
- the module's own body sees them as **declared**
  (`parser::is_user_declared_sub_pub`, newly exposed to the compiler) — P5push's
  no-arg `multi sub pop()` calls `pop(@*ARGS)`, i.e. its own other candidate.

Without the second half, the last subtest (a bare `pop` on an exhausted
`@*ARGS`) still surfaced the builtin's `Cannot pop from an empty Array`.

The ledger listed the two remaining failure shapes as needing separate
diagnoses; they turned out to be one root cause.

## Scope / known limitation

The predicate reads the parser's lexical-scope stack, which at compile time only
still holds scopes that were never popped — in practice the unit scope. So a
listop shadow declared or imported **inside a block** (`{ my sub push {...}; push
@x, 1 }`, or a block-scoped `use`) is still not honoured; mutsu calls the builtin
there. That is pre-existing behaviour, unchanged by this fix, and is recorded in
`todo/tickets/block-scoped-listop-shadow.md`.

Pins: `t/listop-shadow-imported.t` (fixture `t/lib/ListopShadow.rakumod`) for the
imported half, `t/listop-shadow-declared.t` for the declared half. Both verified
identical under `raku`. `roast/S32-array/{pop,push,shift,splice,unshift,perl}.t`
and `roast/S02-types/array.t` stay green, confirming the unshadowed builtin path
is untouched.
