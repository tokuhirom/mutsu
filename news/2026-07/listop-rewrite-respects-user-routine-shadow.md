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

The parser now records a resolved user-routine call in the AST when an imported
or already-declared routine shadows one of these listops. The compiler also
tracks hoisted listop declarations in its own lexical scope state, so a call
before the textual `my sub push` declaration is suppressed as well. The call
then falls through to the generic call path, which resolves the user routine.
Both halves are load-bearing:

- the importing script sees `push`/`pop` as **imported**
  while the parser's lexical scope is live, and
- declared routines are seeded during compiler sub hoisting — P5push's no-arg
  `multi sub pop()` calls `pop(@*ARGS)`, i.e. its own other candidate.

Without the second half, the last subtest (a bare `pop` on an exhausted
`@*ARGS`) still surfaced the builtin's `Cannot pop from an empty Array`.

The ledger listed the two remaining failure shapes as needing separate
diagnoses; they turned out to be one root cause.

Pins: `t/listop-shadow-imported.t` (fixture `t/lib/ListopShadow.rakumod`) for the
imported half, `t/listop-shadow-declared.t` for the declared half, and
`t/listop-shadow-block-scoped.t` for block-local declaration hoisting, nested
visibility, import scoping, and restoration of the builtin outside the block.
All are verified identical under `raku`.
