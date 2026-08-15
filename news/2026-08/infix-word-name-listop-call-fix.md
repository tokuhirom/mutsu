# A sub named after an infix word can now be called as a listop

`is_infix_word_op` (`src/parser/primary/ident/predicates.rs`) lists every
name that is also an infix operator word — `Z X R x xx eq ne lt gt le ge cmp
coll unicmp leg and or not div mod gcd lcm but does min max ff fff before
after andthen orelse notandthen` — and the identifier parser refused to treat
any of them as a paren-less listop call, even when a `sub` of that exact name
was declared and in scope:

```raku
sub before(&cb) { say "called" }
before { 2 };   # was: calls before() with no args, leaves `{ 2 }` dangling
                 # now: calls before({ 2 }), matching raku
```

With parentheses (`before(5)`) it always worked — the gap was specific to the
paren-less listop form.

## Fix

The check lived at exactly the right spot already: `identifier_call.rs`'s
`identifier_or_call` gates entry into the listop-call branch on
`!is_infix_word_op(&name)`, and the very next lines already compute
`is_user_declared_sub(&name)` (the same lexical-scope table every ordinary
paren-less user-sub call already consults) — but only *after* the
`is_infix_word_op` gate had already thrown the name out. The fix relaxes the
gate: `!is_infix_word_op(&name) || is_user_declared_sub(&name)`.

This is safe because term position and operator position are structurally
separate code paths, not a shared flag — `identifier_or_call` is only ever
invoked when a *fresh term* is being parsed; a following infix word after an
already-complete left operand (`{1} before {2}`, `@a min @b`, `1 and 2`) is
recognized by wholly separate, unconditional matchers in
`expr/operators.rs`/`expr/precedence/*.rs` that this change never touches.
So the ambiguous cases the original investigation flagged as needing care
stay exactly as before:

- `x`/`xx` as the replication/repeat infix — recognized by
  `parse_replication_op`, never consulted from term position.
- `@a min @b` as the infix, even with a same-named declared `sub min` — `min`/
  `max` were already reachable as listops via the separate, earlier
  `is_listop` check anyway, so this fix is a no-op for them in term position;
  their infix reading (`parse_or_or_op`) is a different function entirely.
- Word-logicals (`and`/`or`/`andthen`/...) after a complete term stay the
  loose infix, never swallowed into a listop's own argument list — verified
  with a declared `sub and(&cb) {...}` still leaving `(1 and 2)` as the
  infix.

## Verification

The ticket's repro (`before`/`after`) now matches `raku`, as does a sub
declared with the more surprising name `eq`. New
`t/infix-word-name-listop-call.t`; the existing ambiguity-guard tests
(`t/block-brace-ends-statement.t`'s same-line-infix assertion,
`t/listop-word-logical.t`, `t/listop-undeclared-bareword-arg.t`,
`t/min-max-over-seq.t`, `t/builtin-shadow-dispatch.t`) all still pass, plus
the full local `t/` suite (29,577 tests) and the Rust unit test suite (826
tests).
