# A leading bare block is now recognized as a custom infix operator's left operand

Raku's statement grammar is ambiguous at a leading `{`: it can start a bare
block (a standalone statement) or be the left operand of a following custom
infix operator whose first parameter is a `&closure`. Rakudo resolves this
with lookahead past the block's `}`: if the next token is a token that cannot
itself start a new statement — a declared infix operator — the block is a
term, not a complete statement.

mutsu's statement parser never performed this lookahead: `simple::block_stmt`
committed a leading `{ ... }` to a complete `Stmt::Block` unconditionally, so
a following custom infix operator started a bogus new statement instead:

```raku
sub infix:<xxx>(&closure, Int $num) is export {
    $num times &closure;
}
{ $value--; } xxx 25;
```

mutsu used to run `{ $value--; }` as a bare block, then fail on `xxx 25;`
with `Undeclared routine: zork used`. Fixed by adding a lookahead in
`block_stmt`: once the block itself is parsed, if the next token on the same
line is a *declared* custom infix operator (word or symbol form, checked via
the same `is_user_defined_infix`/`match_user_declared_infix_symbol_op`
registry the mid-expression infix parsers already consult), `block_stmt`
backs off instead of committing, letting `simple::expr_stmt` re-parse
`{ ... }` as a term (`block_or_hash_expr`, which already builds the right
`Expr::AnonSub`) and continue into the infix expression.

The check is deliberately gated on the operator being genuinely *declared* —
not merely "looks like an unreserved word" — so the far more common case of a
bare block followed on the same line by an ordinary call (`{ $x++; }
say $x;`) is unaffected and still runs as two statements.

Verified end-to-end against the real vendored `PSpec` module: `{ $value--; }
xxx 25;` now runs correctly via `use PSpec`, closing out the second of the two
bugs found in `todo/tickets/dist-test-suite-failures-batch.md`'s `PSpec`
investigation (the first, closure-argument writeback, was fixed earlier in
`news/2026-08/user-infix-closure-arg-writeback.md`).

New regression test: `t/bare-block-infix-operand.t`.
