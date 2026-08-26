# `when` and `default` are terms, not just statements

`default { }` (and `when COND { }`) could not be used as a term nested inside an
expression. The doc-diff harness caught it on `Language/control.rakudoc:854`:

```raku
given 42 {
    "a".say;
    $_ == 42 and ( default { "b".say; 43 } );
    "c".say;
}
```

`raku` prints `a` then `b` — the `default` matches unconditionally, runs its
block, and then `succeed`s out of the enclosing `given`, so `"c".say` is never
reached. mutsu died at compile time with *"Unexpected block in infix position"*.

## Root cause

Raku's `(...)` is a **semilist of statements**, not merely a parenthesized
expression, so a control clause is a legal term inside it. mutsu already
modelled that for `if` / `unless` / `for` / `while` / `until` / `loop` /
`given` / `with` / `without` / `my` / `constant` / `class`: the keyword dispatch
in `src/parser/primary/ident/identifier_call.rs` recognises each of them in term
position and wraps the parsed statement in `Expr::DoStmt`. `when` and `default`
were simply missing from that list, so the parser fell through to the ordinary
bareword-call path and then choked on the following block.

## Fix

Two `when` / `default` arms were added to the same keyword dispatch, backed by
new `when_stmt_pub` / `default_stmt_pub` shims. `when` is guarded by the usual
`ws1` rule (so a user-defined `sub when` stays callable as `when()`), and
`default` additionally requires its `{` (a bare `default` is a very common
routine and named-argument name). No new execution machinery was needed: the
existing `OpCode::When` / `OpCode::Default` already signal a match by raising
`succeed` carrying the block's value, which unwinds the enclosing topicalizer
exactly as the statement spelling does.

The one value-level gap was the *non-matching* `when`. Raku says such a clause
evaluates to the falsy result of its own smartmatch test (`False`, or `Int 0`
for a type-object matcher), and `exec_when_op` already recorded which of the two
in `when_nonmatch_value` — but only the inline `map`/`grep`/`first` fast paths
read it. A new `OpCode::PushWhenNonmatch`, emitted after a `When`/`Default`
compiled in expression position, consumes that record so
`given 1 { say (when 42 { 43 }) }` prints `False` instead of `Nil`.

That last part also fixed half of the sibling ticket
`todo/tickets/control-do-when-expression-value.md`: `do when` in a real
topicalizer now yields the smartmatch result rather than `Any`.

Pinned by `t/control-constructs-in-expression-position.t`.
