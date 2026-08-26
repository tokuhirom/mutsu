# A statement-modifier `when` is not a `when` clause, and no longer swallows `proceed`

From the doc-diff harness on `Language/control.rakudoc:973`:

```raku
given 42 {
    when * > 41 {
        { "A".say; proceed } when * > 41;
        "B".say;
    }
}
```

`raku` prints `A` only: the `proceed` unwinds the enclosing `when * > 41 { ... }`
clause entirely, so `"B".say` never runs. mutsu printed `A` then `B`.

## Root cause

mutsu desugars the postfix `STMT when COND` modifier into a synthetic
`Stmt::Given { is_statement_modifier: true, body: [Stmt::When { ... }] }`. The
wrapper exists for a good reason — a matching `when` signals itself by raising
`succeed`, and without a topicalizer to catch it the modifier would abandon the
whole enclosing block, which is wrong (`given 42 { "A".say when * > 41; "B".say }`
must print both). But the *inner* `Stmt::When` was an ordinary `when` clause as
far as the VM was concerned, and `exec_when_op` swallows a `proceed` raised in a
`when` body (`Err(e) if e.is_proceed() => did_proceed = true`). So the modifier
consumed the `proceed` that was meant for the real, outer clause.

Measured against `raku` v2026.06, the modifier is not a `when` clause at all:

- `given 42 { { "A".say; proceed } when * > 41; "B".say }` — with no enclosing
  clause — dies with *"proceed without when clause"*, proving the modifier
  establishes no `proceed` target of its own.
- `given 42 { "A".say when * > 41; "B".say }` prints both, proving a match does
  not abandon the enclosing block.

That is exactly what Rakudo's lowering of `statement_mod_cond:sym<when>` to a
plain conditional (`COND.ACCEPTS($_) ?? STMT !! Nil`) predicts.

## Fix

`Stmt::When` grew an `is_statement_modifier: bool` (mirroring `Stmt::Given` and
`Stmt::If`, which already carried one), threaded through `OpCode::When` as
`statement_modifier`. When it is set, `exec_when_op` records the match and
re-raises the `proceed` instead of consuming it, so the signal keeps unwinding to
the nearest real `when` clause. The synthetic `given` wrapper is untouched, so
the non-abandoning behaviour of a matching modifier is preserved —
`exec_given_op` only ever caught `succeed`, never `proceed`.

Pinned by `t/control-constructs-in-expression-position.t`, which covers both
directions: the `proceed` now escapes the modifier, and a plain matching
modifier still runs the rest of its block (statement form and loop body alike).
