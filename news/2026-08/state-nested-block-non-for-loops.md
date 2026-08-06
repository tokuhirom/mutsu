# `state` in a nested bare block restarts per iteration in non-`for` loops

The last residual of the state-scalar ticket family (after
`state-scalar-cell-storage.md`, `state-block-loop-body-accumulates.md`, and
`typed-state-scalars-accumulate.md`). Raku re-clones a bare block nested in
a loop body on every iteration, so its `state` restarts:

```raku
my $c = 0;
while $c++ < 3 { { state $n = 0; $n++; say $n } }   # raku: 1 1 1 — mutsu printed 1 2 3
```

mutsu persisted the counter for `while`/`until`/C-style `loop`/`repeat`
bodies because the #5959 sole-source-block ResetStateLocals suppression
(`loop_body_is_sole_block`) applied unconditionally at those three compile
sites. The `for` twin was fixed by gating on
`Stmt::For::is_statement_modifier`; the ticket assumed `Stmt::While` and
`Stmt::Loop` would need the same flag (~44 construction sites).

They don't. A raku baseline run showed these loops have **no
state-persisting statement-modifier twin to protect**: raku never calls a
bare `{...} while COND` / `{...} until COND` block at all (the Block
literal is evaluated, not invoked — the loop spins silently), C-style
`loop` has no modifier form, and the parser inlines a `repeat { ... }`
block's statements directly into the body (no `Stmt::Block` wrapper). So
the fix is purely negative: drop the suppression at the `While`, C-style
`Loop`, and `repeat` compile sites. Nested sole blocks now reset per
iteration (1 1 1), and direct-body `state` still accumulates via the
loop-statement-entry reset.

Remaining (separate) divergence: mutsu still *calls* a bare `{...} while
COND` modifier block (printing 1 1 1) where raku evaluates it uninvoked
(no output). Not pinned by roast; left as-is.

Pinned by `t/state-nested-block-non-for-loops.t` (6 cases, verified
against raku).
