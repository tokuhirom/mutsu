# `for @a[i] { ... }` does not alias the element for writeback

`for EXPR { ... }` over a single non-iterable element (`for @a[i] { .=Int }`)
should topicalize `@a[i]` the same way `with @a[i] { ... }` does — as an
lvalue alias, so `.=Int`/`$_ = ...` inside the body write back to the
element. `raku` does this; mutsu does not:

```
$ raku -e 'my @a = "1","2"; for @a[1] { .=Int }; say @a.raku'
["1", 2]
$ mutsu -e 'my @a = "1","2"; for @a[1] { .=Int }; say @a.raku'
["1", "2"]
```

Found as a side note while fixing the `with`-statement-modifier element
writeback bug (`news/2026-08/with-statement-modifier-element-writeback.md`) —
that fix only covers `with`/`given`'s `Expr::DoStmt(Stmt::Given)` compile
path (`src/compiler/expr_block.rs`), not `for`'s loop compilation
(`src/compiler/stmt.rs`'s `Stmt::For` handling / `src/vm/vm_control_ops.rs`),
which is a different opcode family entirely.

## Where to look

- `src/compiler/stmt.rs`'s `Stmt::For` compilation: does it ever detect a
  non-iterable single-element source (`@a[i]`, `%h<k>`) as its own case, or
  does everything go through the general iterable path (which would coerce
  `@a[i]`'s value into a one-element list and iterate that, losing the
  container/index pair needed for writeback)? Compare against how `given`
  detects `element_source` (`stmt.rs` around the `Stmt::Given` arm, `Expr::Index`
  matching + `OpCode::TagElementSource`).
- The VM's loop execution (`src/vm/vm_control_ops.rs` or wherever `for`
  iterates) would need the same `write_back_element_source` treatment
  (`src/vm/vm_loop_writeback.rs`) `given`/`with` now get consistently.
