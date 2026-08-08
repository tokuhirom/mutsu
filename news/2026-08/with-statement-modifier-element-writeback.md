# `.=Int with @a[i]` silently dropped the mutation

`t/http-router.rakutest` in the vendored Cro suite hung forever on any route
with a typed optional trailing parameter (`Int $page?`) whose value was
actually present in the URL (`/orders/history/2`, but not the bare
`/orders/history`). Isolated with `CRODBG=1`: the router's own debug trace
showed the route matched and its `Capture` was built with the raw path
segment `"2"` (a `Str`), not the `Int 2` the matcher's regex block is
supposed to produce — `Cro::HTTP::Router`'s matcher does exactly
`.=Int with @segs[2]` to convert a path segment before building the
Capture. Binding a `Str` where the handler signature declares `Int $page?`
then went nowhere; the request pipeline never delivered a response and the
client's `$responses.receive` blocked forever.

## Root cause

Minimal, Cro-independent repro:

```
$ mutsu -e 'my @a = "1","2"; .=Int with @a[1]; say @a.raku'
["1", "2"]     # raku: ["1", 2]
```

`with @a[i] { ... }` (the block form) already worked — `stmt.rs`'s
statement-position `Given` compiler detects an `Expr::Index` topic and emits
`OpCode::TagElementSource`, so the topic aliases the array element as an
lvalue and any mutation of `$_` writes back through
`write_back_element_source`.

The *statement-modifier* form (`.=Int with @a[i]`) takes a different path:
because `.=Int` parses as an expression statement, the parser wraps the whole
thing in `Expr::DoStmt(Stmt::Given { is_statement_modifier: true, ... })` to
preserve expression semantics (`src/parser/stmt/modifier.rs`). But the
expression-form `Given` compiler (`compile_expr_do_stmt` in
`src/compiler/expr_block.rs`) was a stripped-down copy of the statement-position
one that never got the `Expr::Index`/`TagElementSource` branch — only a bare
`Expr::Var`/`ArrayVar`/`HashVar` topic got tagged (`TagContainerRef`, for
whole-container writeback via shared mutation). An array/hash *element* topic
fell through to a plain `compile_expr(topic)`, pushing a value copy with no
way back to the source. The VM side (`exec_do_given_expr_op`) matched: it
never consulted `self.element_source` or called `write_back_element_source`
at all.

`do given @a[i] { ... }` (explicit block, still routed through the same
`DoStmt(Given)` path) had the identical bug — the statement-modifier form is
just the common way to trigger it.

## Fix

Mirror the statement-position compiler's `element_source` detection in
`compile_expr_do_stmt`'s `Given` arm, and give `exec_do_given_expr_op` the
same element-source save/writeback/one-shot-clear handling
`exec_given_op` already has.

Pin: `t/with-statement-modifier-element-writeback.t`. `http-router.rakutest`
no longer hangs (it still has other, unrelated failures — see the ticket
this fix does not close). A related gap in `for @a[i] { ... }` (which should
alias the same way but doesn't) is out of scope and filed separately:
`todo/tickets/for-single-element-topic-does-not-write-back.md`.
