# Placeholder and declared-name walkers run on the typed AST visitor

The placeholder analyses (`collect_placeholders`, `collect_placeholders_shallow`,
`collect_unattached_placeholders`, `collect_where_assign_placeholders`), the bare-`$name`-vs-`$^name`
ordering checks in `placeholder_order.rs`, the attribute-initializer virtual-call check, the
routine-local name collectors behind the return env merge, and the WhateverCode "does this mention
`$_`?" check are now `ast_visit::Visit` implementations instead of hand-rolled recursive matches
(ADR-0137). They live in `src/ast/placeholders.rs`, `src/ast/body_local_names.rs` and
`src/ast/virtual_call.rs`, with the ADR-0048 placeholder-scope oracle in
`src/ast/placeholder_kind.rs`. A block's own placeholder scope is one shared walk
(`walk_stmt_placeholder_scope` / `walk_expr_placeholder_scope`) that every collector and both ordering
checks use, so they can no longer disagree on where a block's scope ends. `src/ast.rs` shrank by about
1700 lines and the walker ratchet fell from 25 to 5 in these files.

The old walkers each listed a handful of positions and skipped the rest. Reaching every position
changed these behaviours, each checked against rakudo:

- A placeholder in a C-style `loop` header, a `==>` feed, an interpolating heredoc, a variable's
  `where` thunk or an expression-position assignment (`($^x = 5)`) is now the block's parameter.
- The mainline (and a class/role/`do` body) now rejects a placeholder in a statement-modifier body
  (`say $^a if True`, `... while`, `... given`), a `loop` header, `temp`/`let`, a variable's `where`
  thunk, a `subset` predicate, a WhateverCode (`say * + $^a`), a reduction (`[+] @_`), `:exists`
  and an element assignment (`@_[0] = 1`). Likewise a routine with a signature now rejects `@_` in
  a destructuring declaration (`sub f($x) { my ($a) = @_ }`), as it already did in `my @a = @_`.
- `X::Syntax::VirtualCall` now covers `$.attr` inside a pointy block, a `sub`, a nested `if`
  block, a `do` block and a WhateverCode in an attribute initializer; an anonymous method still
  rebinds the invocant.
- A statement modifier is checked in source order: `{ $^b if $b }` no longer reports `$b` as
  undeclared, while `{ $b = 1 if $^b }` and `{ say $b if 1; say $^b }` now do.
- A WhateverCode whose body mentions `$_` in a call argument or a string interpolation
  (`* + f($_)`, `* ~ "$_"`) no longer shadows the outer topic with its own parameter.

The WhateverCode priming-scope walks (`contains_whatever`, `count_whatever`, ...) stay hand-rolled:
only the curry operand positions count, so their positive list of positions is the semantics.
