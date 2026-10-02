# The read-only AST visitor carries the tree's lifetime

`trait Visit` (ADR-0137) is now `trait Visit<'ast>`, syn-style: its hooks take
`&'ast Stmt`, `&'ast Expr`, `&'ast ParamDef` and `&'ast RegexNode`, and the
`walk_*` functions and the `compiler/scope_scan.rs` helpers pass the same
lifetime on. An analysis can therefore collect references to the nodes it finds
instead of cloning them, or keeping a hand-rolled walk just to hand out a
borrow. `visit_name` still takes a plain `&str`: some names are computed rather
than borrowed from the tree. The ~75 implementations changed mechanically.

Put to use:

- The class body's nested-`has` collection (`opcode.rs`), the last walker kept
  hand-rolled only because the hooks could not hand out `&'a Stmt`, is now a
  visitor. It used to look only into `sub`/`method` bodies and a fixed list of
  control-flow blocks, and to list a body's direct `has` before the deeper ones.
  Rakudo installs a `has` wherever it lexically sits in the class, so a `has` in
  a closure, a run-time phaser or a `do` block inside a method is now an
  attribute too (a BEGIN/CHECK still attaches its `has` when it runs),
  and nested attributes keep source order (`C.^attributes` is `$!a $!b` for
  `method m { if 1 { has $.a }; has $.b }`, as in rakudo; mutsu printed
  `$!b $!a`). Pinned in `t/oo/method/has-decl-nested-in-method-closures.t`.
- The nested `our sub` scan and the nested exported-declaration scan collect
  borrows; only the declarations actually hoisted or lifted are cloned.

The walker ratchet falls by 2.
