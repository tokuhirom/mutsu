# `$a := my $z = 2` aliases `$a` to `$z`'s container

A rebind whose source is a declaration expression did not share the declared
variable's container. The statement-level `:=` compiled its right-hand side as
an ordinary call argument. A plain variable there becomes a `VarRef`, and
`SetLocal` adopts that variable's cell. A `DoStmt(VarDecl)` left only the value
on the stack, so the rebind stored that value into a fresh cell. Without a
closure, the frame-slot pairing hid the problem. With a closure that had
captured `$a`, a write from the closure reached neither name:

```raku
sub f { my $a = 1; my &w = { $a = 7 }; $a := my $z = 2; w(); say $a; say $z }
f();   # raku: 7 7    mutsu before: 2 2    now: 7 7
```

The compiler (`src/compiler/stmt.rs`, `Stmt::Assign` with `AssignOp::Bind`)
now handles a source that is a declaration of a plain scalar lexical in two
steps. It compiles the declaration first, then binds the declared variable
exactly as `$a := $z` does, so both source shapes share one mechanism. The
declaration has to be emitted before the `MarkScalarBindContext` /
`MarkRebindContext` markers. Otherwise its own store consumes them and `$z`
comes out bound read-only to its initial value.

Pinned by `t/vm/binding/bind-to-declaration-shares-container.t`. Closes #9308.
