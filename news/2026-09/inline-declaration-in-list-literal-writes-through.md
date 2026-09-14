# An inline `my` inside a list literal is a real lvalue, not a copy

```raku
my $a = 1;
(my $x = $a, 6)[0] = 10;
say "x=$x a=$a";
```

`raku` prints `x=10 a=1`. mutsu threw:

```
Cannot modify an immutable List ((1 6))
```

An inline declaration inside a list literal (`(my $x = $a, 6)`) denotes the
freshly-declared variable's own container, exactly like a bare variable
element (`($a, 6)`) does — so a subscript store through the list writes
that variable directly. `compile_expr_index_assign`'s literal-list-target
optimization (`src/compiler/expr_closure.rs`) already special-cased a plain
`Expr::Var` element to assign straight into the named variable, but an
inline `Expr::DoStmt(VarDecl)` element fell through to the generic
"literal element" branch and was refused as immutable.

Fixed by recognizing the `DoStmt(VarDecl)` shape alongside `Expr::Var`:
run the declaration (and its initializer) for its side effect first, then
assign into the newly-declared variable by name the same way.

`t/collections/list-literal-inline-decl-store.t` covers the declared
element at index 0 and a non-zero index, a sibling inline declaration that
is not the assignment target, an uninitialized inline declaration, and
pins that a genuine literal element (`(1, 2)[0] = 5`) is still refused.

Part of the survey in
[#7556](https://github.com/tokuhirom/mutsu/issues/7556).
