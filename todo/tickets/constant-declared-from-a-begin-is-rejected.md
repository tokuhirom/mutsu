# `constant X = BEGIN …` dies with "Cannot assign to a readonly variable"

A `constant` whose initialiser is a `BEGIN` phaser is rejected:

```raku
constant E = BEGIN 5;        # Cannot assign to a readonly variable (E) or a value
my constant F = BEGIN 5;     # same
constant G = BEGIN { 5 };    # same
```

raku accepts all three (`say E` prints 5). A plain `constant D = 5;` works, and
`my $a = BEGIN 5;` works, so the failure is specific to the combination.

The AST is right — `VarDecl { name: "C", expr: PhaserExpr { kind: Begin, … },
custom_traits: [("__constant", None), …] }` — so this is in the compile/run of a
`__constant` declaration whose initialiser compiles to `BeginOnceExpr`. The
likely shape is that the memoized BEGIN's store happens as a second write, after
the constant has already been marked readonly.

## Scope

Not a Cro blocker — the Cro tree uses `BEGIN` only in value position inside a
method, which is fixed in
`news/2026-08/begin-in-value-position-is-the-block-value.md`. Filed because it is
an ordinary Raku shape that silently fails to compile.

## Where to look

The `__constant` custom trait handling in the `Stmt::VarDecl` compilation
(`src/compiler/stmt.rs`) and `Compiler::compile_phaser_expr`
(`src/compiler/expr_data.rs`, the `PhaserKind::Begin` arm that emits
`OpCode::BeginOnceExpr`).
