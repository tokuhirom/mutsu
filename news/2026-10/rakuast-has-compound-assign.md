# RakuAST renders `has $!g //= EXPR`

A `has` declaration may sit anywhere in a class body, even in a method, and
may be followed by a compound assignment operator that is not one of its
initializers:

```raku
class Pool {
    method allocate { has $!out-of-gas //= "exhausted"; $!out-of-gas }
}
```

`.AST` refused it. It now renders what rakudo does, an `ApplyInfix` whose
left operand is the attribute's `VarDeclaration::Simple` and whose infix is
`MetaInfix::Assign(Infix("//"))`. EVAL of the tree installs the attribute on
the class. The parser and the RakuAST lowering now build the declaration and
its assignment with one shared helper.

Two more `t/` files pass under `MUTSU_RAKUAST=1`.
