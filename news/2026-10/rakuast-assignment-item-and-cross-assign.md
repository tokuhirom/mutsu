# RakuAST: `Assignment(:item)` follows rakudo's rule, `X=` is a cross metaop

Whether an `Assignment` node is `:item` now comes from the rule rakudo 2026.09's
grammar uses (`$*LEFTSIGIL` in `src/Raku/Grammar.nqp`), not from the target's
own sigil:

- the first variable of an expression sets the sigil;
- any postfix (a method call, a subscript) makes it `@`;
- `infix:<=>` is an item assignment exactly when it reads `$`, and then clears it;
- an argument list, a parenthesized list, a ternary's middle or a block is an
  expression of its own.

So `1 andthen $x = 2` is `:item`, `$x.say andthen $x = 2` is not, and in
`$a = $b = 1` both are. The flag is derived in one pass over the finished
`.AST` tree (`rakuast/assignment_item.rs`). Lowering does not read it.

`@a X= 1` is now `ApplyListInfix(MetaInfix::Cross(Assignment(:item)), (@a, 1))`,
as on rakudo. mutsu used to render it as an assignment of the cross back to
`@a`. `EVAL` lowers it back to the parser's form.

Found by the roast-wide `.AST.raku` comparison (#7564); closes #14884. The
lvalue trees whose shape still differs are #14911.
