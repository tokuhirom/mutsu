# RakuAST: method literals `method ($n) { … }`

The "invocant marker without an invocant" refusal stopped 54 `t/` files at
`.AST`, most of them at a Proxy's `FETCH => method () { … }`. A method
literal reaches mutsu's closure binder with its receiver as the first
positional argument. The parser therefore prepends a synthetic `self`
parameter, carrying `is_invocant` and the `implicit-invocant` trait, and
the converter refused that receiver as an invocant marker with nothing
behind it. A method literal that did convert rendered as a `Sub`, whatever
its declarator.

Measured on rakudo 2026.09, `method ($n) { … }` is a nameless
`RakuAST::Method`, and `submethod { … }` a `Submethod`. Each signature lists
only the written parameters. The parser's builder is now one function,
`parser::anon_method_expr`, which `make_anon_method` also goes through.

- The converter renders the `Method` / `Submethod` without the receiver,
  and only when that receiver is exactly the synthetic one.
- Lowering hands the written parameters back to `anon_method_expr`, which
  puts the receiver in front again.

A declared invocant still declines, because the parser folds it into the
receiver:

- with a type (`method (Int:D: $x)`);
- with a body alias (`method ($me: $x)` becomes `my $me := self`).

`RakuAST::Method` and `Submethod` also gain the `name` / `signature` /
`traits` / `body` accessors a `Sub` has.
