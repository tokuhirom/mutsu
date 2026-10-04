# RakuAST: scoped and parameterised regex declarations

"regex declaration with scope / params / traits" was the most common `.AST`
refusal left, with 50 `t/` files. Splitting it by cause gave 24 for a
`my`/`our` scope (`my token t { … }`) and 20 for a parameter list
(`token rep($c, :$n) { … }`). One or two each were `multi` and exported
declarations.

Measured on rakudo 2026.09:

- A declaration's `scope` leads the node, and the default `has` renders
  nothing.
- A parameter list is the method-style `signature`, with the implicit
  `Type::Setting(Any)` on each scalar parameter. It sits between `name` and
  `body`.

mutsu now renders both, and the lowering reads them back into
`Stmt::TokenDecl` / `Stmt::RuleDecl`. `.AST` text for `my token t { a }` and
a grammar holding `token r($x, :$y)` and `rule p($s)` is identical to
rakudo's. `multi` and exported declarations stay refused under their own
message.

Testing the round trip turned up #11916. A token's defaulted named parameter
(`token n(:$k = 'z')`) is unset when the token is called as a bare subrule
`<n>`, so the match fails. It works with an explicit `<n(:k<z>)>` or through
`:rule<n>`. Plain mutsu shows the bug too, so it is unrelated to RakuAST.
