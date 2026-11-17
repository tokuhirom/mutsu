# A `my class` term is lexical, and RakuAST keeps the `my` of a term class and a `my rule`

A `my class` written as a term (`my $m = my class MyNum is Num { }.new(1)`,
`(my class :: does Real { ... })`) went through the anonymous-class term
parser. That parser dropped the `my`, so the class was installed like an `our`
one: its name stayed visible after its scope. Now the class is lexical, as the
statement form makes it, and as rakudo does. RakuAST keeps its `my` too:
`my class :: { }` views as `scope => "my"` over the name `::`, while a plain
`class :: { }` is still `anon`.

`my rule foo` and `our rule foo` lost their scope as well. `Stmt::RuleDecl`
had no scope fields, unlike `Stmt::TokenDecl`. It now records them as written,
and the view and the lowering carry them both ways.

Through the `.DEPARSE` corpus: `roast/S02-types/nan.t`,
`roast/S32-num/real-bridge.t` and `roast/S05-metasyntax/unknown.t` now match
rakudo statement for statement in both corpora. `real-bridge.t` used to fail
to view as a whole, because of its `my class ::` term.
