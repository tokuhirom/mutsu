# `.AST` text keeps the source form of calls, pairs, ranges, topic calls and subscripts

The `.AST` text of constructs mutsu already supported differed from rakudo 2026.09
in the spelling of many ordinary statements: a bare `foo 1` rendered as `foo(1)`, a
colonpair `:a(1)` as `a => 1`, `^5` as `0 ..^ 5`, `.say` as `$_.say`, `%h<a> = 1`
as an assignment over `%h{'a'}`, `so $x` as `?$x`. The parser threw those
distinctions away because the compiler does not need them, so the RakuAST layer
could not get them back.

The parser now records the spelling in fields the compiler ignores (the pattern
`HashSpelling` set): `listop` on `Expr::Call` / `Stmt::Call`, `BinaryForm` on
`Expr::Binary` (the colonpair spellings and `^N`), `sugar` on `Expr::MethodCall`
(`.say`, `$[1, 2]`, `lazy EXPR`), `IndexSpelling` on `Expr::Index` and
`Expr::IndexAssign`, `form` on a statement call's `CallArg::Named`, and `word` on
`Expr::Unary` (`so` / `not`). `wrap_composition_operands` was rebuilding these
nodes with defaults, which silently dropped every form; it now carries them
through. The RakuAST conversion renders the node rakudo has for each form, and the
lowering reads them back, so the round trip stays exact.

Alongside the spelling, a measured sweep against rakudo fixed the shape of: `Block`
flags (`may-have-signature`, `implicit-topic`, `required-topic`, the `CATCH` /
`CONTROL` exception block), qualified names (`from-identifier-parts`), the
parentheses a postfix operand loses (and keeps, for `(EXPR for LIST).m`),
`Whatever` priming, the empty list and `[]`, `Var::Attribute` and
`Var::Attribute::Public`, `but` / `does` (`Mixin`), `Call::PrivateMethod`,
`nqp::op(...)` (`RakuAST::Nqp`), calls on type names (`Num(1)`, `Array[Int]`; a
hidden marker tells `Type(1)` from `Type.(1)`), `start` / `quietly` / `sink` and
`lazy` / `hyper` / `race` (`StatementPrefix::*`), item contextualizers, labels,
type-only parameters, the placeholder variables, the list-associative sequence,
`^^` and set operators (`ApplyListInfix`), bracketed colonpair values
(`:a<x>`, `:a[1]`), the `use v6.d` argument and `use newline :crlf`.

`scripts/ast-text-corpus.sh` measures the result: every eighth `t/**/*.t`, the
`.AST` of the whole file under rakudo and mutsu, compared statement by statement.
27.1% of the statements were identical before this work; on the same sample 92.3%
are now, and 91.1% on a sample drawn after the merge with `main` (743 files, 10995
statements). The classes that remain (word-list and heredoc source
text, bare-statement prefixes, name classification) are filed as #12199, #12200 and
#12201. `t/rakuast/rakuast-source-forms.t` pins one snippet per construct against
rakudo's own text, and the `MUTSU_RAKUAST=1` round-trip ratchet is unchanged.
