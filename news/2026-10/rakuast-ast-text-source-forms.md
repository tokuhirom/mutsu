# `.AST` text keeps the source form of calls, pairs, ranges, topic calls and subscripts

The `.AST` text of constructs mutsu already supported differed from rakudo 2026.09
in the spelling of many ordinary statements: a bare `foo 1` rendered as `foo(1)`, a
colonpair `:a(1)` as `a => 1`, `^5` as `0 ..^ 5`, `.say` as `$_.say`, `%h<a>` as
`%h{'a'}`. The parser threw those distinctions away because the compiler does not
need them, so the RakuAST layer could not get them back.

The parser now records the spelling in compiler-ignored fields (the pattern
`HashSpelling` set): `listop` on `Expr::Call` / `Stmt::Call`, `BinaryForm` on
`Expr::Binary` (the four colonpair spellings and `^N`), `on_topic` on
`Expr::MethodCall` and `IndexSpelling` on `Expr::Index`. `wrap_composition_operands`
was rebuilding these nodes with defaults, which silently dropped every form; it
now carries them through. The RakuAST conversion renders the node rakudo has for
each form, and the lowering reads them back, so the round trip stays exact.

Alongside the spelling, a measured sweep against rakudo fixed the shape of: `Block`
flags (`may-have-signature`, `implicit-topic`, `required-topic`, the `CATCH` /
`CONTROL` exception block), qualified calls, the parentheses a postfix operand
loses (and keeps, for `(EXPR for LIST).m`), `Whatever` priming, the empty list,
`Var::Attribute` and `Var::Attribute::Public`, `but` / `does` (`Mixin`),
`Call::PrivateMethod`, `nqp::op(...)` (`RakuAST::Nqp`), calls on type names
(`Num(1)`, `Array[Int]`), `start` / `quietly` / `sink` (`StatementPrefix::*`),
labels, the `use v6.d` argument and `use newline :crlf`.

A differential corpus (every eighth `t/**/*.t`, `.AST` of the whole file, compared
statement by statement with rakudo's) measures the result; see #7564 for the
numbers and the findings that remain. `t/rakuast/rakuast-source-forms.t` pins one
snippet per construct against rakudo's own text, and the `MUTSU_RAKUAST=1`
round-trip ratchet is unchanged.
