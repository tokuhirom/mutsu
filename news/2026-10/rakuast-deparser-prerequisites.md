# Rakudo's own RakuAST::Deparse gets further on mutsu

Running rakudo 2026.09's `RakuAST::Deparse` unchanged on mutsu over a quarter
of roast stopped on gaps any Raku-level RakuAST walker hits (#14860). Fixed:

- `nqp::getlexdyn` answers null for an unbound dynamic, as on MoarVM.
- A RakuAST node now equals itself, so a `for @nodes.kv -> $i, $n` loop over
  a bound List no longer dies with "Cannot modify an immutable value".
- `my $x := EXPR with $t` (and `if`/`unless`/`without`) keeps the bind.
- `Str.leading-whitespace`, `.trailing-whitespace` and `.chomp($needle)`.
- Computed node methods, each after rakudo's definition: `default-scope`,
  `scope`, `augmented`, `declarator`, `Name.is-anonymous`/`is-empty`,
  `Call::Method.dispatch`/`dispatcher`/`macroish`, `colonpairs` of an infix or
  postfix, `ApplyPostfix.on-topic`, `Assignment.operator`, `Parameter.value`,
  `QuotedString.has-variables`, `Contextualizer.sigil`, `StatementPrefix.type`,
  `VarDeclaration::Simple.original-type` and `Signature.parameters-initialized`.

`RakuAST::Signature.new` now keeps a `parameters` field only when given, and
the empty signature a parsed routine fills in has an initialized, empty
parameter list, as on rakudo.
