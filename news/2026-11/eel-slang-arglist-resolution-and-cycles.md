# EEL: index descriptors from a slang's arguments, `class Pair is Capture`, cyclic `eqv`

EEL 0.3 compiles `<.event(:path(*.starts-with('/api/')), :value(1 < * <= 42))>`
into index descriptors by reading the RakuAST of its arguments, which the parser
hands it through the core `<arglist>`. Those nodes now look as they do on
rakudo:

- a `*` a priming curries is a `RakuAST::WhateverCode::Argument` (the slang
  host's productions run the same ADR-0033 classifier the unit's parse runs),
  while `:a(*)` keeps its bare star a `Term::Whatever`, in `.AST` too;
- an operator's lookup resolves to the setting's
  `RakuAST::Declaration::External::Setting`, spelled as rakudo spells it
  (`&infix:<==>`, `&infix:«<=»`), and that declaration does
  `RakuAST::CompileTimeValue`; an operator the surrounding code declares is not
  the setting's;
- `QuotedString` / `Literal` answer `maybe-compile-time-value`, and the host's
  `$*R` answers `resolve-lexical-constant-in-setting`.

Two unrelated crashes the suite hit are fixed as well: a user class that shares
a builtin's name and inherits a builtin (`class Pair is Capture`) is built as
itself rather than by the builtin `Pair` constructor, and `eqv` over a container
that holds itself (`%h<self> := %h`) compares the cycle once instead of
overflowing the stack.
