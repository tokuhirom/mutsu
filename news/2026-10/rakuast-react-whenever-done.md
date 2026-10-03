# RakuAST: `react`, `whenever` and `done`

A re-survey after the proto slice found 124 `t/` files that `.AST` refused,
nearly all of them because of a `react` or `whenever`. Measured on rakudo
2026.09:

- `react { … }` is a `StatementPrefix::React` around its `Block`. The
  statement form, `react whenever S { … }` or `react foo`, holds the
  statement directly. The parser built the same `Stmt::React` for both, so
  the statement now records which form it was in a new `blorst` flag.
- `whenever S { … }` is a `Statement::Whenever(trigger => S, body => …)`. A
  bare block body carries rakudo's `implicit-topic`, `required-topic` and
  `may-have-signature` flags. A pointy body (`-> $v`, `-> Str $l`,
  `-> $a, $b`) is a `PointyBlock`. The parser takes that pointy block apart
  into the statement's parameters, so the converter rebuilds it and the
  lowering takes it apart again.
- `done` is a `Call::Name::WithoutParentheses`.

`done` needed care on the way back. The ratchet caught a first version that
lowered every bare `done` to a fixed `Stmt::ReactDone`: that ignored a
lexical `&done` shadowing the completion
(`t/vm/scope/lexical-shadows-builtin-call.t`). It now lowers to the bare word
the compiler already resolves, a completion unless a `&done` is in scope.

Pointy blocks with a sub-signature (`-> ($a, $b)`) still decline, as they do
everywhere else.

The round-trip ratchet grew by 41 files, to 3279.
