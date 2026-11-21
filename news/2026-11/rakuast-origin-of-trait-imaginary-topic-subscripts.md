# RakuAST `of` traits, imaginary literals and topic subscripts

`.AST` now matches rakudo 2026.11 on three more points:

- a declaration's `of TYPE` trait (`my @arr of Int`) is placed together with
  its type. The declaration's `.type` now answers that type, where mutsu
  used to answer the `RakuAST::Type` type object. `visit-children` also
  visits that type first, as rakudo does;
- an imaginary literal (`0.0i`, `3e2i`, `2i`) places its number and its `i`
  postfix, which the parser had read as one token;
- a subscript on the topic (`.<a>`, `.[0]`, `.{"a"}`) is placed from after
  its dot, together with its index (#15780).
