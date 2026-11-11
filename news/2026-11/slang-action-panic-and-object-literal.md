# A slang action's `$/.panic`, and objects a slang embeds in a proto body

Three gaps EEL 0.3's "unsupported proto body has explicit diagnostic" check
(`t/19-proto.t`, #16888) ran into, in order:

- **An embedded object in the RakuAST round trip.** A slang action that builds
  code around an object (`RakuAST::Literal.from-value($definition)`) put that
  object into the unit; where the parser lowered such a body itself (a `proto`
  with a non-`{*}` body), the RakuAST frontend could not convert it back
  ("`.AST` does not yet support this construct: literal Instance"). Any object
  now converts to rakudo's plain `RakuAST::Literal` holding it.
- **`$/.panic(...)`.** `HLL::Grammar`'s `panic` was missing from the slang
  host's grammar and from the unit's `Raku::Grammar`; it now throws the
  compile-time error with the message. A replayed action (ADR-17243) gets its
  `$/` as a cursor of the host's grammar, a type the host minted and the unit
  does not know, so the replay re-makes it as a cursor of the unit's
  `Raku::Grammar`.
- **A fatal error in a nested production.** A panic inside a production the
  slang reaches through a core rule (`method-def` parsing a body whose
  statement list the slang overrides) was taken for "this core rule did not
  match", so the slang backtracked and the unit failed later with an
  unrelated error. A fatal parse error now ends the parse with its message.
