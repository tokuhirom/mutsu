# RakuAST: a Method visits its implicit Signature, `$.n` its expansion

`visit-children` of a `RakuAST::Method` / `Submethod` now shows the `Signature` rakudo fills in: a
leading invocant `Parameter` typed by the enclosing class or role (a bare one in a grammar or
outside a package) unless the written signature opens with an invocant, and a trailing `%_`
parameter unless the written signature collects named arguments (`*%h`, `|c`). A method with no
written signature gets a whole one. A `Var::Attribute::Public` (`$.n`, `@.a`, `%.h`) visits
`self.n.item` (`.list`, `.hash`). The synthesized nodes carry no origin and are not fields, so
the constructor form, equality and the `EVAL` round trip are unchanged (#16423).
