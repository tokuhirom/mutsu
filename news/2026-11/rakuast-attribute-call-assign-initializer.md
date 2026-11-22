# RakuAST: `has R $.r .= new(...)` has an `Initializer::CallAssign`

The parser turns an attribute default written `.= new(...)` into a call on
the declared type, so the RakuAST view showed it as `Initializer::Assign(R.new(...))`.
Rakudo 2026.11 has `Initializer::CallAssign(Call::Method)`. The parser now
keeps that spelling for a RakuAST parse (`Spelling::AttributeCallAssign`).
The view gives the `CallAssign`, so `.raku` matches Rakudo's, and the
lowering turns it back into the call on the declared type, so `EVAL` of the
tree builds the default.

As in Rakudo, the declaration's `visit-children` does not walk that
initializer. The call is visited through the attribute's
`Trait::WillBuild`, whose type operand and call are now placed. Rakudo wraps
the call in a generated `Method::Initializer` (an implicit signature and a
block), and it also visits one for a plain `= 5` default. mutsu does not
model that class yet; #18621 stays open for it.
