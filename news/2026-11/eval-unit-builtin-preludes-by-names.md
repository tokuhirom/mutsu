# An EVAL'd unit gets the builtin preludes it needs

The builtin preludes (`trait_mod:<does>`, `trait_mod:<of>`, `Rational`,
`Metamodel::Naming`, ...) were injected only into a program or module, gated on
the text of its source. An `EVAL`'d string, or an `EVAL($ast)` tree with no text
at all, ran without them: `Unknown function: trait_mod:<does>`. The gates now
live in one list (`Interpreter::inject_unit_preludes`) used by the program and
by every `EVAL` unit; for a unit the gates are asked of a text view built from
the names its tree uses (`CodeText::from_unit`). A gate that needs a spelling
only source text has (`:default(` at a call site) stays closed for a tree.
