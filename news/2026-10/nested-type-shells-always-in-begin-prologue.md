# Every nested type shell is a BEGIN prologue effect

An `our` class or role declared inside a routine or block is installed at
compile time through a declaration-only shell (#10470). Until now only a unit
with a nested class that composes a role placed those shells in its BEGIN
prologue (ADR-0134), at the declaring statement's place among the unit's
BEGIN-time effects. Every other unit fell back to a separate compiler pass
that emitted the shells at the head of the unit's code, because extending the
prologue's bound over the whole unit used to break several declaration forms
(`my T $x .= new`, `my T $x.method`, `plan` at the top of a file, `INIT`
calls).

Those partition bugs have since been fixed, so the gate and the head-of-unit
pass (`Compiler::hoist_nested_type_decl_shells`) are gone: there is now one
mechanism, and a nested shell always runs after the declarations that precede
its statement and before any BEGIN that follows it (#10524).
