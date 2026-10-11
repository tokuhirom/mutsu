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

Most of those partition bugs had since been fixed elsewhere. Extending the
bound exposed three more, fixed here:

- A typed class-body `my` static left its type in the name-keyed env lane
  after the body, so a later class with a same-named static of another type
  imposed that type on the first class's methods (`class A { my A $i; method
  m { $i //= A.new } }; class B { my B $i }` died assigning an `A`). Leaving a
  class body now records each static's type per class and restores the lane,
  and a method's store to the static checks the recorded type.
- `$PROCESS::OUT` read through the env key `*OUT` that a mainline `my $*OUT`
  also uses, so the declaration (or its static half, at BEGIN time) replaced
  the process handle `Test` prints through. A `my $*X` in scope now leaves
  `$PROCESS::X` at the process value.
- `anon class C { }` parses as a block holding a named class, which the
  nested-shell collector took for an `our` class declared in code and
  installed. It is skipped now, so `C` is undeclared again, as in raku.

So the gate and the head-of-unit pass (`Compiler::hoist_nested_type_decl_shells`)
are gone: there is now one mechanism, and a nested shell always runs after the
declarations that precede its statement and before any BEGIN that follows it
(#10524).
