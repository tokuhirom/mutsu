# EVAL units: prologue end marker, U+2212 atomic subtract, prelude name gates

Three more `EVAL` fixes behind the `EVAL(slurp($file).AST)` ratchet (#16356), each
checked against rakudo:

- An `EVAL`'d unit with a BEGIN-time prologue ends it with the same marker the main
  unit has, so a closure compiled after it treats a binding the prologue only declared
  (`my &b1 := ...`) as a forward reference: `EVAL q[my &a1 := -> { b1 }; my &b1 := ...;
  class C does R { }]` no longer dies with `Unknown function: Callable`.
- `$i ⚛−= 2` (U+2212 MINUS SIGN) lowers to the atomic subtract-assign, like `⚛-=`.
- A prelude gate that skips a unit declaring `role Rational` matched any class whose
  name merely starts with it (`class Rational9795Class does Rational[UInt]`); the
  gate now asks for the whole name.
