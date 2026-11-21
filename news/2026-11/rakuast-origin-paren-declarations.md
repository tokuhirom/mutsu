# `.origin` of `.=` initializers and parenthesized declarations

Three related forms now carry the ranges rakudo 2026.11 gives them:

- **A `.=` initializer** (`my Int $c .= new`). The `Initializer::CallAssign`
  runs from the `.=` to the end, and the method call inside it is placed
  like the right side of a `.=` application.
- **A lone declaration or assignment in parentheses** (`(my Int $a = 1)`,
  `foo(($s = 5))`). The parser reads it as a statement and notes no term for
  it, so it takes the text between the parentheses. Its parts, and the
  `SemiList` and `Statement::Expression` around it, are derived from that
  range. A form ending in `;` stays without a range.
- **`.=` applied to a parenthesized left side** (`(my Int $b .= new).=new(42)`).
  The operator and the call are placed after the left side's range.

(#15780)
