# JSON::Mask: grammar `:temp`, `Cursor` attributes and set operators over lazy Seqs

Three gaps found by the real `JSON::Mask` test suite (all six files now pass under mutsu):

- `has Cursor $.cursor` (and `my Cursor $x`) accepted a grammar instance only through `~~`; the
  attribute/variable type check now treats `Cursor` as the `Match` alias it is.
- `:temp @*stack;` inside a grammar rule rebound the dynamic variable to a fresh empty container
  at rule entry. A bare `:temp` now keeps (a copy of) the current value, like `temp @*x` does.
- Set operators (`(-)`, `(|)`, `(&)`, `(^)`, `(<=)`, `(elem)`, ...) read a not-yet-pulled
  `.map`/`.grep` Seq operand as empty. Operands are now reified first.
