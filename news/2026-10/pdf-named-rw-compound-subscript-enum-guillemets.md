# Four general fixes found by the PDF distribution

- A named `is rw` multi candidate is not applicable to a literal (`coerce(:bool(True))`), so the
  sibling `is copy` candidate runs instead of the call dying with "expects a writable container".
- `(@a[$i] *= 85) += $v` (a compound assignment on a parenthesized subscript compound assignment)
  works for array, native-array and hash elements; the subscript is evaluated once.
- `enum « :Baz<Baz> »` (anonymous enum with guillemets after a space) parses.
- Calling a method on a `subset` type object reaches the methods of its base type
  (`my subset S of R where ...; S.COERCE(...)`).
