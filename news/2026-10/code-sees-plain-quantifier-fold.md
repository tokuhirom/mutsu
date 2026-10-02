# Code inside a `*` / `+` iteration sees the quantifier's folded slots

Rakudo matches a whole quantified chain on one cursor, so a capture group
under `[ … ]+` is a single slot from the first iteration on. Code running
inside an iteration now sees exactly that: `/ [ (\d) { say $/[0] } ]+ /` on
"123" reports `[1]`, `[1 2]`, `[1 2 3]` instead of three separate slots
`1`, `1|2`, `1|2|3` (#10597).

The tree walk hands an iteration's atom a view with the earlier iterations
folded and folds the iteration in progress into the same slots, the way a
separated quantifier's iterations already were. The compiled engine gives such
an iteration an inline capture level of its own (`OpenPlainIter` /
`ClosePlainIter`) that reads the same view, so both engines agree under
`MUTSU_RX_DIFF=1`, including plain and separated quantifiers nested in each
other.

Two neighbouring bugs went with it:

- the separated quantifier's fold scope leaked into deeper sub-patterns and
  into the rest of the pattern, so `[ (\d) [ (x) { … } ] ] +% ','` folded the
  `x` into the digit's slot, and code after the quantifier saw its own
  captures folded into the quantifier's. The scope now belongs to the one atom
  it was armed for, and a `[ … ]` nested in an iteration continues that
  iteration's slot numbering (#10599);
- an iteration whose `(x)?` did not match contributed a Nil entry to the
  folded list (`[ (\d)? x ]+` on "x1xx" listed three entries instead of one).
