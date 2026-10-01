# A capture group under nested quantifiers is one slot, in the value and in code's view

In raku the groups around a capture group do not capture, so `(\d)` in
`[ [ (\d) ] +% '.' ] +% ';'` is one `$0` that every iteration of both quantifiers feeds. mutsu
had three defects here (#10535), all from a quantifier's fold knowing nothing of the quantifier
around it:

- **The value.** `"1.2;3.4" ~~ / [ [ (\d) ] +% '.' ] +% ';' /` gave `$0` = `[2,4]` (and
  `[ [ (\d) ]+ ]+` on `1234` gave `[4]`) where raku has `[1,2,3,4]`: an outer fold listed one entry
  per iteration slot, the last entry only for a slot an inner quantifier had already folded. A slot
  that is already a list now contributes each of its entries, in every fold (`PosSlot::push_entries_to`).
- **Code's view.** `$/` in a `{ … }` inside the inner iteration showed the inner fold as a slot of its own
  (`[]|[1,2]`) instead of the outer atom's slot (`[1,2]`). The view now composes the folds from the
  outermost level in (`ViewFold`, `OuterBackrefCaps::append_captures`), in the tree walk and in the
  compiled engine alike, so code sees `[1]`, `[1,2]`, `[1,2,3]`, `[1,2,3,4]` at any depth.
- **The stride.** A nested separated quantifier's *separator* captures took slots the enclosing
  quantifier did not count, so `[ [ (\d) ] +% (<[.]>) ] +% ';'` lost the separator slot and
  `[ (\d) +% (<[.]>) ]+` mixed the separator's entries into the atom's list.

Still open and filed: code in an *unseparated* quantifier's iteration sees the raw entries rather than
the folded slot (#10597), an aliased group `$<x>=(\d)` under a quantifier takes a positional slot
rakudo does not (#10598), and the tree walk folds a `[ … ]` group after a capture inside a separated
atom into the wrong slot (#10599).
