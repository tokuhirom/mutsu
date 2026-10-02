# A numbered alias under a quantifier keeps every iteration

`"12" ~~ / [ $0=(\d) ]+ /` now gives `$0` as a list of both digits on the
compiled regex engine, as rakudo and the tree walk do (#10792). The compiled
engine inlines a quantifier's body into the enclosing capture level, so a
`$N=` inside it was numbered against the whole level and each iteration
overwrote the slot the previous one had filed; only the last iteration
survived. A body that applies a numbered alias now gets a capture level of its
own per iteration (`OpenPlainIter`), numbering from the iteration's first slot
exactly as the walk's nested match of the body does.

The per-iteration stride now also counts a `$N=` alias over a non-capturing
atom, so `[ $0=\w ]+` folds into one list-valued `$0` on both engines instead
of leaving one top-level slot per iteration.

Rakudo's full rule — slots numbered statically over the whole regex, a slot
filled twice becoming a list — is tracked separately in #10895.
