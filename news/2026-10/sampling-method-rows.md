# pick, roll and pickpairs are rows of the method table

`pick`, `roll` and `pickpairs` had five copies: the zero- and one-argument arity cascades each held their own
`pick`/`roll`/`pickpairs` arm, two copies of the weighted `Bag`/`Mix` sampler lived in `methods_0arg` and
`methods_narg`, and the integer-range fast path sat in `base.rs`. They are now one implementation,
`builtins::sampling`, and rows on the owners Rakudo declares them on (`Any`, `List`, `Range`, `Map` and the six
quant hashes), flagged `RANDOM` so the debug cross-check does not re-run them. The cascade arms for receivers with
no dispatch shape (a `Seq`, a shaped array, a lazy list, an itemized hash) call the same functions.

Behaviour toward Rakudo: `Set.pickpairs($n)` and `Mix.pickpairs($n)` used to die with "No such method"; they now
pick `$n` distinct pairs like `Bag.pickpairs($n)` always did.
