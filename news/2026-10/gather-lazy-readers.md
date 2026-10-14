# `polymod`, `flat` and `cache` read an infinite gather lazily

`600.polymod(gather { loop { take 10 } })`, `flat(gather { loop {...} })`,
`gather { loop {...} }.flat` and `cache(gather { loop {...} })` used to hang.
Rakudo reads each of them lazily, and so does mutsu now:

- `flat` flattens the gather as it is pulled, and still descends into the lists
  and nested gathers the body took. The result is a plain `Seq` that holds the
  flattening pipe in its own adapter source.
- `cache(g)` is the gather's own `.cache`: a List over the cached Seq that runs
  the body only as far as it is read.
- `polymod` pulls only the divisors the decomposition can reach.
- `.head(n)` on a cached gather (`.cache`, or one already subscripted) pulls
  only the prefix it needs, without consuming the Seq.

A finite gather gives the same answers as before. `slip` over an infinite gather
needs a lazy `Slip` representation and is tracked in #13117.
