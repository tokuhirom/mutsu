# Proxy chains, itemized Pairs and Seq defaults in hash construction

The `annotations` distribution's `Keeper` container coerces to a Map through
`Map.new: reverse @of Z=> self.Seq`, where `@of` defaults to a Seq of `Binder`
proxies whose `FETCH is raw` answers another Proxy. Four divergences from
rakudo met there, and all are fixed:

- `Map.new` / `Hash.new` now read an itemized Pair (`my $p = a => 1;
  Map.new(($p,))`) as a Pair instead of dying with "Odd number of elements".
- A user `infix:<op>` that is neither `is rw` nor `is raw` returns its result
  decontainerized, called as an infix or through a metaoperator (`Z[snd]`), as
  a named call already did.
- A returned Proxy, and the key of a metaoperator `=>` (`@proxies Z=> @v`
  with an unbounded operand), read through the whole Proxy chain with the
  shared `auto_fetch_proxy`.
- A Seq default of an `@` parameter binds through `.cache`, so the parameter
  holds a List, as a supplied Seq does.
- `Pair.raku` with an allomorph key (`IntStr.new(2, "k") => 1`) uses the
  colonpair form when the string is identifier-like, like rakudo.

`annotations`' `t/01-containers.rakutest` and `t/02-direct.rakutest` now pass.
