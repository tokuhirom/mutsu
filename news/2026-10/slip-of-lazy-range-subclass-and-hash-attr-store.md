# `|` over a never-ending Range subclass no longer hangs; hash attribute stores skip tie probes

`|$x` of a `Range`/`Seq`/`List` subclass with its own `iterator` called `.list`,
which drained the iterator before slipping it, so `our @A000027 = |ℕ` (with `ℕ`
a never-ending `Range` subclass whose iterator is lazy) never finished and
`use Math::Sequences::Integer` hung at load. A lazy user iterator now rides the
Slip as one lazy list, as an unbounded `Range` already did, and a one-item Slip
of such a list keeps `my @a = |(1..*)` lazy (it used to give `((...) (Any) (Any))`).
Loading the module takes 0.5 s on the `profiling` build.

Separately, `%!h = ...` / `@!a = ...` inside a method asked two by-name env walks
whether the container was tied or had a role mixed in; an attribute's container
lives in `self`'s attribute cell, so a plain Hash/Array there answers both at
once. The store `method set-h(%ch) { %!children = %ch; self }` goes from 41.8k to
37.3k Ir per call on the `profiling` build.
