# A plain `gather` is a `Seq`

A `gather { ... }` used to be a `LazyList` that only presented itself as a
`Seq`. It now is one: `MakeGather` builds a `Seq` whose body pulls from a new
`SeqSource::Gather` source, an adapter over the gather's existing coroutine
(ADR-11678). The coroutine, its GC tracing and its resume machinery are
unchanged; what changed is that everything Raku attaches to a `Seq` now applies
to a gather too:

- **Itemization.** `my $g = gather { take 2 }; $g.raku` is `$((2,).Seq)`, a
  `$`-held gather in a list renders as `("a", $((2,).Seq))`, `($g, 2).flat` has
  two elements and `my @a = $g` stores one.
- **Identity.** `$g === $g` and `$g.WHICH eq $g.WHICH` are true, and an alias
  (`my $h = $g`) is the same Seq rather than a forked copy of the coroutine.
- **Consumption.** `.head`, `.List`, `.iterator` and a consuming alias read
  consume the gather, so a second read throws `X::Seq::Consumed` as in Rakudo,
  while a subscript, `?$g` or `.cache` caches it and keeps it re-readable.
- **Resume after a prefix.** `$g[0]; $g.List` is `(1 2)`, not `(1)`: the prefix
  a subscript pulled is kept in the body and the coroutine resumes from there.

Laziness is kept where it mattered: an infinite gather inside a List, Pair or
Hash is never run at construction, `for` and `.iterator` still interleave with
the body, `.head`, `.first`, `[i]`, `.AT-POS` and `?` run it only as far as they
need, `.List`/`.list` hand back a List that keeps pulling, and the zip/cross
metaoperators, reductions and built-in routines stream a gather argument through
its coroutine.

Fixing the recompile of the gather body also fixed #13003: a `return` inside a
gather now returns from the routine that wrote it when that routine forces the
gather itself (`sub f { my @g = gather { take 1; return 42 }; 99 }` is `42`).
