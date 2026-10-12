# A named sub in a loop body keeps its own iteration's lexicals

A `sub` declared in a loop body or a `.map` callback now closes over the
block activation that declared it, as rakudo's `capturelex` does:

```raku
my @r = (1, 2).map(-> $v { my $q = $v; sub g { $q }; &g });
say @r.map({ $_() });   # (1 2) -- was (2 2)
```

Such a sub used to read its free variables through stores keyed by the
sub's name, so every `&g` taken in an earlier iteration answered with the
latest iteration's `$q`. The code object a `&g` read builds in the declaring
code now holds that activation's cells, and its frame links to them through
the static link of ADR-12529 phase 3, ahead of the per-name stores. Writes
through the sub, later writes in the block, closures created inside the sub
and the TRIR fast path all follow the same activation (#12692).
