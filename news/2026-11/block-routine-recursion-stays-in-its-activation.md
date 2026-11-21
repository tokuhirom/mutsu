# A loop body's recursive routine recurses in its own iteration

A `sub` declared in a loop body closes over that iteration's lexicals, and its
code object is captured per iteration (the routine's activation capture). The
capture bound the routine's sibling routines but not the routine itself. A
recursive call therefore resolved by name, reaching the registry's latest
iteration:

```raku
my @g;
for 3, 4 -> $v { my $q = $v; sub g($n) { $n <= 1 ?? $q !! $n * g($n - 1) }; @g.push(&g) }
say @g.map({ $_(3) });   # rakudo: (18 24)   mutsu: (24 24)
```

After a top-level loop the same call died with `Unknown function: g`.

The routine's own name is now bound through the same back-edge cell a sibling
cycle uses, so the recursion stays in the iteration. A routine that reads
nothing of the iteration is the same in every iteration and keeps resolving by
name. This is #18248's shape C. With the closure frame's caller walk switched
off, it also passes without that fallback.
