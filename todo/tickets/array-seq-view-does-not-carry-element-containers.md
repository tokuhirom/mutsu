# `@a.Seq` does not carry the array's element containers, so `for @a.Seq { $_++ }` writes nothing back

Split out of `news/2026-08/take-rw-preserves-mutable-container-alias.md` on
2026-09-01, when that ticket was closed: it was the one live finding left in
the file.

## Repro

```raku
my @a = 1, 2, 3;
for @a.Seq { $_++ }
say @a;      # raku: [2 3 4]    mutsu: [1 2 3]

my @b = 1, 2, 3;
for @b.List { $_++ }
say @b;      # raku: dies X::Multi::NoMatch (List decontainerizes)    mutsu: [1 2 3], silently
```

Measured against raku v2026.06. In raku a `Seq` over an Array carries the
array's element containers (`my $s = @a.Seq; $s.List[0] =:= @a[0]` is `True`);
in mutsu it does not (`False`), so the loop topic is a copy and the increment
is lost.

## Where it belongs

This is ADR-0045 (`for` parameters bind the element container) seen from
another producer: the `.Seq` coercion. ADR-0045 slice 4 routed `.values`,
`.reverse` and `.sort` through `src/vm/vm_element_producers.rs`; `.Seq` is not
in `ELEMENT_PRODUCERS`. It is a useful second acceptance test for the
remaining ADR-0045 slices, and probably lands as one more producer arm rather
than as its own mechanism -- but check whether the `Seq`'s cached `.List` view
(`news/2026-08/seq-list-view-is-a-list-everywhere-and-the-eqv-routine-is-the-eqv-operator.md`)
preserves the cells before assuming that.

## Measurement caveat

Do not assert identity with `=:=` when verifying this: mutsu's `=:=` answers
`True` for two equal `Int`s held in distinct containers (`my $x = 1; my $z = 1;
my @g := (gather { take-rw $x }).List; say @g[0] =:= $z` -> mutsu `True`, raku
`False`). Assert by *mutating through the alias and reading the source*, as the
repro above does.
