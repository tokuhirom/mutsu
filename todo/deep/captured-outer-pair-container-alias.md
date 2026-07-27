# Captured outer variables snapshot in Pair values

## Divergence

Pair values built from a captured outer scalar snapshot its current value instead
of retaining the scalar's container:

```raku
my $value = 1;
sub make-pair() { key => $value }
my $pair = make-pair();
$value = 2;
say $pair.value;
```

The same residue applies to `Pair.new("key", $value)`. Local-slot sources already
retain their container, but a captured outer has no local slot for the existing
boxing mechanism to reuse.

## Design constraint

This is container-representation work, related to the element-cell design in
[ADR-0001](../../docs/adr/0001-gc-strategy-and-phasing.md). Do not fix it by
special-casing Pair construction or by snapshotting another copy. The eventual
mechanism must preserve the source container across closure boundaries for both
fat-arrow and `Pair.new` construction.
