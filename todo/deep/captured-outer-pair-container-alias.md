# Captured outer variables snapshot in Pair values

**Design decided: see [ADR-0032](../../docs/adr/0032-wrapvarref-container-capture-across-closure-boundaries.md)**
(Proposed, 2026-08-19). It carries the root-cause analysis, the measured probe
table, the decision, the rejected alternatives and the implementation slices.
The remainder of this file is the original finding, kept for provenance.

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

## Status update (2026-08-19, re-measured on `fb54d5ce9`)

The exact repro above **now passes** — the named-sub half was fixed by the
`container_ref_capture_syms` / `needs_cell_named_sub_ref_slots` mechanism
(commit `3b9ead7c2`, pinned by `t/captured-outer-pair-container-alias.t`). The
residue is that the mechanism is keyed on the *reader being a directly nested
named sub*, so it never runs for a pointy block, an anonymous `sub {...}`, a
bare block, or a class method:

```raku
my $value = 1;
my $mk = -> { key => $value };   # or: class C { method mk() { key => $value } }
my $pair = $mk();
$value = 2;
say $pair.value;                 # raku: 2, mutsu: 1
```

It is also not a Pair bug: `\($value)` built inside a closure and
`$value.VAR.WHICH` compared across a closure boundary fail identically. ADR-0032
generalizes the capture edge to every closure kind.

## Design constraint

This is container-representation work, related to the element-cell design in
[ADR-0001](../../docs/adr/0001-gc-strategy-and-phasing.md). Do not fix it by
special-casing Pair construction or by snapshotting another copy. The eventual
mechanism must preserve the source container across closure boundaries for both
fat-arrow and `Pair.new` construction.
