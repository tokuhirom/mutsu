# A `Range` assigned to a named scalar is itemized, so it indexes instead of slicing

Closed 2026-09-01 by the TRIAGE regeneration's repro sweep: the ticket
(`todo/tickets/range-assigned-to-named-scalar-not-itemized-as-subscript.md`,
filed 2026-08-25, re-measured still-open on 2026-08-27 after ADR-0040 slice 2)
no longer reproduces on `main`. Something landed between 2026-08-27 and
2026-09-01 itemized the named-`$` scalar store for a `Range`; this entry pins
the behaviour rather than attributing it.

## What was wrong

```raku
my @n = <4 8 15 16 23 42>;
my $assigned = 1..3;
say @n[$assigned].raku;      # raku: IntStr.new(16, "16")   mutsu (then): (8, 15, 16)
say @n[my $ = 1..3].raku;    # both: IntStr.new(16, "16")   -- was already OK
say @n[$(1,2)].raku;         # both: IntStr.new(15, "15")   -- was already OK
```

mutsu kept a bare `Range` (no `ValueView::Scalar` wrapper) on the named-scalar
`=` store, so the subscript treated it as a slice selector instead of numifying
it (element count 3 -> index 3). The ticket had correctly placed the gap at the
**scalar** store, outside ADR-0040's array/hash-element scope.

## Now

All three lines print raku's answers. Pin:
`t/range-in-named-scalar-subscript-indexes.t`, which also keeps the bound
(`my $r := 1..3`) spelling slicing, as raku does.

`todo/deep/element-itemization-lost-in-scalar-binding.md` listed this ticket as
a blocked dependent; that list is now stale (see its 2026-09-01 note).
