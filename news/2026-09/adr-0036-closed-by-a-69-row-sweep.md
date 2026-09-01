# ADR-0036 closes, and its sweep pays for itself again

[ADR-0036](../../docs/adr/0036-element-container-pairs-from-subscripts-and-pairs.md)
— "a Pair produced by a subscript adverb or `.pairs` carries the element
*container*, not a snapshot" — is fully implemented. Slice 5 is the sweep, and
its job was to prove that rather than assume it.

## The table it was written from

All twelve rows of the ADR's §1.3 divergence table now reach raku's answer, as
do the two `.VAR.^name` probes from §1.1. Each was run as its own one-line
program with raku's and mutsu's `stdout`+`stderr` compared verbatim, so the rows
that die are checked on their message rather than merely on dying.

## The rows it was not written from

After ADR-0045 slice 6, the standing advice in `todo/TRIAGE.md` is that an ADR's
own divergence matrix cannot see what its mechanism does off the rows it was
written from. So the sweep went to **69 rows** across the whole surface this ADR
owns: subscript adverbs on arrays and hashes (single, slice, out-of-range,
`:exists`/`:delete`), every producer and its `for`-loop consumption, immutable
sources, element type constraints, 1-D and multi-dimensional shaped arrays,
QuantHash weights, the "a Pair value is read as DATA" rule from slice 3, and —
the one nobody had written a row for — subscripting a producer's result.

Twelve rows differ. **None is a regression**: every one was re-run against `main`
before the slice-4 branch and behaves identically there. They fall into three
groups, two of which were already on file:

- **Five are the blame *name* only.** The class, the type and the check are
  right; mutsu says `an element of @` where raku says `an element of @a`.
- **One is the QuantHash weight arm's keying**, in its sharpest form yet:
  `for $b.pairs -> $p { $p.value = 5 }` dies where `for $b.pairs { .value = 5 }`
  works. The two programs differ only in the loop's parameter form —
  `topic_source_var` is set when the loop binds the *topic*, and `-> $p` binds a
  named parameter instead.
- **Five are one new finding**, and three of those are silent no-ops.

## The new one

Subscripting an element producer's `Seq` drops the container:

```raku
my @a = <A B>;
say (@a.values)[0].VAR.^name;   # raku: Scalar    mutsu: Str
(@a.values)[0] = "x"; say @a;   # raku: [x B]     mutsu: [A B]   silent
(@a.kv)[1]     = "x"; say @a;   # raku: [x B]     mutsu: [A B]   silent
my $c := (@a.values)[0]; $c = "x";  # raku: [x B] mutsu: [A B]   silent
```

The producer is fine — `for @a.values -> $v is rw` writes through, and
`.head` keeps the cell. The consumer is fine — `@a.pairs[0].value` keeps it. It
is the positional subscript in between: `exec_index_op_with_positional`
normalizes a `Seq` receiver to an `ArrayKind::List` array, and the read then goes
through `resolve_array_entry`, the decontainerization chokepoint.

That is ADR-0036 §6's blast-radius consequence seen from the other side. The
chokepoint that stops a cell leaking into `.raku`/`.WHAT` also stops it reaching
the one caller that wants it, and telling those apart needs the `Seq` to record
that its items *are* element containers — which no slice of this ADR gives it.
So it is `todo/tickets/producer-seq-index-read-decontainerizes-the-element-cell.md`,
not a slice 6.

## A blocker attribution corrected on the way out

`todo/deep/immutable-lvalues-that-mutsu-still-lets-you-assign-to.md` has been
blocked on this ADR since 2026-08-27, when ADR-0040 was ruled out as its blocker
and ADR-0036 named in its place. Its seven-row harness was re-run against the
finished ADR: **no row moved.** The remaining rows are an immutable-container
check on the element *store* and the closure-call topic marking, and neither of
those is a pair producer. The file says so now, so the next reader does not
re-derive the same wrong attribution a third time.
