# A hyper descends into an itemized list — and `DBIish` reaches 9/9

```raku
my $i = $(:a(1), :b(2), :c(3));
say ($i>>.key).raku;   # was ($("a", "b", "c"),) -- now ("a", "b", "c")
say $i>>.key.sort;     # was (((a b c)))        -- now (a b c)
```

mutsu treated the itemized list as **one** element and mapped over that, so the
result was a one-element list wrapping the original — and every downstream
operation then saw one thing. `.sort` on a one-element list is a no-op, which is
how `DBIish`'s `$installed>>.key.sort` came out unsorted.

## The wrong question

The per-element descend rule (`itemize_if_descended`, keyed off whether the
*source element* is `Iterable`) was right. The bug was one level up: both hyper
entry points took the target's elements with `runtime::value_to_list`, which is
**list-assignment flattening**. Under that rule an itemized list is deliberately
one element, and rightly so — `my @x = $(1, 2)` has one.

But itemization is a property of the container a value sits in, not of the list
`>>` is walking, so Rakudo hypers straight into it. The two questions are
different, and the hyper was asking the wrong one. It now asks
`hyper_source_items`, which takes an itemized list's own elements and defers to
`value_to_list` for everything else. Nested itemization is untouched: the
elements keep theirs, because `itemize_if_descended` restores it per element.

Pinned by `t/hyper-itemized-list.t`, which also pins the flattening semantics
that must *not* change (`my @flat = $pairs` still sees one element) and passes
identically under raku.

## `DBIish` is at raku parity on all nine files

```
FILE                       RAKU                     MUTSU
01-basic.rakutest          PASS (35/35)             PASS (35/35)
02-meta.rakutest           PASS (1/1)               PASS (1/1)
03-lib-util.rakutest       FAIL 1/5                 FAIL 1/5      (same subtest; no libpq)
05-mock.rakutest           PASS (16/16)             PASS (16/16)
06-types.rakutest          PASS (12/12)             PASS (12/12)
44-sqlite-memory.rakutest  FAIL 1/109 (# TODO)      PASS (109/109)
45-sqlite-common.rakutest  FAIL 1/109 (# TODO)      PASS (109/109)
46-sqlite-blob.rakutest    PASS (18/18)             PASS (18/18)
48-sqlite-errors.rakutest  PASS (17/17)             PASS (17/17)
```

It was 1/9 when the database battery slot was chosen and 8/9 as of 2026-07-26.
The ninth file — the `mysql` driver — took three fixes in a row, and only the
first was the one the ledger predicted:

1. [ADR-0015 P2](buf-repr-body-and-native-storage.md): `Blob` answers `VMArray`,
   carries a real `MVMArrayB` body, and hands C its own storage, so
   `NativeHelpers::Blob`'s `pointer-to` works.
2. [an enum value in a ternary's then-branch](ternary-then-branch-enum-value.md):
   `DBDish::mysql::StatementHandle` did not parse at all.
3. this one.

Only the first was a representation problem; the other two surfaced only by
re-running the file after each fix, which is the argument for re-measuring
rather than trusting a ledger row.

## Still open: the itemized-**Hash** twin

```raku
my $g = ${a => 1, b => 2};
say ($g>>.Str).raku;   # raku: {:a("1"), :b("2")}   mutsu: ("a\t1\nb\t2",)
```

A plain `%h>>.Str` is correct — the hyper has a Hash branch that maps over the
values and rebuilds a Hash from the keys. An *itemized* hash does not reach it:
the `ValueView::Hash` gate does not match, so it falls through and
`value_to_list` keeps it as one element. It needs the same treatment at the
`hash_keys` computation (which must keep the keys, so it cannot simply reuse
`hyper_source_items`), and working out why the gate misses is the first step.
Recorded in
[todo/tickets/hyper-on-itemized-hash.md](../../todo/tickets/hyper-on-itemized-hash.md).
