use v6;
use Test;

# ADR-0045: a `for` loop parameter binds the element *container*.
#
# This file pins the whole semantic surface of that ADR — both halves, on
# purpose:
#
#   * the DIVERGENCE table (ADR-0045 §1.3), so each slice can un-`todo` exactly
#     the rows it claims; and
#   * the INVARIANT table (same section), the rows that already agree with raku
#     today. The invariant half is what stops a later slice from "fixing" a
#     divergence by over-promoting — e.g. by making the plain named parameter
#     `-> $v` alias (rows 45/46 say it must not), or by cascading a promoted
#     cell across iterations (rows 34/35, ADR-0027's per-iteration freeze).
#
# Slice 1 (the direct array source with a writable aliasing parameter) has
# landed, so rows 01, 02, 03, 04, 07, 11, 12, 13, 14, 20, 27, 36 and 41 are
# ordinary passing tests here. The rows still `todo`-marked name the slice that
# owns them: slice 2 (hash sources), slice 3 (the implicit topic and the plain
# named param), slice 4 (derived producers) and slice 5 (bind-time enforcement).

plan 57;

# ---------------------------------------------------------------------------
# Class 1 — a binding that outlives the loop body still writes through.
# ---------------------------------------------------------------------------

# row 01
{
    my @a = 10, 20;
    my @c;
    for @a -> $v is rw { @c.push(-> { $v = $v + 1 }) }
    @c[0]();
    @c[1]();
    is-deeply @a, [11, 21], 'row 01: escaping closure over an `is rw` param writes through';
}

# row 02
{
    my @a = 10, 20;
    my @c;
    for @a <-> $v { @c.push(-> { $v = $v + 1 }) }
    @c[0]();
    @c[1]();
    is-deeply @a, [11, 21], 'row 02: escaping closure over a `<->` param writes through';
}

# row 03
{
    my @a = 10, 20;
    my @c;
    for @a -> \v { @c.push(-> { v = v + 1 }) }
    @c[0]();
    @c[1]();
    is-deeply @a, [11, 21], 'row 03: escaping closure over a sigilless param writes through';
}

# row 12 — two closures pushed in the SAME iteration share one cell.
{
    my @a = 10;
    my @c;
    for @a -> $v is rw {
        @c.push(-> { $v = $v + 1 });
        @c.push(-> { $v = $v + 1 });
    }
    @c[0]();
    @c[1]();
    is-deeply @a, [12], 'row 12: sibling closures in one iteration share the element cell';
}

# row 13 — called inside the loop AND after it.
{
    my @a = 10, 20;
    my @c;
    for @a -> $v is rw {
        my $f = -> { $v = $v + 1 };
        @c.push($f);
        $f();
    }
    @c[0]();
    @c[1]();
    is-deeply @a, [12, 22], 'row 13: in-loop and post-loop calls both write through';
}

# row 14 — a whole-value rebind through the escaping closure.
{
    my @a = [1, 2], [3, 4];
    my @c;
    for @a -> $v is rw { @c.push(-> { $v = [9] }) }
    @c[0]();
    is-deeply @a[0], [9], 'row 14: rebinding the alias to a fresh container writes through';
}

# row 27 lives at the very END of this file, not here: a `start` block anywhere
# in a program corrupts every LATER `for @m -> @row { ... }` loop, which would
# otherwise sink the `-> @row` invariant rows 32 and 37 below. That is a
# pre-existing, unrelated bug — see
# `todo/tickets/at-sigil-for-param-rebinds-stale-container-after-start-block.md`.

# row 36 — one closure writes, a sibling closure reads the new value.
{
    my @a = 1;
    my @c;
    for @a -> $v is rw {
        @c.push(-> { $v = 9 });
        @c.push(-> { $v });
    }
    @c[0]();
    is @c[1](), 9, 'row 36: a sibling closure reads what its sibling wrote';
}

# row 08 — the hash source (slice 2).
{
    my %h = a => 1;
    my @c;
    for %h.values -> $v is rw { @c.push(-> { $v = $v + 1 }) }
    @c[0]();
    todo 'ADR-0045 slice 2: hash sources still use write_back_hash_value_item';
    is %h<a>, 2, 'row 08: escaping closure over a `%h.values` rw param writes through';
}

# row 16 — `.kv` is a derived producer (slice 4).
{
    my @a = 10, 20;
    my @c;
    for @a.kv -> $i, $v is rw { @c.push(-> { $v = $v + 1 }) }
    @c[0]();
    @c[1]();
    todo 'ADR-0045 slice 4: .kv must hand out element containers';
    is-deeply @a, [11, 21], 'row 16: escaping closure over a `.kv` rw param writes through';
}

# row 44 — the implicit topic (slice 3).
{
    my @a = 1, 2;
    my @c;
    for @a { @c.push(-> { $_ = 99 }) }
    @c[0]();
    todo 'ADR-0045 slice 3: the implicit topic must bind the element container';
    is-deeply @a, [99, 2], 'row 44: escaping closure over the implicit topic writes through';
}

# ---------------------------------------------------------------------------
# Class 2 — an aliasing binding READS later writes to the element.
# ---------------------------------------------------------------------------

# row 11
{
    my @a = 10, 20;
    my @c;
    for @a -> $v is rw { @c.push(-> { $v }) }
    @a[0] = 5;
    is @c[0](), 5, 'row 11: a deferred read through an `is rw` alias sees a later element write';
}

# row 20
{
    my @a = 10, 20;
    my @c;
    for @a -> $v is rw { @c.push(-> { $v }) }
    @a[0] = 5;
    @a[1] = 6;
    is-deeply (@c[0](), @c[1]()).List, (5, 6), 'row 20: every deferred read sees its later element write';
}

# row 41 — an in-body read after an in-body direct element write.
{
    my @a = 1, 2;
    my $seen;
    for @a -> $v is rw { @a[0] = 9; $seen = $v; last }
    is $seen, 9, 'row 41: reading the `is rw` alias sees the body own direct element write';
}

# row 42 — the implicit topic (slice 3).
{
    my @a = 1, 2;
    my $seen;
    for @a { @a[0] = 9; $seen = $_; last }
    todo 'ADR-0045 slice 3: the implicit topic must bind the element container';
    is $seen, 9, 'row 42: reading the implicit topic sees the body own direct element write';
}

# row 43 — a sigilless param over the topic-ish path (slice 3/4 wiring).
{
    my @a = 1, 2;
    my $seen;
    for @a -> \v { @a[0] = 9; $seen = v; last }
    is $seen, 9, 'row 43: reading a sigilless alias sees the body own direct element write';
}

# ---------------------------------------------------------------------------
# Class 3 — the end-of-iteration snapshot must not clobber the body's own
# direct writes to the source.
# ---------------------------------------------------------------------------

# row 04
{
    my @a = 10, 20;
    for @a -> $v is rw { $v = $v + 1; @a[1] = 99 }
    is-deeply @a, [11, 99], 'row 04: a direct `@a[1] = 99` in an `is rw` body survives';
}

# row 21 — implicit topic (slice 3).
{
    my @a = 10, 20;
    for @a { $_ = $_ + 1; @a[1] = 99 }
    todo 'ADR-0045 slice 3: the topic writeback still clobbers direct element writes';
    is-deeply @a, [11, 99], 'row 21: a direct `@a[1] = 99` in a topic body survives';
}

# row 22 — no `rw`, no closure: ordinary code losing an ordinary write.
{
    my @a = 10, 20;
    for @a -> $v { @a[1] = 99 }
    todo 'ADR-0045 slice 3: the named-param writeback is a pure deletion';
    is-deeply @a, [10, 99], 'row 22: a plain `-> $v` body direct element write survives';
}

# row 38 — the body rebinds the source wholesale.
{
    my @a = 1, 2;
    for @a -> $v is rw { $v = 9; @a = 7, 8 }
    is-deeply @a, [7, 8], 'row 38: rebinding the source array wholesale is not overwritten';
}

# row 07 — the iteration ends by dying, but the write already landed.
{
    my @a = 10, 20;
    try { for @a -> $v is rw { $v = 77; die "stop" } }
    is-deeply @a, [77, 20], 'row 07: a write through the alias survives an exception';
}

# ---------------------------------------------------------------------------
# Class 4 — derived producers must alias in the derived order (slice 4).
# ---------------------------------------------------------------------------

# row 17
{
    my @a = 10, 20;
    for @a.reverse -> $v is rw { $v = $v + 1 }
    todo 'ADR-0045 slice 4: .reverse writes to the mirror-image index';
    is-deeply @a, [11, 21], 'row 17: `.reverse` aliases the elements, not the mirror index';
}

# row 24
{
    my @a = 20, 10;
    for @a.sort -> $v is rw { $v = $v + 1 }
    todo 'ADR-0045 slice 4: .sort has no index mapping to reconstruct';
    is-deeply @a, [21, 11], 'row 24: `.sort` aliases the elements in sorted order';
}

# row 39
{
    my $s = [1, 2];
    for @$s <-> $x { $x = $x + 1 }
    todo 'ADR-0045 slice 4: `@$s` must hand out element containers';
    is-deeply $s, [2, 3], 'row 39: `for @$s <-> $x` aliases the inner array elements';
}

# ---------------------------------------------------------------------------
# Class 5 — bind-time enforcement (slice 5).
# ---------------------------------------------------------------------------

# row 19
{
    todo 'ADR-0045 slice 5: an immutable source must reject an `is rw` bind';
    dies-ok { for (1, 2) -> $v is rw { $v = 5 } }, 'row 19: `is rw` over a List dies at bind time';
}

# row 30
{
    todo 'ADR-0045 slice 5: an immutable source must reject an `is rw` bind';
    dies-ok { for 1 .. 2 -> $v is rw { $v = 5 } }, 'row 30: `is rw` over a Range dies at bind time';
}

# row 28
{
    todo 'ADR-0045 slice 5 / ADR-0042: the promoted cell must carry the element constraint';
    dies-ok { my Int @a = 1, 2; for @a -> $v is rw { $v = "s" } },
        'row 28: a typed array rejects a bad element through the alias';
}

# ---------------------------------------------------------------------------
# INVARIANTS — these agree with raku today and must keep agreeing. A later
# slice that "fixes" a divergence by over-promoting breaks one of these.
# ---------------------------------------------------------------------------

# rows 05 / 06 — loop control after mutating the alias.
{
    my @a = 10, 20;
    for @a -> $v is rw { $v = 77; last }
    is-deeply @a, [77, 20], 'row 05: `last` after mutating the alias keeps the write';
}
{
    my @a = 10, 20;
    for @a -> $v is rw { $v = 77; next }
    is-deeply @a, [77, 77], 'row 06: `next` after mutating the alias keeps every write';
}

# row 09 — direct `%h.values` mutation (the writeback path slice 2 replaces).
{
    my %h = a => 1, b => 2;
    for %h.values -> $v is rw { $v *= 10 }
    is-deeply %h, {a => 10, b => 20}, 'row 09: `%h.values -> $v is rw` mutates in place';
}

# rows 10 / 32 / 33 — a plain named param still mutates the container it binds.
{
    my @m = [1, 2], [3, 4];
    for @m -> $row { $row.push(9) }
    is-deeply @m, [[1, 2, 9], [3, 4, 9]], 'row 10: `-> $row` in-place container mutation propagates';
}
{
    my @m = [1, 2], [3, 4];
    for @m -> @row { @row.push(9) }
    is-deeply @m, [[1, 2, 9], [3, 4, 9]], 'row 32: `-> @row` in-place container mutation propagates';
}
{
    my %h = a => [1, 2];
    for %h.values -> $v { $v.push(9) }
    is-deeply %h<a>, [1, 2, 9], 'row 33: `%h.values -> $v` in-place container mutation propagates';
}

# row 15 — `.kv` with a direct rw write.
{
    my @a = 10, 20;
    for @a.kv -> $i, $v is rw { $v += $i }
    is-deeply @a, [10, 21], 'row 15: `.kv -> $i, $v is rw` writes each element';
}

# rows 23 / 26 — `.values` and a shaped array, direct.
{
    my @a = 10, 20;
    for @a.values -> $v is rw { $v = $v + 1 }
    is-deeply @a, [11, 21], 'row 23: `.values -> $v is rw` mutates the source';
}
{
    my @a[2];
    @a[0] = 10;
    @a[1] = 20;
    for @a -> $v is rw { $v = $v + 1 }
    is @a[0] ~ "," ~ @a[1], '11,21', 'row 26: a shaped array `is rw` loop mutates the source';
}

# row 25 — mutate, then `last` partway.
{
    my @a = 10, 20;
    for @a -> $v is rw { $v = $v + 1; last if $v > 20 }
    is-deeply @a, [11, 21], 'row 25: mutations before a partway `last` all land';
}

# row 29 — a Seq source accepts an `is rw` param (no bind-time die).
{
    my @a = 1, 2;
    lives-ok { for @a.map({ $_ }) -> $v is rw { $v = 5 } },
        'row 29: an `is rw` param over a Seq source does not die';
}

# row 31 — the plain named param cannot be assigned at all.
{
    my @a = 1, 2;
    dies-ok { for @a -> $v { $v = 5 } }, 'row 31: `-> $v` is read-only';
}

# rows 34 / 35 — ADR-0027 per-iteration capture identity. Each closure must
# keep ITS OWN iteration's binding; a promoted cell must not cascade across
# iterations. This pulls in the opposite direction from rows 12/36 above (which
# require sharing WITHIN one iteration) — together they pin the behaviour.
{
    my @c;
    for 1, 2, 3 -> $x { @c.push(-> { $x }) }
    is-deeply @c.map({ .() }).List, (1, 2, 3), 'row 34: pointy closures keep per-iteration identity';
}
{
    my @c;
    for 1, 2, 3 -> $x { @c.push(sub { $x }) }
    is-deeply @c.map({ .() }).List, (1, 2, 3), 'row 35: named-sub closures keep per-iteration identity';
}
# The same, over a real mutable Array source (the shape slice 1 promotes).
{
    my @a = 1, 2, 3;
    my @c;
    for @a -> $x is rw { @c.push(-> { $x }) }
    is-deeply @c.map({ .() }).List, (1, 2, 3),
        'row 34b: an `is rw` alias over an Array keeps per-iteration identity';
}

# row 37 — nesting.
{
    my @m = [1, 2], [3, 4];
    for @m -> @row { for @row <-> $x { $x = $x * 10 } }
    is-deeply @m, [[10, 20], [30, 40]], 'row 37: a nested `<->` loop mutates the inner rows';
}

# row 40 — the promoted cell stays invisible to reflection.
{
    my @a = 1, 2;
    for @a -> $v is rw { $v = $v + 1 }
    is @a.raku, '[2, 3]', 'row 40: `.raku` does not reveal the promoted element cell';
    is @a.elems, 2, 'row 40: `.elems` is unaffected by element promotion';
    is @a.WHAT.^name, 'Array', 'row 40: the source stays an Array';
    is @a[0].WHAT.^name, 'Int', 'row 40: an element reads back decontainerized';
    is-deeply @a.List, (2, 3), 'row 40: list context decontainerizes every element';
    is "@a[]", '2 3', 'row 40: interpolation decontainerizes every element';
}

# rows 45 / 46 — the plain named param must NOT alias, in either direction.
{
    my @a = 1, 2;
    my $seen;
    for @a -> $v { @a[0] = 9; $seen = $v; last }
    is $seen, 1, 'row 45: `-> $v` does not read-alias the element';
}
{
    my @a = 1, 2;
    my @c;
    for @a -> $v { @c.push(-> { $v }) }
    @a[0] = 9;
    is @c[0]() ~ " " ~ @c[1](), '1 2', 'row 46: a deferred read of `-> $v` does not alias';
}

# ADR-0045 §5 Q6 — a `Proxy` element under an aliasing loop. Assigning a Proxy
# into an Array FETCHes it, so the element is a plain value and the loop must
# write the ARRAY, never the Proxy's STORE target.
{
    my $n = 5;
    my @a = Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v });
    for @a -> $x is rw { $x = 42 }
    is $n, 5, 'Q6: an `is rw` loop over a FETCHed Proxy element leaves the Proxy target alone';
    is @a[0], 42, 'Q6: ... and writes the array element';
}

# ADR-0045 §5 Q1 — `box_captured_lexicals` must not double-box an already-cell
# param: the closure's read must decontainerize exactly once.
{
    my @a = 7;
    my @c;
    for @a -> $v is rw { @c.push(-> { $v + 1 }) }
    is @c[0](), 8, 'Q1: a closure over a promoted element param reads a plain value';
    is @a[0].WHAT.^name, 'Int', 'Q1: the source element still reads back as an Int';
}

# ---------------------------------------------------------------------------
# ADR-0045 §1.5 — the mutating `<->` loop must be O(n), not O(n^2). Before
# slice 1 every iteration rebuilt the whole backing ArrayData to replace one
# element, so 20 000 elements took ~1.1 s release / far longer debug. The bound
# is generous (this file runs on the DEBUG binary in CI) but still fails
# decisively on a quadratic regression.
# ---------------------------------------------------------------------------
{
    my @big = ^20000;
    my $t0 = now;
    for @big <-> $x { $x = $x + 1 }
    my $elapsed = now - $t0;
    ok $elapsed < 20, "mutating `<->` over 20k elements is O(n) ({$elapsed.round(0.01)}s)";
    is @big[19999], 20000, 'the mutating `<->` loop actually wrote every element';
}

# row 27 — the binding also outlives the THREAD that captured it. Deliberately
# last: see the note where this row would otherwise sit.
{
    my @a = 10, 20;
    my @p;
    for @a -> $v is rw { @p.push(start { $v = $v + 1 }) }
    await @p;
    is-deeply @a, [11, 21], 'row 27: a `start` block over an `is rw` param writes through';
}

done-testing;
