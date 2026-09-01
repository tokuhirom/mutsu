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
# Slices 1 (the direct array source with a writable aliasing parameter),
# 2 (`%h.values`) and 3 (the implicit topic, plus the plain named parameter's
# pure deletion) have landed, so every row they own is an ordinary passing test
# here. The rows still `todo`-marked name the slice that owns them: slice 4
# (derived producers — `.kv`/`.reverse`/`.sort`/`@$s`) and slice 5 (bind-time
# enforcement).

plan 102;

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
    is %h<a>, 2, 'row 08: escaping closure over a `%h.values` rw param writes through';
}

# row 16 — `.kv` is a derived producer (slice 4), consumed by a MULTI-parameter
# bind (slice 5): the value slot binds raw, so it aliases the source element.
{
    my @a = 10, 20;
    my @c;
    for @a.kv -> $i, $v is rw { @c.push(-> { $v = $v + 1 }) }
    @c[0]();
    @c[1]();
    is-deeply @a, [11, 21], 'row 16: escaping closure over a `.kv` rw param writes through';
}

# row 16's hash twin, and the direct write that the retired writeback used to
# carry (which must not regress when the writeback stops running).
{
    my %h = a => 1, b => 2;
    my @c;
    for %h.kv -> $k, $v is rw { @c.push(-> { $v = $v + 1 }) }
    @c[0]();
    @c[1]();
    is-deeply %h, {a => 2, b => 3}, 'row 16h: the same through `%h.kv`';

    my @d = 10, 20;
    for @d.kv -> $i, $v is rw { $v += $i }
    is-deeply @d, [10, 21], 'row 16: the direct write still lands';

    # The alias is live in BOTH directions inside the loop, like every other
    # promoted element.
    my @e = 10, 20;
    for @e.kv -> $i, $v is rw { @e[0] = 99 if $i == 0; $v = $v + 1 }
    is-deeply @e, [100, 21], 'row 16: a direct write to the element is seen through the alias';

    # A typed container rejects a bad element through the `.kv` alias too.
    dies-ok { my Int @f = 1, 2; for @f.kv -> $i, $v is rw { $v = "s" } },
        'row 16: a typed array constrains the `.kv` value slot';
}

# The raw multi-parameter bind is not `.kv`-specific: a chunked rw multi-param
# over a plain array aliases its elements too.
#
# The parameters are `$p`/`$q` on purpose. Naming them `$x` makes the Q6 Proxy
# rows below fail, and that is NOT this row's doing: a *plain* `my $x = 1`
# anywhere in the file does it too, on `main` as well. mutsu stores a `Proxy`
# assigned into an Array without FETCHing it (raku's `my @a = Proxy.new(...)`
# is `[5]`, mutsu's is `[Proxy]`) and compensates inside the loop; a same-named
# lexical disturbs the compensation and the Proxy's STORE fires. See
# todo/tickets/proxy-assigned-into-an-array-is-not-fetched.md.
{
    my @a = 1, 2, 3, 4;
    my $c;
    for @a -> $p is rw, $q is rw { $c = -> { $p = $p + 1 } if $p == 1 }
    @a[0] = 99;
    $c();
    is-deeply @a, [100, 2, 3, 4],
        'a chunked rw multi-param aliases the element it binds';
}

# row 44 — the implicit topic (slice 3).
{
    my @a = 1, 2;
    my @c;
    for @a { @c.push(-> { $_ = 99 }) }
    @c[0]();
    is-deeply @a, [99, 2], 'row 44: escaping closure over the implicit topic writes through';
}

# The topic sibling of row 08: an escaping closure over the topic of a
# `%h.values` loop (slice 2 x slice 3).
{
    my %h = a => 1;
    my @c;
    for %h.values { @c.push(-> { $_ = 99 }) }
    @c[0]();
    is %h<a>, 99, 'slice 2/3: escaping closure over a `%h.values` topic writes through';
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
    is $seen, 9, 'row 42: reading the implicit topic sees the body own direct element write';
}

# row 43 — a sigilless param over the topic-ish path (slice 3/4 wiring).
{
    my @a = 1, 2;
    my $seen;
    for @a -> \v { @a[0] = 9; $seen = v; last }
    is $seen, 9, 'row 43: reading a sigilless alias sees the body own direct element write';
}

# The topic sibling of rows 11/20 — ADR-0045 §5 Q1's pin. Slice 1 was protected
# from ADR-0027's capture freeze by accident: an `is rw` parameter never enters
# `loop_local_vars` (that set is gated on `!spec.is_rw`), so a promoted cell
# could not reach `compute_owned_captures`' unguarded primary branch or
# `freeze_readonly_owned_captures`. The topic is NOT gated that way, so this row
# is the one that would break first if the freeze ever value-froze a promoted
# cell: a READ-ONLY closure over the topic must still see later element writes.
{
    my @a = 1, 2;
    my @c;
    for @a { @c.push(-> { $_ }) }
    @a[0] = 9;
    is @c[0]() ~ " " ~ @c[1](), '9 2', 'Q1: a deferred read of the promoted topic sees a later write';
}

# The same, through a hash value.
{
    my %h = a => 1;
    my $seen;
    for %h.values -> $v is rw { %h<a> = 5; $seen = $v; last }
    is $seen, 5, 'slice 2: reading a `%h.values` rw alias sees a later element write';
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
    is-deeply @a, [11, 99], 'row 21: a direct `@a[1] = 99` in a topic body survives';
}

# row 22 — no `rw`, no closure: ordinary code losing an ordinary write.
{
    my @a = 10, 20;
    for @a -> $v { @a[1] = 99 }
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
    is-deeply @a, [11, 21], 'row 17: `.reverse` aliases the elements, not the mirror index';
}

# row 24
{
    my @a = 20, 10;
    for @a.sort -> $v is rw { $v = $v + 1 }
    is-deeply @a, [21, 11], 'row 24: `.sort` aliases the elements in sorted order';
}

# `Array.Seq` is another derived source. Like `.reverse` and `.sort`, it must
# yield the source element containers, while `.List` intentionally yields bare
# immutable values.
{
    my @a = 1, 2, 3;
    for @a.Seq { $_++ }
    is-deeply @a, [2, 3, 4], 'Array.Seq topic aliases the array element containers';
}
{
    my @a = 1, 2, 3;
    my @c;
    for @a.Seq -> $v is rw { @c.push(-> { $v++ }) }
    @c[0]();
    @c[1]();
    @c[2]();
    is-deeply @a, [2, 3, 4], 'an escaping Array.Seq rw alias writes through';
}
{
    my @a = 1, 2, 3;
    is @a.Seq.raku, '(1, 2, 3).Seq', 'Array.Seq still renders its element values';
}
{
    my @a = 1, [2, [3, [4, 5]]];
    @a.Seq.elems;
    is-deeply @a.flat(:hammer), (1, 2, 3, 4, 5),
        'flat decontainerizes elements after Array.Seq promotes them';
}

# row 39
{
    my $s = [1, 2];
    for @$s <-> $x { $x = $x + 1 }
    is-deeply $s, [2, 3], 'row 39: `for @$s <-> $x` aliases the inner array elements';
}

# The `$`-tagged source is captured at loop entry, not re-resolved by name each
# iteration: `for @$s` derefs `$s` once to pick the array it walks. The name is
# usually `$_` -- `encode($_) for @$_` is the idiomatic recursive structure walk
# -- and a nested loop rebinds the topic, so a by-name re-resolution aliased
# into whatever container the INNER loop was walking.
{
    my @leaves;
    my &walk = -> $v {
        with $v {
            if $_ ~~ Positional { walk($_) for @$_ }
            else                { @leaves.push($_) }
        }
    };
    walk([[0, 2], "x"]);
    is-deeply @leaves, [0, 2, "x"],
        'row 39c: a nested `for @$_` walk does not redirect the outer loop';
}
{
    my $s = [1, 2];
    for @$s <-> $x { $s = [9, 9]; $x = $x + 1 }
    is-deeply $s, [9, 9],
        'row 39d: reassigning the scalar mid-loop does not redirect the alias';
}

# The deferred-closure form of each derived producer. These are the rows that
# prove the alias is the ITEM the producer handed out, not an index the loop
# reconstructed: `.reverse` and `.sort` change the order, so anything assuming
# "item i came from index i" is wrong twice over for them.
{
    my $s = [1, 2];
    my @c;
    for @$s <-> $x { @c.push(-> { $x = $x + 10 }) }
    @c[0]();
    @c[1]();
    is-deeply $s, [11, 12], 'row 39b: an escaping closure over a `for @$s` alias writes through';
}
{
    my @a = 10, 20;
    my @c;
    for @a.reverse -> $v is rw { @c.push(-> { $v = $v + 1 }) }
    @c[0]();
    @c[1]();
    is-deeply @a, [11, 21], 'row 17b: an escaping closure over a `.reverse` alias writes through';
}
{
    my @a = 20, 10;
    my @c;
    for @a.sort -> $v is rw { @c.push(-> { $v = $v + 1 }) }
    @c[0]();
    @c[1]();
    is-deeply @a, [21, 11], 'row 24b: an escaping closure over a `.sort` alias writes through';
}
# `.reverse`/`.sort` must still READ as values everywhere else.
{
    my @a = 3, 1, 2;
    is @a.sort.raku, '(1, 2, 3).Seq', '.sort still renders values';
    is @a.reverse.raku, '(2, 1, 3).Seq', '.reverse still renders values';
    is-deeply @a, [3, 1, 2], 'and neither reorders the source';
}

# ---------------------------------------------------------------------------
# Class 5 — bind-time enforcement (slice 5).
# ---------------------------------------------------------------------------

# row 19
{
    dies-ok { for (1, 2) -> $v is rw { $v = 5 } }, 'row 19: `is rw` over a List dies at bind time';
}

# row 30
{
    dies-ok { for 1 .. 2 -> $v is rw { $v = 5 } }, 'row 30: `is rw` over a Range dies at bind time';
}

# rows 19/30, the rest of the bind-time rejection (ADR-0045 slice 5). The bind
# fails BEFORE the body runs, so an empty body dies just as an assigning one
# does, and the exception is raku's own `X::Parameter::RW` with its wording.
{
    dies-ok { for <a b> -> $v is rw { } },
        'row 19b: `is rw` over a word list dies even with an empty body';
    dies-ok { for (1, 2) <-> $v { } }, 'row 19c: `<->` over a List dies too';
    dies-ok { my %h = a => 1; for %h.keys -> $v is rw { } },
        'row 19d: `%h.keys` yields bare keys, so an `is rw` bind dies';
    dies-ok { my $a = 1; my $b = 3; for $a .. $b -> $v is rw { } },
        'row 30b: a Range built from variables is still immutable';

    my $err;
    try { for (1, 2) -> $v is rw { }; CATCH { default { $err = $_ } } };
    isa-ok $err, X::Parameter::RW, 'row 19: the bind failure is X::Parameter::RW';
    is $err.message,
        "Parameter '\$v' expects a writable container (variable) as an argument,\n"
        ~ "but got '1' (Int) as a value without a container.",
        'row 19: ... with raku\'s wording';
    is $err.symbol, '$v', 'row 19: .symbol names the parameter';
    is $err.got, 1, 'row 19: .got carries the offending item';
}

# The rejection is keyed on the SOURCE being provably bare, not on "mutsu did
# not promote this element": a producer ADR-0045 has not routed yet must keep
# its (currently lost) write rather than acquire a spurious death, and the
# forms that raku itself accepts must keep working.
{
    lives-ok { for (1, 2) -> $v is copy { $v = 5 } },
        'invariant: `is copy` over a List is fine -- it binds a fresh container';
    lives-ok { for () -> $v is rw { } },
        'invariant: an empty source binds nothing, so there is nothing to reject';
    lives-ok { for (1, 2) -> \v { my $x = v } },
        'invariant: a sigilless param binds a bare item and only dies on assignment';
    lives-ok { my @a = 1, 2; for flat(@a) -> $v is rw { $v = 9 } },
        'invariant: an unrouted producer over a real Array must not die';
    lives-ok { my @a = 1, 2; for @a[0, 1] -> $v is rw { $v = 9 } },
        'invariant: an array slice source must not die';
    lives-ok { my $x = 1; for $x -> $v is rw { $v = 5 } },
        'invariant: a scalar source is a container';
}

# row 28
{
    # Green since 2026-09-01: the promoted element cell carries its array's
    # `value_type` (news/2026-09/is-rw-bare-tail-returns-container.md).
    dies-ok { my Int @a = 1, 2; for @a -> $v is rw { $v = "s" } },
        'row 28: a typed array rejects a bad element through the alias';

    # ... and the write really is refused, not merely reported.
    my Int @a = 1, 2;
    try { for @a -> $v is rw { $v = "s" } };
    is-deeply @a, Array[Int].new(1, 2), 'row 28: the typed array is unchanged';

    my $err;
    try { my Int @b = 1, 2; for @b -> $v is rw { $v = "s" }; CATCH { default { $err = $_ } } };
    isa-ok $err, X::TypeCheck::Assignment, 'row 28: the failure is X::TypeCheck::Assignment';
    is $err.message,
        'Type check failed for an element of @b; expected Int but got Str ("s")',
        'row 28: ... and it blames the container, not the alias';

    # The topic form and a derived producer go through the same promoted cell.
    dies-ok { my Int @c = 1, 2; for @c { $_ = "s" } },
        'row 28: the implicit topic is constrained too';
    dies-ok { my Int @d = 1, 2; for @d.values -> $v is rw { $v = "s" } },
        'row 28: a `.values` producer hands out a constrained cell';
    dies-ok { my Int %h = a => 1; for %h.values -> $v is rw { $v = "s" } },
        'row 28: so does a typed hash';

    lives-ok { my Int @e = 1, 2; for @e -> $v is rw { $v = 9 } },
        'row 28: a well-typed write through the alias still lands';
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
# A container-sigil parameter binds the element's CONTAINER, and must keep
# doing so when the element is already a shared `ContainerRef` cell (the rw-alias
# cell `.grep` leaves behind, or a `:=`-bound element): it has to bind the
# container INSIDE the cell, not the cell. This was masked by the named
# parameter's writeback until slice 3 deleted it — the writeback re-stored the
# mutated binding over the element, hiding that the binding was never the
# container. `t/for-loop-cell-elements.t` is the other pin.
{
    my @c = [1, 2], [3, 4];
    @c.grep(*.so).elems;
    for @c -> @row { @row.push(8) }
    is-deeply @c, [[1, 2, 8], [3, 4, 8]],
        'slice 3: an `@`-sigil param binds through a ContainerRef element';
}
{
    my @c = [1, 2], [3, 4];
    @c.grep(*.so).elems;
    for @c -> $row { $row.push(8) }
    is-deeply @c, [[1, 2, 8], [3, 4, 8]],
        'slice 3: a `$`-sigil param binds through a ContainerRef element';
}
# The statement-modifier topic form of row 09.
{
    my %h = a => 1, b => 2;
    $_ = $_ * 10 for %h.values;
    is-deeply %h, {a => 10, b => 20}, 'slice 2: `$_ = X for %h.values` mutates in place';
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

# row 37 — nesting. Also the pin for the kind test in `array_is_aliasable`:
# ADR-0040 stores an `Array` element ITEMIZED, so `@row` here binds an
# `ItemArray`. An allow list of `Array | List` silently dropped that back onto
# the writeback, which rebuilds a fresh `ArrayData` and severs `@row` from the
# `@m` element it was sharing — invisible while the named parameter's writeback
# copied the severed array back, and a lost mutation the moment slice 3 deleted
# that writeback.
{
    my @m = [1, 2], [3, 4];
    for @m -> @row { for @row <-> $x { $x = $x * 10 } }
    is-deeply @m, [[10, 20], [30, 40]], 'row 37: a nested `<->` loop mutates the inner rows';
}
# The same severance, one level flatter: whatever the inner loop does to the
# bound row must leave the row still shared with its source element.
{
    my @m = [1, 2], [3, 4];
    for @m -> @row {
        for @row <-> $x { $x = $x * 10 }
        @row.push(9);
    }
    is-deeply @m, [[10, 20, 9], [30, 40, 9]],
        'row 37b: a bound row stays shared with its source element across an inner loop';
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

# row 27 — the binding also outlives the THREAD that captured it.
{
    my @a = 10, 20;
    my @p;
    for @a -> $v is rw { @p.push(start { $v = $v + 1 }) }
    await @p;
    is-deeply @a, [11, 21], 'row 27: a `start` block over an `is rw` param writes through';
}

done-testing;
