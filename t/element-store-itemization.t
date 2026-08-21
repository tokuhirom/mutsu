use v6;
use Test;

# ADR-0040: Array and Hash elements are itemized at the *store*, not
# compensated at the read (docs/adr/0040-array-hash-elements-are-itemized-
# at-the-store.md).
#
# This file is the acceptance oracle from ADR-0040 SS1.3 (the 25-row
# divergence matrix, measured on `main` 52631889f 2026-08-20) plus SS1.6's
# agreeing rows, the SS2 arity invariants, and the SS2 negative (non-)
# itemization list. Slice 1 (this PR) fixes the *mutation* sites only --
# element assign, autovivification, and push/unshift/append/prepend for a
# real Array/Hash -- so rows 19-22 (plus a few Slice-1-specific extras below
# them) turn green. Rows 01-18, 23, 24 stay `todo`-marked: they depend on
# itemizing at *construction* (list-assign, literal construction), which is
# Slice 2's job. Row 25 (and the SS1.6 rows) are invariants that must NOT
# move -- they are what stops a later slice from "fixing" the divergence by
# over-itemizing.

# === SS1.3 divergent rows (25) ===
# Rows 01-18, 23, 24 are read/construction-side and still diverge (Slice 2/3).

my @c = [<a b>],[<c d>];
my %h = a => [1,2];
sub takes(*@a) { @a.elems }

{
    todo 'row 01: construction-side itemization is ADR-0040 slice 2';
    is @c[0].raku, '$["a", "b"]', 'row 01: my @c = [<a b>],[<c d>]; @c[0].raku';
}
{
    todo 'row 02: construction-side itemization is ADR-0040 slice 2';
    is %h<a>.raku, '$[1, 2]', 'row 02: my %h = a => [1,2]; %h<a>.raku';
}
{
    todo 'row 03: construction-side itemization is ADR-0040 slice 2';
    is @c[0,1].raku, '($["a", "b"], $["c", "d"])', 'row 03: @c[0,1].raku';
}
{
    todo 'row 04: construction-side itemization is ADR-0040 slice 2';
    is @c.head.raku, '$["a", "b"]', 'row 04: @c.head.raku';
}
{
    todo 'row 05: construction-side itemization is ADR-0040 slice 2';
    is @c.tail.raku, '$["c", "d"]', 'row 05: @c.tail.raku';
}
{
    todo 'row 06: construction-side itemization is ADR-0040 slice 2';
    is @c.first(*.so).raku, '$["a", "b"]', 'row 06: @c.first(*.so).raku';
}
{
    todo 'row 07: construction-side itemization is ADR-0040 slice 2';
    is @c.sort.raku, '($["a", "b"], $["c", "d"]).Seq', 'row 07: @c.sort.raku';
}
{
    todo 'row 08: construction-side itemization is ADR-0040 slice 2';
    is @c.reverse.raku, '($["c", "d"], $["a", "b"]).Seq', 'row 08: @c.reverse.raku';
}
{
    todo 'row 09: construction-side itemization is ADR-0040 slice 2';
    is @c.map({$_}).raku, '($["a", "b"], $["c", "d"]).Seq', 'row 09: @c.map({$_}).raku';
}
{
    todo 'row 10: construction-side itemization is ADR-0040 slice 2';
    is @c.pairs[0].value.raku, '$["a", "b"]', 'row 10: @c.pairs[0].value.raku';
}
{
    todo 'row 11: construction-side itemization is ADR-0040 slice 2';
    is @c.Slip.raku, 'slip($["a", "b"], $["c", "d"])', 'row 11: @c.Slip.raku';
}
{
    todo 'row 12: construction-side itemization is ADR-0040 slice 2';
    is takes(@c[0]), 1, 'row 12: takes(@c[0])';
}
{
    todo 'row 13: construction-side itemization is ADR-0040 slice 2';
    is takes(%h<a>), 1, 'row 13: takes(%h<a>)';
}
{
    todo 'row 14: construction-side itemization is ADR-0040 slice 2';
    is takes(@c.head), 1, 'row 14: takes(@c.head)';
}
{
    todo 'row 15: construction-side itemization is ADR-0040 slice 2';
    my $n;
    for @c { $n = takes($_) }
    is $n, 1, 'row 15: for @c { takes($_) }';
}
{
    todo 'row 16: construction-side itemization is ADR-0040 slice 2';
    is (my @z = @c[0]).elems, 1, 'row 16: (my @z = @c[0]).elems';
}
{
    todo 'row 17: construction-side itemization is ADR-0040 slice 2';
    is [@c[0]].elems, 1, 'row 17: [@c[0]].elems';
}
{
    todo 'row 18: construction-side itemization is ADR-0040 slice 2';
    is join('|', @c[0]), 'a b', "row 18: join('|', \@c[0])";
}

# --- rows 19-22: the mutation sites (Slice 1, THIS PR) ---

{
    my @a = 1, 2;
    @a[0] = (7, 8);
    is @a[0].raku, '$(7, 8)', 'row 19: @a[0] = (7,8); @a[0].raku';
}
{
    my @a = 1, 2;
    @a.push([7, 8]);
    is @a[2].raku, '$[7, 8]', 'row 20: @a.push([7,8]); @a[2].raku';
}
{
    my @a;
    @a[3] = [7, 8];
    is @a[3].raku, '$[7, 8]', 'row 21: my @a; @a[3] = [7,8]; @a[3].raku';
}
{
    my @a;
    @a.append([7, 8], [9, 0]);
    is @a[0].raku, '$[7, 8]', 'row 22: @a.append([7,8],[9,0]); @a[0].raku';
    is @a.elems, 2, 'row 22: @a.append([7,8],[9,0]); @a.elems is 2';
}

{
    todo 'row 23: construction-side itemization is ADR-0040 slice 2';
    my @a23 = (1..3), (4..6);
    is takes(@a23[0]), 1, 'row 23: my @a = (1..3),(4..6); takes(@a[0])';
}
{
    todo 'row 24: .VAR reflection is ADR-0040 slice 3';
    my @l := 1, (1, 2), [3, 4];
    is "{@l[1].VAR.^name} {@l[2].VAR.^name}", 'List Array',
        'row 24: @l[1].VAR.^name, @l[2].VAR.^name';
}

# row 25: the invariant. An Array's OWN .raku de-itemizes its elements --
# this must NOT move, or a later slice has over-itemized.
is @c.raku, '[["a", "b"], ["c", "d"]]', 'row 25: @c.raku stays bare (invariant)';

# === SS1.6 agreeing rows (must stay agreeing) ===

{
    my $n1;
    for ((1, 2), (3, 4)) { $n1 = takes($_) }
    is $n1, 2, 'SS1.6: List literal source, implicit topic, stays 2';
}
{
    my $n2;
    for (([1, 2], [3, 4])) { $n2 = takes($_) }
    is $n2, 2, 'SS1.6: List of Array literals, implicit topic, stays 2';
}
{
    my $n3;
    for @c -> $v { $n3 = takes($v) }
    is $n3, 1, 'SS1.6: real Array source, pointy param, stays 1 (bind-side)';
}

# === Slice 1 extras: the other mutation sites this PR fixes ===

{
    my %h2;
    %h2<a> = [1, 2];
    is %h2<a>.raku, '$[1, 2]', 'slice 1: %h<a> = [1,2]; %h<a>.raku';
}
{
    my @u = 1, 2;
    @u.unshift([9, 9]);
    is @u[0].raku, '$[9, 9]', 'slice 1: @u.unshift([9,9]); @u[0].raku';
}
{
    my @p = 1, 2;
    @p.prepend([8, 8]);
    is @p[0].raku, '8', 'slice 1: @p.prepend([8,8]) flattens (one-arg rule)';
    is @p.elems, 4, 'slice 1: @p.prepend([8,8]) flattens to 4 elements';
}
{
    my @b2 = 1, 2;
    @b2.push(1, [2, 3]);
    is @b2[3].raku, '$[2, 3]', 'slice 1: multi-arg push itemizes only the aggregate';
}

# === arity invariants (must NOT change) ===

{
    my @p1;
    @p1.push(1, 2);
    is @p1.elems, 2, 'arity: push(1,2) is 2 elements';
}
{
    my @a2;
    @a2.append((5, 4));
    is @a2.elems, 2, 'arity: append((5,4)) is 2 elements';
}

# === SS2 negatives: must NOT be wrapped ===

{
    my @a3;
    @a3[0] = (a => 1);
    is @a3[0].raku, ':a(1)', 'negative: a Pair element stays unwrapped';
}
{
    my @a4;
    @a4[0] = Set.new(1, 2);
    is @a4[0].raku, 'Set.new(1,2)', 'negative: a Set element stays unwrapped';
}
{
    my @a5;
    @a5[0] = 5;
    is @a5[0].raku, '5', 'negative: an Int element stays unwrapped';
}

# === native-array safety (must not corrupt native storage) ===

{
    my int @n;
    @n.push(5);
    is @n[0], 5, 'native: int @n.push(5) stores a plain native Int';
}

# === further Slice 1 mutation sites: nested autovivification, Hash.push,
# and reference-shared push all itemize too ===

{
    # Nested autovivification: `@a[5][0] = 1` autovivifies `@a[5]` as a
    # fresh Array, and that freshly-stored intermediate element itemizes
    # like any other Array/Hash element store.
    my @a6;
    @a6[5][0] = 1;
    is @a6[5].raku, '$[1]', 'slice 1: @a[5][0] = 1; @a[5].raku';
}
{
    # Nested hash autovivification: `%h<a><b> = 1` autovivifies `%h<a>` as
    # a fresh Hash, itemized the same way.
    my %h4;
    %h4<a><b> = 1;
    is %h4<a>.raku, '${:b(1)}', 'slice 1: %h<a><b> = 1; %h<a>.raku';
}
{
    # Hash.push (the slow-path method dispatch, not the `%h<k> = v` opcode)
    # itemizes the pushed pair's value the same way.
    my %h3;
    %h3.push('a' => [1, 2]);
    is %h3<a>.raku, '$[1, 2]', 'slice 1: %h.push(pair) itemizes the pushed value';
}
{
    # Reference-shared push (`@a.push(@b)`, a NAMED array): raku's
    # non-flattening `**@values` slurpy stores the container itself, kept
    # live-aliased, AND itemized on the pushed element -- while `@b` read
    # directly stays bare (the two readers of the shared cell disagree on
    # itemization, matching raku exactly).
    my @b3 = (1, 2);
    my @a7;
    @a7.push(@b3);
    is @a7[0].raku, '$[1, 2]', 'slice 1: @a.push(@b) (reference-shared); @a[0].raku';
    is @b3.raku, '[1, 2]', 'slice 1: @b itself stays bare after the reference push';
    @b3.push(3);
    is @a7[0].raku, '$[1, 2, 3]', 'slice 1: mutating @b propagates through the shared cell';
}

done-testing;
