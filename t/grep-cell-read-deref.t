use Test;

# A `.grep` over an array now returns first-class element containers
# (`ContainerRef` cells aliasing the source slots) so a writeback loop mutates
# the source. Every *read* path over such a result — and over any `:=`-bound
# element cell — must decontainerize the cell before operating on it.

plan 17;

# .sort over a grep result orders the inner values, not the cells.
{
    my @a = 3, 1, 12, 2, 5;
    is-deeply @a.grep(* > 1).sort, (2, 3, 5, 12), '.sort over grep cells orders values';
    is-deeply @a.grep(* > 1).sort({ $^b <=> $^a }), (12, 5, 3, 2),
        '.sort(block) over grep cells';
}

# .sort over a `:=`-bound element cell (pre-existing read-deref path).
{
    my $x = 10;
    my @b = 1, 2, 3;
    @b[1] := $x;
    is-deeply @b.sort, (1, 3, 10), '.sort derefs a :=-bound element cell';
}

# .min / .max over grep cells.
{
    my @a = 5, 3, 8, 1, 9, 2, 7;
    is @a.grep(* > 3).min, 5, '.min over grep cells';
    is @a.grep(* > 3).max, 9, '.max over grep cells';
}

# .minmax over grep cells builds a value Range.
{
    my @x = 3, 1, 2;
    is-deeply @x.grep(* >= 1).minmax, (1..3), '.minmax over grep cells';
}

# .minmax over a `:=`-bound element cell.
{
    my $y = 10;
    my @b = 1, 2, 3;
    @b[1] := $y;
    is-deeply @b.minmax, (1..10), '.minmax derefs a :=-bound element cell';
}

# Other read paths over grep cells.
{
    my @a = 5, 3, 8, 1, 9, 2, 7;
    is @a.grep(* > 3).sum, 29, '.sum over grep cells';
    is @a.grep(* > 3).join(","), '5,8,9,7', '.join over grep cells';
    is-deeply @a.grep(* > 3).reverse, (7, 9, 8, 5), '.reverse over grep cells';
    is @a.grep(* > 3).first(* > 6), 8, '.first over grep cells';
    is-deeply @a.grep(* > 3).map(* + 1), (6, 9, 10, 8), '.map over grep cells';
    is @a.grep(* > 3)[2], 9, 'index into grep cells derefs';
}

# A read-only grep must not corrupt the source array.
{
    my @a = 1..6;
    @a.grep(* %% 2);             # promotes slots to cells, no writeback
    is-deeply @a, [1, 2, 3, 4, 5, 6], 'read-only grep leaves source intact';
}

# Stringifying a grep result whose cells wrap user Instances must call the
# user `.Str` (not the cell's gist) — for both arg-form and no-arg `.join`.
{
    my class P { has $.n; method Str { "P<$.n>" } }
    my @objs = P.new(n => 1), P.new(n => 2), P.new(n => 3);
    is @objs.grep({ .n > 1 }).join(","), 'P<2>,P<3>', '.join(sep) over grep Instance cells';
    is @objs.grep({ .n > 1 }).join, 'P<2>P<3>', '.join (no arg) over grep Instance cells';
    # `.map(*.Str)` exercises method dispatch on each cell.
    is-deeply @objs.grep({ .n > 1 }).map(*.Str), ('P<2>', 'P<3>'),
        '.map(method) over grep Instance cells';
}
