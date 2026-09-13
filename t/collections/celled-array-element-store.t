use Test;

# `@a[$i] = $v` where `@a` is `:=`-bound to another container, i.e. reached
# through a `ContainerCell` rather than held directly (issue #8307).
#
# That shape used to decline `try_fast_array_element_assign` outright -- env
# hands out a `ContainerRef` and the lane matched only `ValueView::Array` -- so
# every such store ran the full name-keyed cascade at ~3,800 ns against a plain
# array's ~780 ns, where rakudo charges 226 vs 224. The lane now descends one
# cell, which puts a bound container on the same footing as a plain one.
#
# These cases are the ones the lane must still refuse, or must get right while
# serving: writes visible through every alias, a by-value copy that detaches,
# element and container type constraints, autovivification past the end, a
# shaped array, an element that is itself a container, an immutable `List`, the
# array stored into its own element, and a three-name bind group. Every one of
# them passes under rakudo too.

plan 14;

# plain baseline
my @a = 0 xx 4;
@a[1] = 5;
is @a[1], 5, 'plain element store';

# `:=`-bound alias, both directions
my @b = 0 xx 4;
my @alias := @b;
@alias[0] = 9;
is @b[0], 9, 'store through the alias reaches the source';
@b[1] = 7;
is @alias[1], 7, 'store through the source reaches the alias';

# a copy taken after the binding must NOT see later writes
my @copy = @b;
@alias[2] = 3;
is @copy[2], 0, 'a by-value copy detaches';
is @b[2], 3, 'the source still sees the write';

# typed bound array keeps its constraint
my Int @t = 0 xx 4;
my @ta := @t;
@ta[0] = 5;
is @t[0], 5, 'typed bound array accepts a conforming store';
dies-ok { @ta[1] = "x" }, 'typed bound array still rejects a bad store';

# out-of-range store through a bound alias autovivifies
my @g = 0 xx 2;
my @ga := @g;
@ga[4] = 1;
is @g.elems, 5, 'out-of-range store through an alias extends the source';
is @g[4], 1, 'and lands in the right slot';

# shaped array must not take the lane
my @s[2;2];
my @sa := @s;
@sa[0;1] = 4;
is @s[0;1], 4, 'shaped bound array stores through both subscripts';

# an element that is itself a container is written THROUGH
my $cellv = 1;
my @h = 0 xx 3;
@h[0] := $cellv;
my @ha := @h;
@ha[0] = 42;
is $cellv, 42, 'a bound element is written through, not replaced';

# a List is immutable as a container
my @l := (1, 2, 3);
dies-ok { @l[0] = 9 }, 'a bound List refuses an element store';

# storing the container into its own element
my @circ = 0 xx 2;
my @circa := @circ;
@circa[0] = @circa;
is @circ[0].elems, 2, 'storing the array into its own element keeps it an array';

# three-deep binding chain
my @root = 0 xx 3;
my @mid := @root;
my @tail := @mid;
@tail[2] = 8;
is @root[2], 8, 'a three-name container bind group is closed';
