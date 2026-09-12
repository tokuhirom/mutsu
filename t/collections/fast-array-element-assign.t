use Test;

# `@a[$i] = $v` on a plain, untyped, unbound array with an in-range Int index
# is served by `try_fast_array_element_assign` (#8069 §2), which bypasses the
# full name-keyed store. This file pins that the fast lane agrees with the slow
# lane it short-circuits: every construct below either goes THROUGH the fast
# lane (and must still be right) or makes it decline (and must still reach the
# path that handles it).

plan 34;

# --- the fast lane itself -----------------------------------------------

my @a = 0 xx 4;
@a[2] = 7;
is @a[2], 7, 'plain in-range store lands';
is @a.elems, 4, 'plain store does not resize';
is @a.raku, '[0, 0, 7, 0]', 'the other slots are untouched';

# The assignment expression's rvalue is itemized: one positional index names one
# scalar slot, so an aggregate stored there contributes exactly ONE element in
# list context rather than flattening.
my @z = (@a[0] = [7, 8]), 2;
is @z.elems, 2, 'single-index store yields an itemized rvalue';
is @z.raku, '[[7, 8], 2]', 'the rvalue did not flatten into the list';

# An aggregate stored into an element itemizes (ADR-0040).
my @nest = 0 xx 2;
@nest[0] = [1, 2, 3];
is @nest[0].raku, '$[1, 2, 3]', 'an aggregate itemizes at the element store';
is @nest[0][1], 2, 'and is still subscriptable';

# Container identity (S3): an element write on a SHARED array is seen by every
# by-value holder of the same container.
my @src = 0 xx 3;
my @alias := @src;
@alias[1] = 42;
is @src[1], 42, 'in-place write reaches the := alias';
@src[2] = 43;
is @alias[2], 43, 'and the other way round';

sub touch(@arr) { @arr[0] = 99 }
my @passed = 0 xx 2;
touch(@passed);
is @passed[0], 99, 'a write through a parameter reaches the caller container';

# Raku `=` copy semantics still detach at COPY time, not at write time.
my @orig = 1, 2, 3;
my @copy = @orig;
@copy[0] = 9;
is @orig[0], 1, 'assigning a copy does not write through to the source';
is @copy[0], 9, 'and the copy itself changed';

# --- shapes the fast lane must decline ----------------------------------
#
# Two of its decline routes are pinned by the tests that caught them rather than
# duplicated here: a module routine's own `our @arr` must not resolve to the
# loading script's same-named array (t/modules/our-container-bare-name-resolution.t
# -- the write chokepoint's package-mirror root outranks the bare env key), and
# once a second mutator thread exists the store belongs to the ADR-0068 lanes and
# guard (t/concurrency/concurrent-lane-decline-routes.t).

# Autovivification past the end.
my @grow;
@grow[3] = 'x';
is @grow.elems, 4, 'a past-the-end store still autovivifies';
is @grow[3], 'x', 'and stores at the right index';
nok @grow[0]:exists, 'the skipped slots read as holes';

# Typed arrays keep their constraint check and their native fill.
my Int @typed = 1, 2, 3;
@typed[1] = 5;
is @typed[1], 5, 'a typed array still accepts a conforming value';
dies-ok { @typed[0] = 'nope' }, 'and still refuses a non-conforming one';

my int @native = 1, 2, 3;
@native[0] = 7;
is @native[0], 7, 'a native int array still stores';

# A shaped array keeps its bounds check.
my @shaped[2;2];
@shaped[1;1] = 'v';
is @shaped[1;1], 'v', 'a shaped array still stores multidimensionally';

# A `:=`-bound element is written THROUGH its cell, not replaced.
my $cell = 1;
my @bound = 0 xx 2;
@bound[0] := $cell;
@bound[0] = 5;
is $cell, 5, 'a :=-bound element writes through to its source';
is @bound[0], 5, 'and reads back the new value';

# A List is immutable as a container.
my $list = (1, 2, 3);
dies-ok { $list[0] = 9 }, 'a List element slot cannot be replaced';

# A `Nil` rvalue restores the container default rather than storing Nil.
my @nils = 1, 2, 3;
@nils[1] = Nil;
nok @nils[1].defined, 'assigning Nil leaves an undefined slot';

# `is default(...)` participates in the store.
my @defaulted is default(-1) = 1, 2, 3;
@defaulted[1] = Nil;
is @defaulted[1], -1, 'Nil restores the declared container default';

# A deleted index is resurrected by a later store.
my @del = 1, 2, 3;
@del[1]:delete;
nok @del[1]:exists, 'the index is gone after :delete';
@del[1] = 8;
ok @del[1]:exists, 'and a later store brings it back';
is @del[1], 8, 'with the stored value';

# A negative index still resolves from the end.
my @neg = 1, 2, 3;
@neg[*-1] = 30;
is @neg[2], 30, 'a *-1 subscript still addresses the last slot';

# A slice subscript distributes rather than storing one value.
my @slice = 0 xx 4;
@slice[1, 2] = 'a', 'b';
is @slice.raku, '[0, "a", "b", 0]', 'a slice subscript still distributes';

# A Range subscript likewise.
my @range = 0 xx 4;
@range[1..2] = 'p', 'q';
is @range.raku, '[0, "p", "q", 0]', 'a Range subscript still distributes';

# A `Whatever` subscript assigns across the whole array.
my @whatever = 0 xx 3;
@whatever[*] = 1, 2, 3;
is @whatever.raku, '[1, 2, 3]', 'a * subscript still assigns across';

# An itemized element ($-wrapped) is written through, not replaced.
my $held = 3;
my @items = List.new(1, 2, $held);
is @items[2], 3, 'an itemized element reads its value';

# A `Proxy` element mediates its own store.
my $behind = 0;
my @proxied = 0 xx 2;
@proxied[0] := Proxy.new(FETCH => -> $ { $behind }, STORE => -> $, $v { $behind = $v * 2 });
@proxied[0] = 5;
is $behind, 10, 'a Proxy element fires its STORE';

# The container the array lives in keeps its identity across a store.
my @ident = 0 xx 2;
my $before = @ident.WHICH;
@ident[0] = 1;
is @ident.WHICH, $before, 'the array keeps its identity across an element store';
