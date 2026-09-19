# `.grep`/`.map` over an array promote the elements they touch into
# rw-alias `ContainerRef` cells in place (an ordinary consequence of
# topicalizing `$_` as an alias to the source slot). A later
# `.sort({ $^a <=> $^b })` over that SAME array must still order correctly
# and use the inline numeric comparator fast path — not fall back to full
# `<=>` dispatch for every comparison just because some elements now sit in
# a container. (#8586's user-numeric-coercion fix used "is this a
# ContainerRef" as its inline-fast-path guard, which caught plain
# container-wrapped Ints too and forced the slow dispatch path for any array
# `.grep`/`.map` had ever touched -- an 8.5x instruction-count regression on
# `benchmarks/bench-array.raku`, since its sort runs right after a `.grep`
# over the same array.)
use Test;

plan 4;

my @arr = 1..20;
# Force element promotion: `.grep`'s block topicalizes `$_` as an alias to
# each source slot, promoting the matching elements to ContainerRef cells.
my @evens = @arr.grep(* %% 2);

is-deeply @arr.sort({ $^a <=> $^b }).List, (1..20).List,
    'sort by <=> after .grep still orders correctly with aliased elements';
is-deeply @arr.sort({ $^b <=> $^a }).List, (1..20).reverse.List,
    'reversed sort by <=> after .grep still orders correctly';

# A user Instance must still dispatch its real `<=>` (the original #8586 bug)
# even when it sits behind the very same kind of container promotion.
class SortableNumeric {
    has $.n;
    method Numeric { $!n }
    method Real { $!n }
}
my @objects = SortableNumeric.new(n => 3), SortableNumeric.new(n => 1),
    SortableNumeric.new(n => 2);
my @matched = @objects.grep({ .n > 0 });
is-deeply @objects.sort({ $^a <=> $^b })».n.List, (1, 2, 3),
    'user numeric coercion still dispatches after .grep aliases the array';
is-deeply @matched».n.sort.List, (1, 2, 3),
    'grep result itself is unaffected';
