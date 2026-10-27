use Test;

# `set` / `bag` / `mix` take `*@` arguments: non-itemized lists flatten
# recursively, an Array's own elements (itemized) stay whole. (Red's
# `set $ast.find-column-name` over a nested `(("a",), ("b",).Seq)` kept the two
# inner lists as elements, so `$lcols ∩ $rcols` was always empty and the
# optimizer's `AND` rules never fired: `t/06-optimizer`.)

plan 16;

is-deeply set((1, (2, 3))), set(1, 2, 3), 'set: nested list flattens';
is-deeply set((1, (2, (3, 4)))), set(1, 2, 3, 4), 'set: two levels';
is-deeply set((1, [2, 3])), set(1, 2, 3), 'set: an Array inside a List flattens';
is-deeply set(([1, 2], 3)), set(1, 2, 3), 'set: an Array at the head of a List';
is-deeply set((1, (2, 3).Seq)), set(1, 2, 3), 'set: a Seq inside a List';
is set([1, [2, 3]]).elems, 2, 'set: an Array keeps its elements whole';
is set((1, $(2, 3))).elems, 2, 'set: an itemized list stays one element';
is-deeply set(<a b>, <c d>), set(<a b c d>), 'set: several list arguments';

is-deeply bag((1, (1, 2))), bag(1, 1, 2), 'bag: nested list flattens';
is-deeply mix((1, (1, 2))), mix(1, 1, 2), 'mix: nested list flattens';
is bag([1, [1, 2]]).elems, 2, 'bag: an Array keeps its elements whole';

my sub names($x) { ("a",), ("b",).Seq }
is-deeply set(names(1)), set(<a b>), 'the nested result of a method is a flat Set';
is (set(names(1)) ∩ set(<a>)).elems, 1, '... which intersects as expected';

is-deeply set(1..3), set(1, 2, 3), 'set of a Range';
is-deeply set((1, 2), 3), set(1, 2, 3), 'set: mixed list and scalar';
is-deeply set(), set(), 'set of nothing';

done-testing;
