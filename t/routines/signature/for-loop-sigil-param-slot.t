use Test;

# A pointy `for` parameter with an `@`, `%` or no sigil (`\x`) is a local
# slot of the loop body, like a `$` one (ADR-12529). Binding, writes through
# it, nested shadows and closures behave as before, and a whole-container
# assignment through an `@`/`%` parameter keeps the bound container's own
# type. Every expectation was checked against rakudo.

plan 13;

my @r;
for 1, 2 -> \x { @r.push: x * 10 }
is-deeply @r, [10, 20], 'a sigilless parameter';

@r = ();
for [1, 2], [3] -> @a { @r.push: @a.elems }
is-deeply @r, [2, 1], 'an array parameter';

@r = ();
for {a => 1}, {b => 2, c => 3} -> %h { @r.push: %h.elems }
is-deeply @r, [1, 2], 'a hash parameter';

my @c;
for 1, 2 -> \y { @c.push: { y } }
is-deeply @c.map({ .() }).List, (1, 2), 'closures over a sigilless parameter';

@r = ();
for [[1, 2],] -> @a { { my @a = 9; @r.push: @a[0] }; @r.push: @a[0] }
is-deeply @r, [9, 1], "a nested block's `my @a` shadows the array parameter";

my @w = 1, 2;
for @w -> \v { v = v * 5 }
is-deeply @w, [5, 10], 'a write through a sigilless parameter reaches the element';

my @b = [1], [2];
for @b -> @x { @x.push: 0 }
is-deeply @b, [[1, 0], [2, 0]], 'a push through an array parameter reaches the element';

my %h = a => 1;
for %h.values -> \v { v++ }
is-deeply %h, {a => 2}, 'an increment through a sigilless parameter over .values';

my @outer = 7;
for [1], [2] -> @outer { }
is-deeply @outer, [7], 'the enclosing same-named array is untouched after the loop';

my %ao{Any} = a => 1;
my %reset{Any} = b => 2;
for (%ao,) -> %a { %a = %reset; is %a.WHAT.raku, 'Hash[Any,Any]', 'an assigned object-hash parameter keeps its type' }
is %ao.raku, '(my Any %{Any} = :b(2))', 'and the assignment reached the bound hash';

my Int @ints = 1, 2;
for (@ints,) -> @a { @a = 3, 4; is @a.WHAT.raku, 'Array[Int]', 'an assigned typed-array parameter keeps its type' }
is-deeply @ints, Array[Int].new(3, 4), 'and the assignment reached the bound array';
