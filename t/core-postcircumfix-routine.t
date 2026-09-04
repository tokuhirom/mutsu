use v6;
use Test;

# The subscript operators are ordinary CORE routines in Raku: callable by
# name (`postcircumfix:<[ ]>(@a, 1)`) and capturable as a term
# (`&postcircumfix:<[ ]>`). mutsu lowers `@a[...]` straight to the Index opcode
# family, so the operator used to exist only as syntax.
# See docs/adr/0041-sub-hoisting-vs-compile-time-name-visibility.md.

plan 12;

my @a = 10, 20, 30;
is postcircumfix:<[ ]>(@a, 1), 20, 'positional subscript routine called by name';
is postcircumfix:<[ ]>(@a, 2), 30, 'positional subscript routine, last element';
is postcircumfix:<[ ]>(@a, (0, 2)).join(','), '10,30', 'a slice index still slices';
is postcircumfix:<[ ]>(@a, *-1), 30, 'a Whatever index still counts from the end';

my %h = a => 1, b => 2;
is postcircumfix:<{ }>(%h, 'b'), 2, 'associative subscript routine called by name';

postcircumfix:<[ ]>(@a, 1, 99);
is @a[1], 99, 'the three-argument form stores';
postcircumfix:<{ }>(%h, 'c', 3);
is %h<c>, 3, 'the three-argument associative form stores';

my $pos = &postcircumfix:<[ ]>;
ok $pos ~~ Callable, '&postcircumfix:<[ ]> is a callable term';
is $pos(@a, 0), 10, 'the captured term indexes';
my $assoc = &postcircumfix:<{ }>;
is $assoc(%h, 'a'), 1, 'the captured associative term indexes';

# The one-argument form is the zen slice: the whole container.
is postcircumfix:<[ ]>(@a).join(','), @a.join(','), 'the one-argument form is the zen slice';
is postcircumfix:<{ }>(%h).elems, %h.elems, 'the one-argument associative form is the zen slice';
