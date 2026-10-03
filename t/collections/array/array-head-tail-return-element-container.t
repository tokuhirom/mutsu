use Test;

# An Array's argless `.head` / `.tail` hand back the element itself (its
# container), as rakudo's do: `$_ = ... with @a.tail` and `my $x := @a.head`
# write into the array. Reduced from Version::Nginx, whose
# `$_ = .Int + 1 with @parts.tail` bumps the last version component.

plan 11;

my @parts = "1.2".split('.');
$_ = .Int + 1 with @parts.tail;
is-deeply @parts, [ "1", 3 ], 'with @a.tail aliases the last element';

my @b = 1, 2, 3;
$_ = 7 with @b.head;
is-deeply @b, [7, 2, 3], 'with @a.head aliases the first element';

given @b.tail { $_ = 9 }
is-deeply @b, [7, 2, 9], 'given @a.tail aliases it too';

my $x := @b.tail;
$x = 8;
is-deeply @b, [7, 2, 8], 'binding @a.tail aliases the element';

my $copy = @b.head;
$copy = 0;
is-deeply @b, [7, 2, 8], 'assigning @a.head copies the value';
is @b.head, 7, '.head still reads the value';
is @b.tail, 8, '.tail still reads the value';
isa-ok @b.tail, Int, '...of the element type';

my @empty;
is-deeply @empty.head, Nil, '.head of an empty Array is Nil';
is-deeply @empty.tail, Nil, '.tail of an empty Array is Nil';

my @l := (1, 2, 3);
dies-ok { $_ = 5 with @l.tail }, 'a List element stays immutable';

