use Test;

# An allomorph (IntStr/RatStr/NumStr) used as a Range endpoint coerces to its
# numeric value, so `0..<5>` is the range `0..5`, not a degenerate one-element
# range over the un-numified Mixin.

plan 10;

is (0..<5>).elems,   6,           'IntStr as exclusive-of-nothing end endpoint';
is (0..^<5>).elems,  5,           'IntStr as excluded end endpoint';
is (<5>..10).elems,  6,           'IntStr as start endpoint';
is (<2>..<5>).list,  (2, 3, 4, 5), 'IntStr on both ends';
is (<5e0>..10).elems, 6,          'NumStr endpoint coerces numerically';
is (<3.0>..6).elems, 4,           'RatStr endpoint coerces numerically';

# the range actually iterates to the right values
is (<1>..<3>).join(','), '1,2,3', 'allomorph range iterates correctly';

# subscripting with an allomorph range
my @a = 'a'..'j';
is @a[0..^<3>].join, 'abc',       'allomorph range as a subscript';

# plain numeric ranges are unaffected
is (1..3).elems, 3,               'plain Int range unchanged';
is (0..^5).elems, 5,              'plain excluded range unchanged';
