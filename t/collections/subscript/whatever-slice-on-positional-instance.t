use Test;

plan 6;

# `$obj[*]` on an instance whose elements live in the backing
# `__mutsu_array_storage` (an `is Array` / `is List` subclass) or that serves
# them through its own `AT-POS` used to answer Nil: the instance subscript path
# resolved integer and list indices but had no `Whatever` arm. Every other
# subscript form already worked, and `[*]:exists` did the expansion too.

class MA is Array { }
class ML is List { }

my $m = MA.new('a', 'b', 'c');
is-deeply $m[*], ('a', 'b', 'c'), 'is Array subclass answers a whatever slice';
is $m[*].WHAT.^name, 'List', 'the whatever slice is a List';
is-deeply $m[1], 'b', 'a plain index still works';
is-deeply $m[0, 2], ('a', 'c'), 'a list index still works';

my $l = ML.new('x', 'y');
is-deeply $l[*], ('x', 'y'), 'is List subclass answers a whatever slice';

# NOTE: a bare `does Positional` class is deliberately out of scope. raku answers
# `Own.new(...)[*]` with a one-element list holding the object itself (mutsu still
# says Nil there), so the arm is restricted to storage-backed instances rather
# than widened to every Positional.

# An empty one yields the empty list, not Nil.
is-deeply MA.new()[*], (), 'an empty positional instance gives the empty list';
