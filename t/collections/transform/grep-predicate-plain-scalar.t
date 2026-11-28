use Test;

# A grep predicate that is a plain Bool, Int, Num or Str is answered by its
# own truthiness (#19323); these pin the answers, and those of the shapes
# that still take the full path.

plan 9;

is-deeply (0, 1, 2).grep({ $_ }), (1, 2), 'Int predicates';
is-deeply (0e0, 1.5e0).grep({ $_ }), (1.5e0,), 'Num predicates';
is-deeply ('', '0', 'a').grep({ $_ }), ('0', 'a'), 'Str predicates: only "" is false';
is-deeply (1, 2, 3).grep({ $_ > 1 }), (2, 3), 'Bool predicates';
is-deeply <0 1>.grep({ $_ }), (IntStr.new(1, '1'),), 'an allomorph boolifies as its number';
is-deeply (1, 2).grep({ 1.5 }), (1, 2), 'a Rat predicate';

class Never { method Bool { False } }
is-deeply (1, 2).grep({ Never.new }), (), 'an instance with its own Bool';
is-deeply (1, 2).grep({ 1 but False }), (), 'a mixin with its own Bool';

# A cell with an `is default` keeps it across the cycle collector's checks.
my @a is default(42) = 1, 2;
@a.grep({ $_ > 1 }).eager for ^3;
@a[0] = Nil;
is @a[0], 42, 'an element default survives repeated greps';
