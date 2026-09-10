use Test;

# Range positional protocol (AT-POS / EXISTS-POS / in-range) and .WHICH.
# All expected values verified against the reference raku implementation.

plan 20;

# .WHICH is the range's notation, not just its minimum
is (1..2).WHICH.Str, 'Range|1..2', '.WHICH of an inclusive range';
is (1^..^5).WHICH.Str, 'Range|1^..^5', '.WHICH of an exclusive-both range';
is (1..^3).WHICH.Str, 'Range|1..^3', '.WHICH of an exclusive-end range';
is ('a'..'z').WHICH.Str, 'Range|"a".."z"', '.WHICH of a string range quotes endpoints';
ok (1..2).WHICH eqv (1..2).WHICH, 'equal ranges have equal .WHICH';

# AT-POS: 0-based element access
is (1..4).AT-POS(0), 1, 'AT-POS(0) is the first element';
is (1..4).AT-POS(2), 3, 'AT-POS(2)';
is (1..4).AT-POS(3), 4, 'AT-POS(last)';
is (1..4).AT-POS(4), Nil, 'AT-POS past the end is Nil';
is (2^..8).AT-POS(0), 3, 'AT-POS on an exclusive-start range';
is ('a'..'e').AT-POS(2), 'c', 'AT-POS on a string range';
is (1..*).AT-POS(5), 6, 'AT-POS on an infinite range is arithmetic';

# EXISTS-POS: index existence
ok (6..10).EXISTS-POS(2), 'EXISTS-POS in bounds';
ok (6..10).EXISTS-POS(4), 'EXISTS-POS at the last index';
nok (6..10).EXISTS-POS(5), 'EXISTS-POS past the end';
nok (6..10).EXISTS-POS(-1), 'EXISTS-POS with a negative index';

# in-range: containment or throw
ok (1..10).in-range(5), 'in-range for an interior value';
ok ('a'..'z').in-range('c'), 'in-range on a string range';
throws-like { (1..10).in-range(50) }, X::OutOfRange,
    'in-range throws X::OutOfRange outside the range';
throws-like { (1^..10).in-range(1) }, X::OutOfRange,
    'in-range respects an excluded start';
