use Test;

# The big-integer `+`/`-`/`*` paths borrow their operands instead of
# deep-cloning them (todo/perf/bigint-repeated-addition-performance-gap.md), and
# the VM fast-paths an Int/BigInt pair past the junction/coercion wrapper. Pin
# the semantics that shortcut must not change.

plan 25;

my $big = 10 ** 30;          # BigInt
my $big2 = 3 * 10 ** 30;

# Basic BigInt op BigInt / BigInt op Int, both operand orders.
is $big + $big, 2 * 10 ** 30, 'BigInt + BigInt';
is $big2 - $big, 2 * 10 ** 30, 'BigInt - BigInt';
is $big * $big, 10 ** 60, 'BigInt * BigInt';
is $big + 1, 1000000000000000000000000000001, 'BigInt + Int';
is 1 + $big, 1000000000000000000000000000001, 'Int + BigInt';
is $big - 1, 999999999999999999999999999999, 'BigInt - Int';
is 1 - $big, -999999999999999999999999999999, 'Int - BigInt';
is $big * 2, 2 * 10 ** 30, 'BigInt * Int';
is 2 * $big, 2 * 10 ** 30, 'Int * BigInt';

# The result must renormalise back to a plain Int when it fits.
is ($big - $big).WHAT.^name, 'Int', 'BigInt - BigInt narrowing to Int keeps type Int';
is $big - $big, 0, 'BigInt - BigInt narrows to 0';
is ($big + $big).WHAT.^name, 'Int', 'a big sum is still an Int';

# Mixed with non-integers: must fall through to the Rat/Num paths, not the
# borrowed big-integer shortcut.
# (spelled as an expression, not a `1000000000000000000000000000000.5`
# literal: mutsu drops such a literal's integer part -- see
# todo/tickets/decimal-literal-with-big-integer-part-loses-it.md)
is $big + 1/2, (2 * 10 ** 30 + 1) / 2, 'BigInt + Rat stays exact';
is ($big + 1/2).WHAT.^name, 'Rat', 'BigInt + Rat is a Rat';
is $big * (1/2), 5 * 10 ** 29, 'BigInt * Rat';
ok ($big + 0.5e0) ~~ Num, 'BigInt + Num is a Num';

# Junctions must still thread through the operators.
ok (($big | $big + 1) == $big), 'a junction operand still threads through +';
is ($big + (1 | 2)).WHAT.^name, 'Junction', 'BigInt + Junction stays a Junction';

# A user-declared infix must still override, even for big operands.
{
    my sub infix:<+>($a, $b) { 'overridden' }
    is $big + $big, 'overridden', 'a user infix:<+> overrides big-integer addition';
    my sub infix:<*>($a, $b) { 'mul-overridden' }
    is $big * $big, 'mul-overridden', 'a user infix:<*> overrides big-integer multiplication';
}
is $big + $big, 2 * 10 ** 30, 'the override is out of scope again';

# The growing-magnitude loop from the perf ticket produces raku's answer.
my ($prev, $current) = (1, 0);
for (0 .. 1000) {
    ($prev, $current) = ($current, $prev + $current);
}
is $current.chars, 209, 'fib(1001) has 209 digits';
is $current.substr(0, 20), '70330367711422815821', 'fib(1001) starts with raku digits';
is $current % 1000, 501, 'fib(1001) ends with raku digits';

# The same loop run downwards must land back on the seeds.
for (0 .. 1000) {
    ($prev, $current) = ($current - $prev, $prev);
}
is "$prev $current", '1 0', 'subtracting back down recovers the seeds';
