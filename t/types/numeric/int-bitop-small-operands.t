use Test;

# `+&`, `+|` and `+^` on two plain small Ints answer without the operand
# dispatch; every other shape (a big Int, a Str, a Failure, a user infix)
# must still answer as before.

plan 12;

is 255 +& 15, 15, '+& on small Ints';
is 8 +| 3, 11, '+| on small Ints';
is 6 +^ 3, 5, '+^ on small Ints';
is -1 +& 0xFF, 255, 'a negative operand';
is (-9223372036854775807 - 1) +| 1, -9223372036854775807, 'the most negative Int';

my $s = 0;
for ^1000 -> $i { $s += $i +& 7 }
is $s, 3500, 'in a loop';

is 2**70 +& (2**70 + 5), 2**70, 'big Int operands';
is '12' +& 4, 4, 'a numeric Str operand';
throws-like { my $x = 'abc' +& 1; $x.Int }, X::Str::Numeric, 'a non-numeric Str fails';
is 3 +& 1.9, 1, 'a Rat operand coerces';

{
    sub infix:<+&>($a, $b) { "custom $a $b" }
    is 6 +& 3, 'custom 6 3', 'a user infix:<+&> wins';
}
is 6 +& 3, 2, 'outside its scope the core op answers';
