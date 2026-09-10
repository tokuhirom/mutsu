use v6;
use Test;

# The Unicode multiplicative operators `×` (U+00D7) and `÷` (U+00F7) have
# compound-assignment forms `×=` and `÷=`, mirroring `*=` and `/=`. mutsu
# recognised the bare infix operators but not their compound-assign forms:
# `$x ×= 4` mis-parsed the `×` as the infix operator and then failed on the `=`
# ("expression after multiplicative operator"). Seen in Pod::Contents
# (`$indent_level ×= .level - 1 when Pod::Item given $pod`).

plan 8;

my $x = 3;
$x ×= 4;
is $x, 12, '×= multiplies in place';

my $y = 20;
$y ÷= 4;
is $y, 5, '÷= divides in place';

# Chained with other compound assigns.
my $z = 2;
$z ×= 3;
$z += 1;
is $z, 7, '×= composes with += ';

# ÷= yields a Rat when it does not divide evenly (matches / semantics).
my $r = 5;
$r ÷= 2;
is $r, 2.5, '÷= yields a Rat for a non-integer quotient';

# With a statement modifier (the Pod::Contents shape).
my $lvl = 4;
$lvl ×= 3 when Int given 9;
is $lvl, 12, '×= works with a when/given statement modifier';

# The bare infix operators are unaffected.
is 6 × 7, 42, 'bare × still multiplies';
is 20 ÷ 4, 5, 'bare ÷ still divides';

# `×=` binds like an item assignment (tighter than comma).
my @a = 1, 2;
my $m = 3;
$m ×= 2, 99;
is $m, 6, '×= is item-assignment precedence (comma not absorbed)';

done-testing;
