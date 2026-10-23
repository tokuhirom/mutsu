use v6;
use Test;

# `++`/`--` of a plain slot-held Int steps the slot directly (#14370). Every
# shape the generic by-name path treats specially must still take it.

plan 25;

my $n = 5;
is $n++, 5, 'postfix ++ returns the old value';
is $n, 6, '... and steps the variable';
is ++$n, 7, 'prefix ++ returns the new value';
is $n--, 7, 'postfix -- returns the old value';
is --$n, 5, 'prefix -- returns the new value';

my $big = 9223372036854775807;
$big++;
is $big, 9223372036854775808, '++ past the i64 range promotes to a bigger Int';
my $small = -9223372036854775808;
$small--;
is $small, -9223372036854775809, '-- below the i64 range promotes too';

my $s = 'a9';
$s++;
is $s, 'b0', 'a string slot takes the string increment';

my $u;
$u++;
is $u, 1, 'an undefined scalar increments from zero';

my Int $typed = 3;
$typed++;
is $typed, 4, 'a typed Int scalar steps';
my int $native = 9223372036854775807;
$native++;
is $native, -9223372036854775808, 'a native int wraps';

sub ro($x) { $x++ }
throws-like { ro(1) }, X::Multi::NoMatch, 'a readonly parameter still refuses ++';
sub copy($x is copy) { $x++; $x }
is copy(1), 2, 'an `is copy` parameter steps';

my $count = 0;
my &bump = { $count++ };
bump() for ^3;
is $count, 3, 'a closure over the variable shares the step';

my $c2 = 0;
for ^1000 { $c2++ }
is $c2, 1000, 'a hot loop counts';

my $x = 10;
my @r;
for ^3 { @r.push($x++) }
is-deeply @r, [10, 11, 12], 'post-increment as an expression value';

sub counter { state $k = 0; $k++; $k }
counter() for ^4;
is counter(), 5, 'a state variable steps';

our $pkg = 1;
$pkg++;
is $pkg, 2, 'an our variable steps';

my $y = 1;
{
    my $y = 100;
    $y++;
    is $y, 101, 'an inner variable steps without touching the outer one';
}
is $y, 1, 'the outer variable is untouched';

my int $ni = 5;
is $ni++, 5, 'a native int slot: postfix ++ returns the old value';
is ++$ni, 7, '... prefix ++ the new value';
my int32 $n32 = 2147483647;
$n32++;
is $n32, -2147483648, 'an int32 slot wraps at its own width';
my uint8 $u8 = 255;
$u8++;
is $u8, 0, 'a uint8 slot wraps to zero';
my int $nk = 0;
$nk++ for ^1000;
is $nk, 1000, 'a native int counter in a hot loop';
