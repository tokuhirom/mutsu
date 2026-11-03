use Test;
use NativeCall;

# CArray[T] is a role-mixed class instance: every element store/fetch goes
# through a role multi method on a mixin receiver. The memoized `.WHAT` and
# borrowed role lookups on that path (#16215) must not change the results.

plan 9;

my $n = CArray[num64].new;
$n[$_] = $_ * 1.5e0 for ^5;
is $n[4], 6e0, 'num64 store/fetch round trip';
is $n.elems, 5, 'elems after stores';
$n[2] = 100e0;
is $n[2], 100e0, 'overwrite an element';

my $i = CArray[int32].new;
$i[$_] = $_ * 3 for ^4;
is $i[3], 9, 'int32 store/fetch round trip';
isnt $i.WHAT.^name, $n.WHAT.^name, 'different compositions keep distinct .WHAT';
is $n.WHAT.^name, CArray[num64].^name, 'instance .WHAT matches the parameterized type';

# Repeated stores to one instance and to a second instance of the same
# composition stay independent.
my $m = CArray[num64].new;
$m[0] = 1e0;
$n[0] = 2e0;
is $m[0], 1e0, 'a second instance of the same composition is independent';
is $n[0], 2e0, 'the first instance keeps its own value';
is $m.WHAT.^name, $n.WHAT.^name, 'same composition shares one .WHAT';
