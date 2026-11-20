use Test;

# A `sub EXPORT` that hands out an ordinary `&name` (not a postcircumfix
# operator) leaves element reads on their fast path; every read shape still
# answers what it did.

plan 14;

use lib 't/lib';
use ExportHookPlainCallable;

is shout('hi'), 'HI', 'the exported callable is installed';

my @a = 10, 20, 30;
is @a[1], 20, 'array element';
is @a[5].raku, 'Any', 'past the end of an Array is Any';
my %h = a => 1, b => 2;
is %h<b>, 2, 'hash element';
nok %h<zz>.defined, 'a missing key is undefined';
my $item = [7, 8];
is $item[0], 7, 'element of an itemized Array';
my $hi = { url => 'x' };
is $hi<url>, 'x', 'element of an itemized Hash';
my Int @typed = 1, 2;
is @typed[9].raku, 'Int', 'past the end of a typed Array is its type';
my @default is default(42);
is @default[3], 42, 'is default answers a miss';
is (1, 2, 3)[2], 3, 'element of a List';
is-deeply @a[0, 2], (10, 30), 'a slice';

sub nested() {
    my %memo = k => [1, 2];
    my sub probe($key) { %memo{$key}[1] }
    probe('k');
}
is nested(), 2, 'a captured outer hash read from a nested sub';

my @loop = 1 .. 5;
my $sum = 0;
$sum += @loop[$_] for ^5;
is $sum, 15, 'element reads in a loop';
is shout(@a[2].Str), '30', 'the callable and a subscript together';
