use Test;

# #9505: a subscript call argument's `is rw` snapshot/writeback temps are
# stored with dedicated call-temp opcodes instead of the general by-name
# variable store. The observable writeback behaviour must not change.

plan 9;

sub bump($x is rw) { $x++ }
sub plain($n, $b) { $n }
sub keep($x is rw) { $x }

my @a = 1, 2, 3;
bump(@a[1]);
is-deeply @a, [1, 3, 3], 'is rw param writes back into an array element';

my %h = a => 10;
bump(%h<a>);
is %h<a>, 11, 'is rw param writes back into a hash element';

my @b = 0 xx 5;
bump(@b[$_]) for ^5;
bump(@b[$_]) for ^3;
is-deeply @b, [2, 2, 2, 1, 1], 'writeback in a loop hits each iteration element';

my @c = 1..5;
my $sum = 0;
$sum += plain(@c[$_], 0) for ^5;
is $sum, 15, 'a plain param reads the element value';
is-deeply @c, [1, 2, 3, 4, 5], 'a plain param never writes back';

my @d = 7, 8;
keep(@d[0]);
is-deeply @d, [7, 8], 'an unchanged is rw param leaves the element alone';

my @e = 1, 2, 3;
sub modify-outer($x is rw) { @e[0] = 99; $x }
modify-outer(@e[0]);
is @e[0], 99, 'a callee writing the slot directly is not undone by the writeback';

sub rec(@xs, $depth) { $depth == 0 ?? @xs[0] !! rec(@xs, $depth - 1) + plain(@xs[$depth], 0) }
is rec([1, 2, 3, 4], 3), 10, 'recursion through a call site with a subscript argument';

sub lv($x is rw) is rw { $x }
my @f = 1, 2;
lv(@f[1]) = 5;
is-deeply @f, [1, 5], 'an is rw routine returning its is rw param assigns through';
