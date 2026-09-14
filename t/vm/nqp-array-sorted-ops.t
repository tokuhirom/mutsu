use v6;
use Test;
use nqp;

# Array::Sorted::Map 0.0.4 reaches these native operations through
# Array::Sorted::Util's binary-search midpoint calculation.
plan 15;

is nqp::div_i(7, 2), 3, 'nqp::div_i divides positive integers';
is nqp::div_i(7, -2), -4, 'nqp::div_i floors positive/negative division';
is nqp::div_i(-7, 2), -4, 'nqp::div_i floors negative/positive division';
is nqp::div_i(-7, -2), 3, 'nqp::div_i divides two negative integers';

my class NativeIntSubclass is Int { };
my $boxed = nqp::box_i(42, NativeIntSubclass);
is $boxed.^name, 'NativeIntSubclass', 'nqp::box_i preserves an Int subclass';
is $boxed, 42, 'nqp::box_i preserves the native integer payload';
ok nqp::istype($boxed, NativeIntSubclass), 'nqp::box_i result matches its target type';
is nqp::unbox_i($boxed), 42, 'nqp::unbox_i reads an Int subclass payload';
is nqp::cmp_i(2, 3), -1, 'nqp::cmp_i compares native integers';
is nqp::cmp_s('b', 'a'), 1, 'nqp::cmp_s compares native strings';
is nqp::iseq_s('same', 'same'), 1, 'nqp::iseq_s compares native strings';
is nqp::cmp_n(2.5e0, 2.5e0), 0, 'nqp::cmp_n compares native numbers';

my class SpliceIndex is Int { };
my @spliced = 'a', 'c';
@spliced.splice(nqp::box_i(1, SpliceIndex), 0, 'b');
is @spliced.join(','), 'a,b,c', 'an Int subclass boxed by nqp can index splice';

my @instants = Instant.from-posix(1), Instant.from-posix(2);
my $instant = @instants[1];
ok nqp::eqaddr(&infix:<cmp>($instant, @instants[1]), Order::Same),
    'cmp decontainerizes scalar and array-element operands';

my @also-values;
my @also = @also-values, 666;
my $reified := nqp::getattr(@also, List, '$!reified');
is nqp::elems($reified), 2, 'nqp::getattr exposes slurpy list storage';
