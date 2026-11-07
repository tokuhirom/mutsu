# A block whose last statement is a subscript hands back the element's container, as in
# rakudo: `.map({ %h<a> })` yields bindable element containers (#16919).
use Test;
plan 8;

my %h = a => 1;
my $r = (1,).map(-> $k { %h<a> });
my ($z is rw) := $r;
$z = 9;
is %h<a>, 9, 'a map block returning a hash element yields its container';

my $r2 = (1,).map(-> $k { if True { %h<a> } else { 0 } });
my ($w is rw) := $r2;
$w = 10;
is %h<a>, 10, 'the tail of an if branch is a container too';

sub f(\h) {
    my $items = h;
    my $res = ('a',).map(-> $a { if ($items{$a}:exists) { $items{$a} } else { Any } });
    return $res;
}
my ($v is rw) := f(%h);
$v = 11;
is %h<a>, 11, 'a block chosen through an exists test yields the element';

my @arr = 1, 2;
for (0,).map({ @arr[$_] }) -> $e is rw { $e = 33 }
is-deeply @arr, [33, 2], 'a for loop over mapped array elements writes through';

my @copy = (0, 1).map({ @arr[$_] });
@copy[0] = 99;
is-deeply @arr, [33, 2], 'assigning the mapped list into an array copies the values';
is-deeply @copy, [99, 2], 'the copy holds the values';

is-deeply (0, 1).map({ @arr[$_] + 1 }).List, (34, 3), 'arithmetic on mapped elements';
is-deeply (0, 1).map({ @arr[$_] }).List, (33, 2), 'mapped elements read as plain values';
