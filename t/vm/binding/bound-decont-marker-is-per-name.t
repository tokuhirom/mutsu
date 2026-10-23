use v6;
use Test;

# A `:=` bind of a container to a scalar marks that name "bound decont": the
# scalar flattens into an `@` assignment instead of staying one item. The
# program-wide latch that records "some name has one" no longer sends every
# plain scalar store down the full path (#14774); the per-name marker, its
# clearing by a later store and its effect on other names are pinned here.

plan 9;

my @a = 1, 2, 3;
my $b := @a;
my @r = $b;
is @r.elems, 3, 'a scalar bound to an array flattens into an @ assignment';

my $plain = 7;
$plain = 8;
is $plain, 8, 'a plain scalar store after a bind elsewhere still stores';

my $other = (1, 2);
my @o = $other;
is @o.elems, 1, 'another scalar stays one item (it holds a List)';

$b := 5;
my @r2 = $b;
is @r2.elems, 1, 'rebinding the name to a non-container clears the marker';

my $n = 4;
$n = [1, 2, 3];
my @r3 = $n;
is @r3.elems, 1, 'a scalar assigned an Array stays one item';

my $m := [7, 8];
my @r4 = $m;
is @r4.elems, 2, 'a scalar bound to a fresh Array flattens';
$m := 9;
my @r5 = $m;
is @r5.elems, 1, 'and stops flattening once bound to an Int';

sub counter {
    my $codes := (1, 2, 3).List;
    my int $i = -1;
    my $sum = 0;
    while ++$i < $codes.elems { $sum += $codes[$i] }
    $sum
}
is counter(), 6, 'a loop counter next to a bound container';

my $s = 'a';
$s = 'b';
is $s, 'b', 'a string scalar store';
