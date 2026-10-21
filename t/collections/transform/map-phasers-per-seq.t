use Test;

# A prefix read of a deferred `.map` pulls the source a bit at a time; the
# block's FIRST fires once for the Seq and its LAST only when a read sees the
# end, as under Rakudo. The block used to make the whole Seq pulled at once
# (#14196). Expected values are the `raku` (v2026.09) output.

plan 14;

my $j = 0;
my $m = (1..10).map({ FIRST { $j += 100 }; $j++; $_ });
is $m[2], 3, 'a prefix read of a map with a FIRST phaser';
is $j, 103, '... runs the callback three times and FIRST once';
is $m[4], 5, 'a later read of the same Seq';
is $j, 105, '... fires FIRST no second time';

my $l = 0;
my $m2 = (1..5).map({ LAST { $l = 99 }; $_ });
is $m2[1], 2, 'a prefix read of a map with a LAST phaser';
is $l, 0, '... does not fire LAST';
is $m2[4], 5, 'a read of the last element';
is $l, 0, '... does not fire LAST before the end is seen';
is $m2.elems, 5, 'elems reaches the end';
is $l, 99, '... which fires LAST';

my ($f, $g) = 0, 0;
my $m3 = (1..6).map(-> $a, $b { FIRST { $f++ }; LAST { $g++ }; $a + $b });
is $m3[0], 3, 'a two-parameter map with both phasers';
is "$f $g", "1 0", '... FIRST fired, LAST not yet';
is $m3.elems, 3, 'elems';
is "$f $g", "1 1", '... LAST fired once';
