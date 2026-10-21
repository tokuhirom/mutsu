use Test;

# A signature-bearing map block's LAST phaser fires with the final call's
# arguments once a read sees the end of the Seq, also when that read runs no
# further callback because an earlier prefix read already consumed the last
# element (#14196). Expected values are the `raku` (v2026.09) output.

plan 8;

my @log;
my $m = (1..6).map(-> $a, $b { FIRST { @log.push("F") }; LAST { @log.push("L:$a,$b") }; $a + $b });
is $m[0], 3, 'a prefix read of a two-parameter map';
is-deeply @log, ["F"], '... fires FIRST only';
is $m.elems, 3, 'elems reaches the end';
is-deeply @log, ["F", "L:5,6"], '... which fires LAST with the final call\'s arguments';

my @l2;
my $n = (1..4).map(-> $a, $b { LAST { @l2.push("L:$a,$b") }; $a * $b });
$n.elems;
$n.elems;
is-deeply @l2, ["L:3,4"], 'a second read of an ended Seq fires LAST no second time';

my @l3;
my $c = (1..6).map({ $_ }).map(-> $a, $b { LAST { @l3.push("L:$a,$b") }; $a + $b });
is $c[2], 11, 'a chained two-parameter map, read up to its last element';
is-deeply @l3, [], '... has not fired LAST';
$c.elems;
is-deeply @l3, ["L:5,6"], '... until a read sees the end';
