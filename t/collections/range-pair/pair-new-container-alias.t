use v6;
use Test;

plan 8;

# traps.rakudoc: Pair.new binds its positional value parameter raw, so the
# built Pair's value aliases the source scalar's container (write-through).
# Only the 2-positional form aliases; the named form, the key, and a frozen
# Pair decontainerize.

my $v = 1;
my $p = Pair.new("a", $v);
$v = 9;
is $p.value, 9, 'Pair.new positional value reflects later mutation of the source scalar';

my $w = 1;
my $q = Pair.new("a", $w);
$q.value = 7;
is $w, 7, 'assigning to .value writes through to the source scalar';

my $m = 2;
is Pair.new("b", $m).value.VAR.^name, 'Scalar', '.value is a Scalar container';

my $x = 1;
my $r = Pair.new(key => "a", value => $x);
$x = 9;
is $r.value, 1, 'named-form value does not alias';

my $k = "x";
my $s = Pair.new($k, 1);
$k = "y";
is $s.key, "x", 'key does not alias';

my $u = 1;
my $t = Pair.new("a", $u).freeze;
$u = 9;
is $t, 1, 'freeze decontainerizes the value';

# fat-arrow parity: the same capture mechanism backs `key => $var`
my $f = 1;
my $fp = (a => $f);
$f = 4;
is $fp.value, 4, 'fat-arrow pair value aliases (parity check)';

# a Pair built in a fresh block scope each iteration stays independent
my @ps = (1..3).map({ my $n = $_; Pair.new("k", $n) });
is-deeply @ps.map(*.value).list, (1, 2, 3), 'per-iteration Pairs keep distinct values';
