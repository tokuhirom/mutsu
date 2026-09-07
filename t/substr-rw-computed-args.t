use Test;

plan 12;

# `$s.substr-rw($from, $len) = $v` — the method's own arguments are ordinary
# rvalues. They reach the lvalue-assignment builtin inside an array carrier
# whose elements are `Scalar` containers, so a *variable* argument used to
# arrive as a container while a literal one arrived bare; the length arm then
# fell to its "no length given" default and replaced the whole tail.

my $p = 1;
my $l = 3;

my $a = "hello";
$a.substr-rw(1, 3) = "Z";
is $a, "hZo", "literal from, literal length";

my $b = "hello";
$b.substr-rw($p, 3) = "Z";
is $b, "hZo", "variable from, literal length";

my $c = "hello";
$c.substr-rw(1, $l) = "Z";
is $c, "hZo", "literal from, variable length";

my $d = "hello";
$d.substr-rw($p, $l) = "Z";
is $d, "hZo", "variable from, variable length";

my Int $li = 3;
my $e = "hello";
$e.substr-rw(1, $li) = "Z";
is $e, "hZo", "Int-typed variable length";

my $f = "hello";
$f.substr-rw($p, $l) = "";
is $f, "ho", "empty replacement still honours the length";

my %h = (pos => 1, len => 3);
my $g = "hello";
$g.substr-rw(%h<pos>, %h<len>) = "ZZ";
is $g, "hZZo", "hash-element from and length";

my @idx = 1, 3;
my $i = "hello";
$i.substr-rw(@idx[0], @idx[1]) = "ZZ";
is $i, "hZZo", "array-element from and length";

# A `$`-held Range is itemized, so it arrives wrapped in a Scalar. Both the
# rvalue `substr` and the `substr-rw` lvalue must still see a Range.
my $rng = 1..3;
is "abcdef".substr($rng), "bcd", "substr with a Range held in a scalar";
is "abcdef".substr(1..3), "bcd", "substr with a literal Range";

my $j = "abcdef";
$j.substr-rw($rng) = "XY";
is $j, "aXYef", "substr-rw with a Range held in a scalar";

my $k = "abcdef";
$k.substr-rw(1..3) = "XY";
is $k, "aXYef", "substr-rw with a literal Range";
