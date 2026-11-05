use Test;

# Found via EC (secp256k1): `[+] (0, 0, 1) Z* (G, 2G, 4G)` folds user-defined
# `infix:<+>` candidates that take type objects (`Point:U`) as operands. The
# reduction used to skip the user candidate for a type-object operand and fold
# the objects as numbers.
plan 6;

class P { has $.v }
multi infix:<+>(P:U, P $b)      { $b }
multi infix:<+>(P:D $a, P:U)    { $a }
multi infix:<+>(P:D $a, P:D $b) { P.new(v => $a.v + $b.v) }
my $p = P.new(v => 4);

is (P + P).raku, 'P', 'the plain infix picks the user candidate';
is ([+] P, P).raku, 'P', 'reduction: two type objects';
is ([+] P, P, $p).raku, 'P.new(v => 4)', 'reduction: type objects, then an instance';
is ([+] $p, P, P).raku, 'P.new(v => 4)', 'reduction: an instance, then type objects';
is ([+] $p, $p).raku, 'P.new(v => 8)', 'reduction: two instances';
is ([+] (P, P)).raku, 'P', 'reduction over a list of type objects';

done-testing;
