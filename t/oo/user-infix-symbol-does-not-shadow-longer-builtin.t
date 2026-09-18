use Test;

# A user-declared `infix:<...>` symbol operator must not claim a strict
# prefix of a longer BUILT-IN operator that shares its leading characters --
# the same way a declared `infix:<->` must not swallow the `-` of `->`.
# `==` and `===` are the concrete case: ecosystem `IP::Addr` declares
# `multi infix:<==>(IP::Addr:D, IP::Addr:D) is export { ... }`, and once that
# was in scope, EVERY later `===` in the importing file broke -- the parser
# greedily matched the declared 2-char `==` against the first two `=` of
# `===`, left a lone `=` behind, and died with "Confused. expected
# expression after infix operator". The single-character guards in
# `match_user_declared_infix_symbol_op` (pointy arrow, `++`/`--`, `+=`-style
# compound assignment) already protected 1-char ops; this pins the general
# fix that covers any length.

plan 3;

class Point {
    has $.x;
}

multi infix:<==>(Point:D $a, Point:D $b) is export {
    $a.x == $b.x
}

my $p1 = Point.new(x => 1);
my $p2 = Point.new(x => 1);

ok $p1 == $p2, 'the custom infix:<==> itself still works';

# The actual regression: `===` (identity) must still parse and run as the
# built-in three-character operator, not as the declared `==` followed by a
# stray `=`.
nok $p1.WHICH === $p2.WHICH, 'two distinct objects have different WHICH, via the real === operator';

my $p3 = $p1;
ok $p1.WHICH === $p3.WHICH, '=== still recognizes identity once === parses at all';
