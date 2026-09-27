# A `constant` bound to a *value*, written where a parameter's type goes, is a
# value constraint: rakudo compiles `multi f(G)` to the value's type plus a
# smartmatch against it (`:(Point $ where { ... })`), which for a definite
# object is `===` -- WHICH identity. The Bitcoin distribution's `secp256k1`
# special-cases its generator point this way
# (`multi infix:<*>(Int $n where 1 < $n < 2**256, G)`, using a precomputed
# table), and mutsu used to reject the name as an invalid typename, then rank
# the candidate below a plain `Point:D` one and cache its verdict per type.
use Test;

plan 16;

class Point { has $.v }
constant G = Point.new(v => 1);

multi foo(G) { 'G' }
multi foo(Point:D $p) { 'Point' }
is foo(G), 'G', 'the constant candidate wins for the constant itself';
is foo(Point.new(v => 1)), 'Point', 'an equal but distinct object is not the constant';
is foo(G), 'G', 'and the verdict is not cached per argument type';

multi bar(G) { 'G' }
multi bar($) { 'other' }
is bar(Point.new(v => 1)), 'other', 'a lone value candidate rejects a distinct object';
is bar(G), 'G', 'and accepts the constant';

class Valued { has $.v; method WHICH { "Valued|$!v" } }
constant H = Valued.new(v => 1);
multi baz(H) { 'H' }
multi baz($) { 'other' }
is baz(Valued.new(v => 1)), 'H', 'a class with value identity matches by WHICH';
is baz(Valued.new(v => 2)), 'other', 'and a different WHICH does not';

constant N = 5;
multi five-ish(N) { 'five' }
multi five-ish($) { 'other' }
is five-ish(5), 'five', 'a numeric constant matches its value';
is five-ish(6), 'other', 'a different value falls through';
is five-ish(5), 'five', 'after the fall-through the constant still matches';

multi times(Int $n where 1 < $n < 100, G) { 'table' }
multi times(Int $n where 1 < $n < 100, Point:D $p) { 'generic' }
is times(3, G), 'table', 'a value candidate outranks an equally-where-constrained typed one';
is times(3, Point.new(v => 2)), 'generic', 'and other points take the generic candidate';

constant TAU = 6.28;
sub typed(TAU $x) { "got $x" }
is typed(6.28), 'got 6.28', 'a typed parameter named by a value constant binds the value';
dies-ok { typed(1.5) }, 'and rejects anything else';

constant INT-ALIAS = Int;
sub aliased(INT-ALIAS $x) { $x + 1 }
is aliased(41), 42, 'a type-alias constant is still a nominal type';

enum Colour <red green>;
multi colour(red) { 'red' }
multi colour($) { 'other' }
is colour(green), 'other', 'an enum value parameter is unaffected';
