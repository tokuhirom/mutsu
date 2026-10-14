use Test;

# ADR-11276 §9.105: `ACCEPTS` of the quant hashes, `Pair` and `Range` binds
# `Mu \topic`, so a user object is a topic like any other and what the method
# does with it is the object's own code.

plan 22;

class P { }
class A { method a { 1 } }
class S { method Str { "3" } }
class N { method Numeric { 3 } }

# A quant hash coerces a topic of another kind to its own.
ok set(1, 2).ACCEPTS(1..2), 'Set.ACCEPTS: a Range topic is its .Set';
ok set(1, 2).ACCEPTS((1, 2)), 'Set.ACCEPTS: a List topic is its .Set';
ok set(<a>).ACCEPTS("a"), 'Set.ACCEPTS: a Str topic is its .Set';
ok set(<a>).ACCEPTS({a => 1}), 'Set.ACCEPTS: a Hash topic is its .Set';
ok set(1, 2).ACCEPTS(bag(1, 1, 2)), 'Set.ACCEPTS: a Bag topic is its .Set';
ok bag(1, 2).ACCEPTS(set(1, 2)), 'Bag.ACCEPTS: a Set topic is its .Bag';
ok mix(1, 2).ACCEPTS(bag(1, 2)), 'Mix.ACCEPTS: a Bag topic is its .Mix';
ok SetHash.new(1, 2).ACCEPTS(set(1, 2)), 'SetHash.ACCEPTS: a Set';
nok set(1, 2).ACCEPTS(P.new), 'Set.ACCEPTS: a user object is a one-element Set';
nok set(1, 2).ACCEPTS(P), 'Set.ACCEPTS: a type object';

# `Pair.ACCEPTS(Mu $topic)` calls the method the key names and compares truth.
ok (a => 1).ACCEPTS(A.new), 'Pair.ACCEPTS: the method the key names';
ok (a => 2).ACCEPTS(A.new), 'Pair.ACCEPTS: ... compared by truth';
nok (a => False).ACCEPTS(A.new), 'Pair.ACCEPTS: a False value wants a false result';
ok (abs => 5).ACCEPTS(-5), 'Pair.ACCEPTS: a built-in topic calls the method too';
throws-like { (a => 1).ACCEPTS(5) }, X::Method::NotFound, 'Pair.ACCEPTS: no such method';
ok (a => 42).ACCEPTS({a => 42}), 'Pair.ACCEPTS: an Associative topic is looked up';
nok (a => 42).ACCEPTS({a => 4}), 'Pair.ACCEPTS: ... and its value smartmatched';
ok (a => 2).ACCEPTS(bag(<a a>)), 'Pair.ACCEPTS: a Bag is Associative';
my $q = a => 1;
ok (a => 1).ACCEPTS($q), 'Pair.ACCEPTS: a Pair topic';

# `Range.ACCEPTS(Mu \topic)` compares a user object with the ends by `cmp`.
ok (1..5).ACCEPTS(S.new), 'Range.ACCEPTS: a user object is compared by its .Stringy';
nok (1..5).ACCEPTS(N.new), 'Range.ACCEPTS: ... not by its .Numeric';
nok (1..5).ACCEPTS(P.new), 'Range.ACCEPTS: a user object outside the ends';
