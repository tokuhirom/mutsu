use Test;

# `eqv` on two instances of one class compares their default `.raku`
# renderings (Rakudo's `Any:D eqv Any:D`), so an attribute counts by how it
# reads. Red's `Select.of` holds two distinct generated types with one name;
# its CASE optimizer folds identical branches only if they are `eqv`.

plan 9;

class Q { has $.v }
class P { has $.v; method raku { "P" } }

my \T1 = Metamodel::ClassHOW.new_type(:name<Same>); T1.^compose;
my \T2 = Metamodel::ClassHOW.new_type(:name<Same>); T2.^compose;

ok T1 !=== T2, 'the two types are distinct objects';
nok T1 eqv T2, 'and distinct type objects are not eqv themselves';
ok Q.new(v => T1) eqv Q.new(v => T2), 'instances whose attributes read the same are eqv';
ok Q.new(v => Q.new(v => T1)) eqv Q.new(v => Q.new(v => T2)), 'also nested';
ok Q.new(v => {a => T1}) eqv Q.new(v => {a => T2}), 'also inside a hash attribute';
nok (T1,) eqv (T2,), 'a plain List still compares its elements by eqv';
nok Q.new(v => 1) eqv Q.new(v => 2), 'different attribute values are not eqv';
ok P.new(v => 1) eqv P.new(v => 2), 'a user .raku decides for its class';
nok Q.new(v => 1) eqv P.new(v => 1), 'different classes are not eqv';
