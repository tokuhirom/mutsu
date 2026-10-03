use Test;

# #10004: an attribute has exactly one `Attribute` meta-object, so every
# `.^attributes` lookup of it is `===` and shares a `.WHICH`. Expected values
# are rakudo's.

plan 13;

class Foo { has $.foo; has @.b }

my $a = Foo.^attributes[0];
ok $a === Foo.^attributes[0], 'two lookups of an attribute are ===';
is $a.WHICH, Foo.^attributes[0].WHICH, 'and share a .WHICH';
ok Foo.^attributes(:local)[1] === Foo.^attributes[1], ':local and the MRO walk agree';
nok Foo.^attributes[0] === Foo.^attributes[1], 'two attributes stay distinct';

my $set = SetHash.new;
$set.set($_) for |Foo.^attributes, |Foo.^attributes;
is $set.elems, 2, 'an identity-keyed SetHash sees each attribute once';

role R { has $.x }
class A does R {}
class B does R {}
nok A.^attributes[0] === B.^attributes[0], 'a role attribute composed into two classes is two attributes';
ok A.^attributes[0] === A.^attributes[0], 'but one per class';
ok R.^attributes[0] === R.^attributes[0], 'and one on the role itself';

class P { has $.p }
class C is P {}
ok C.^attributes[0] === P.^attributes[0], 'an inherited attribute is the parent class one';

ok Rat.^attributes[0] === Rat.^attributes[0], 'a built-in type attribute is stable too';

my @l = (^2).map: { my class L { has Int $.x }; L.^attributes[0] };
ok @l[0] === @l[1], 'a lexical class declared in a loop has one attribute';

is Foo.^attributes[0].name, '$!foo', 'the meta-object still describes the attribute';

class D {
    #| the d
    has $.d;
}
ok $=pod[0].WHEREFORE === D.^attributes[0], 'the $=pod declarant is the same attribute';
