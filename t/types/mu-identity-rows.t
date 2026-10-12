use Test;

# ADR-11276 §9.91: `self`, `defined`, `DEFINITE`, `Bool`, `WHICH`, `WHERE`
# and `clone` are `Mu`'s rows, reached by every receiver: a value of any
# shape, a type object, a Junction, a code object and an instance of a user
# class. The answers below are Rakudo's.

plan 36;

my $m = "abc" ~~ /b/;
isa-ok $m.self, Match, 'Match.self is the match itself, not its string';
is $m.self.Str, 'b', '... with its matched text';

is \(1, :a(2)).clone.raku, '\(1, :a(2))', 'Capture.clone';
is "a".NFC.clone.raku, 'Uni.new(0x0061).NFC', 'Uni.clone';

is 5.self, 5, 'Int.self';
is-deeply {a => 1}.self, {a => 1}, 'Hash.self';
ok Int.self === Int, 'a type object is its own self';
dies-ok { Failure.new("boom").self }, 'an unhandled Failure explodes on .self';

ok 5.defined, 'Int.defined';
nok Int.defined, 'a type object is not defined';
nok Nil.defined, 'Nil is not defined';
ok any(1, Int).defined, 'any() collapses its eigenstates';
nok all(1, Int).defined, 'all() collapses its eigenstates';
nok one(Int, Str).defined, 'one() with no defined eigenstate';
my $f = Failure.new("x");
nok $f.defined, 'a Failure is not defined';
ok $f.handled, '... and asking handles it';

ok 5.DEFINITE, 'Int.DEFINITE';
nok Int.DEFINITE, 'a type object is not DEFINITE';
my $g = Failure.new("y");
ok $g.DEFINITE, 'a Failure instance is DEFINITE';
$g.so;

nok 0.Bool, 'Int.Bool';
ok (a => 1).Bool, 'Pair.Bool';
nok ().Bool, 'an empty List is False';
nok Int.Bool, 'a type object is False';
ok (sub {}).Bool, 'a code object is True';

class Plain { has @.x }
my $p = Plain.new(x => [1]);
is $p.WHICH.^name, 'ObjAt', 'an instance has an ObjAt';
is 5.WHICH, 'Int|5', 'a value has a ValueObjAt';
is (a => [1]).WHICH.^name, 'ObjAt', 'a Pair holding a container has an ObjAt';
class SubInt is Int { }
like SubInt.new(3).WHICH.Str, /^ 'SubInt|' /, 'an instance of a subclass of Int';
my class MyArr is Array { }
like MyArr.new(1, 2).WHICH.Str, /^ 'MyArr|' /, 'an instance of a subclass of Array';

isa-ok 5.WHERE, Int, 'Int.WHERE';
is $p.WHERE, $p.WHERE, 'WHERE is stable for one object';
isnt Plain.new.WHERE, Plain.new.WHERE, '... and differs between objects';

my $c = $p.clone;
$c.x.push(5);
is-deeply $p.x, [1, 5], 'an instance clone is shallow, as in Rakudo';
ok Int.clone === Int, 'a type object clones to itself';
is (1..3).clone, 1..3, 'Range.clone';
is Date.new(2020, 1, 1).clone(day => 5), Date.new(2020, 1, 5), 'Date.clone with a twiddle';
