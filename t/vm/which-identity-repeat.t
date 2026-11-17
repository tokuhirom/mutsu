use Test;

# `===` asks, for each instance operand, whether its class overrides `WHICH`
# (a `method WHICH` or a `has $.WHICH` accessor); the answers are memoized per
# class until the next declaration. Repeated tests must keep answering what a
# fresh one would, and a type object compares by its name. Every expectation
# was checked against Rakudo.

plan 12;

class Plain { has $.v }
class ByValue { has $.v; method WHICH { ValueObjAt.new("ByValue|$!v") } }
class ByAttr { has $.WHICH }

my ($p1, $p2) = Plain.new(:v(1)), Plain.new(:v(1));
is-deeply (^3).map({ $p1 === $p1 }).List, (True, True, True), 'an instance is itself, every time';
is-deeply (^3).map({ $p1 === $p2 }).List, (False, False, False),
    'two plain instances stay distinct, every time';

my ($v1, $v2, $v3) = ByValue.new(:v(1)), ByValue.new(:v(1)), ByValue.new(:v(2));
is-deeply (^3).map({ $v1 === $v2 }).List, (True, True, True), 'a method WHICH gives value identity';
nok $v1 === $v3, 'and tells different values apart';
nok $v1 === $p1, 'against a class without its own WHICH';

ok ByAttr.new(:WHICH<x>) === ByAttr.new(:WHICH<x>), 'a public $.WHICH accessor decides too';
nok ByAttr.new(:WHICH<x>) === ByAttr.new(:WHICH<y>), 'and tells them apart';

# A class declared after warm tests is probed afresh.
for ^2 { my $ = $p1 === $p2 }
my class Late { has $.v; method WHICH { ValueObjAt.new("Late|$!v") } }
ok Late.new(:v(3)) === Late.new(:v(3)), 'a class declared later is probed afresh';

ok Plain === Plain, 'a type object is itself';
nok Plain === ByValue, 'and not another type';
my $t = Plain;
is-deeply (^3).map({ $t === Plain }).List, (True, True, True), 'through a variable, every time';
ok Plain.HOW.WHAT === Metamodel::ClassHOW, "a class's HOW is the public ClassHOW type";
