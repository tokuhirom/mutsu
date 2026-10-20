use Test;
plan 27;

role Local { }
class T { }
sub outer-only(Local $value) { 'outer' }
sub outer-type { Local }
role Owner[Str :$name] {
    my role Local { method greeting { 'hello' } }
    my $Local = 'scalar';
    multi sub read($value where * !~~ Local, |rest) {
        $value does Local;
        read($value, |rest)
    }
    multi sub read(Local $value, Int $count) { 'ok ' ~ $count }
    method go($value) { read($value, 3) }
    method wrap($value) { $value does Local }
    method checked(Local $value) { $value.greeting }
    method local-type { Local }
    method scalar { $Local }
    method set-scalar($value) { $Local = $value }
    method outer-only($value) { outer-only($value) }
    method outer-type { outer-type() }
}
class A does Owner[name => 'a'] { }
class B does Owner[name => 'b'] { }
class Element { }

my $a = A.new;
my $b = B.new;
is $a.go(Element.new), 'ok 3', 'a recursive multi accepts the role it just applied';
is $a.go(Element.new), 'ok 3', 'the typed multi works on repeated calls';
is $b.go(Element.new), 'ok 3', 'another parameterization keeps its local role binding';
my $wrapped = $a.wrap(Element.new);
is $a.checked($wrapped), 'hello', 'a role-typed method accepts its own local role';
is $b.checked($wrapped), 'hello', 'a parameter-independent local role is shared';
ok $a.local-type === $b.local-type, 'the declaration retains the same role identity';
ok $a.^find_method('checked').signature.params[1].type === $a.local-type,
    'the stored method signature retains the local role type object';
ok $wrapped ~~ $a.local-type, 'smartmatch agrees with parameter binding';
nok $wrapped ~~ Local, 'the same-named outer role remains a distinct type';
is $a.scalar, 'scalar', 'a same-named scalar does not replace the local type';
$a.set-scalar('changed');
is $a.scalar, 'changed', 'the same-named scalar remains a writable container';
is $a.checked($wrapped), 'hello', 'changing the scalar does not change a signature type';
dies-ok { $a.checked(Element.new) }, 'a bare object is rejected by the role constraint';
is $a.outer-only(Element.new but Local), 'outer', 'a global callee keeps its own role constraint';
dies-ok { $a.outer-only($wrapped) }, 'a caller local role does not retarget a global signature';
ok $a.outer-type === Local, 'a global callee also reads its own role in its body';

role Other[::T] {
    my role Local { method greeting { 'other' } }
    method wrap($value) { $value does Local }
    method checked(Local $value) { $value.greeting }
    method local-type { Local }
    method picked(T $value) { $value }
}
class C does Other[Int] { }
my $c = C.new;
my $other = $c.wrap(Element.new);
is $c.checked($other), 'other', 'another declaration resolves its own local role';
nok $c.local-type === $a.local-type, 'same-named roles at different sites are distinct';
dies-ok { $c.checked($wrapped) }, 'the other declaration rejects the first role';
dies-ok { $a.checked($other) }, 'the first declaration rejects the other role';
is $a.checked($wrapped), 'hello', 'later role declarations do not retarget the first signature';
ok $other ~~ $c.local-type, 'the other role also agrees with smartmatch';
is $c.picked(5), 5, 'a role type parameter still shadows a global same-named class';
dies-ok { $c.picked('bad') }, 'the composed role type parameter retains its constraint';

role WithParent[::T] {
    my role Parent { method parent-answer { 7 } }
    also does Parent;
}
class D does WithParent[Int] { }
is D.new.parent-answer, 7, 'a role parent resolves its local declaration after alias restoration';

my $native = 1;
$native does Owner[name => 'native'];
my $native-child = Element.new but $native.local-type;
is $native.checked($native-child), 'hello', 'a runtime mixin retains its local signature type';
nok $native.local-type === Local, 'a runtime mixin does not replace the outer role binding';
