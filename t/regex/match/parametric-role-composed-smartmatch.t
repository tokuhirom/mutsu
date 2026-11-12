use Test;

plan 10;

# A class that composes a parameterised role through the MOP (`^add_role`)
# smartmatches the parameterised role, both as a type object and an instance,
# and answers `^does` for it.

role RS[::T] { method of { T } }
class Base { }

sub mk(Mu \type) is raw {
    my $c := Metamodel::ClassHOW.new_type(:name<X::RS>);
    $c.^add_parent: Base;
    $c.^add_role: RS[type];
    $c.^compose;
    $c
}

class M1 { }
my $c := mk(M1);
ok $c ~~ RS[M1], 'MOP-composed type object matches RS[M1]';
ok $c.of === M1, 'role parameter is bound';
ok $c ~~ RS, 'matches the bare role';
ok $c.new ~~ RS[M1], 'instance matches RS[M1]';
ok $c.^does(RS[M1]), '^does sees the parameterised role';
nok $c ~~ RS[Base], 'a different argument does not match';

my $dyn := Metamodel::ClassHOW.new_type(:name<Dyn>);
$dyn.^compose;
my $d := mk($dyn);
ok $d ~~ RS[$dyn], 'argument may itself be a MOP-built type';

class C does RS[M1] { }
ok C ~~ RS[M1], 'statically composed type object';
ok C.new ~~ RS[M1], 'statically composed instance';
ok C.^does(RS[M1]), 'statically composed ^does';
