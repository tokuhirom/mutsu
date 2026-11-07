use Test;

plan 5;

# `^add_role` of a parametric role INSTANCE keeps the instance's argument
# values (#17007).
role Numish[&times] { method m($x) { times($x) } }
my $r = Numish[* * 10];
my $c = Metamodel::ClassHOW.new_type(:name<Comp>);
$c.^add_role($r);
$c.^compose;
is $c.new.m(7), 70, 'a WhateverCode argument reaches the composed role';

class K does Numish[* * 3] { }
is K.new.m(7), 21, 'the static `does` form agrees';

role Tagged[Str $tag] { method tag { $tag } }
my $d = Metamodel::ClassHOW.new_type(:name<Dyn>);
$d.^add_role(Tagged["hello"]);
$d.^compose;
is $d.new.tag, 'hello', 'a Str value argument';

role Typed[::T] { method t { T } }
my $e = Metamodel::ClassHOW.new_type(:name<DynT>);
$e.^add_role(Typed[Int]);
$e.^compose;
is $e.new.t.^name, 'Int', 'a type argument still works';

my $f = Metamodel::ClassHOW.new_type(:name<Two>);
$f.^add_role(Numish[* + 1]);
$f.^compose;
is $f.new.m(1), 2, 'another instance of the same role group';
