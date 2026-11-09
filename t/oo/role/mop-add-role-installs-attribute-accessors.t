use Test;

plan 8;

# A role added with `^add_role` brings its public attributes' accessors with it,
# and they answer before a built-in method of the same name: Qwiratry's
# aggregate functions compose `has $.sort` into a class made through the MOP.
role Plain { has $.sort is rw = 5; has $.foo is rw = 6; }
my $c = Metamodel::ClassHOW.new_type(:name<ViaMop>);
$c.^add_role(Plain);
$c.^compose;
my $obj = $c.new;

is $obj.foo, 6, 'an accessor with no built-in of its name';
is $obj.sort, 5, 'an accessor named like a built-in method (Any.sort)';
$obj.sort = 9;
is $obj.sort, 9, 'and it is writable';
is-deeply $obj.^attributes.map(*.name).List, ('$!sort', '$!foo'), 'the attributes came along';

role Param[::T] does Callable { has T $.state is rw; has $.sort is rw is built = Nil; }
my $p = Metamodel::ClassHOW.new_type(:name<ViaMopParametric>);
$p.^add_role(Param[Int]);
$p.^compose;
my $q = $p.new;
$q.sort = { .<amount> };
isa-ok $q.sort, Block, 'a parametric role: the accessor hands back the stored block';
is $q.sort.count, 1, 'whose arity is readable';

class Declared does Plain { }
is Declared.new.sort, 5, 'the declared class composes the same way';
is-deeply $obj.sort.WHAT, Int, 'the stored value is an Int, not a sorted Seq';
