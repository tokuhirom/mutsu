use Test;

# ADR-15829: an unquoted, argument-less `.WHAT` / `.HOW` on a plain value or
# type object is answered without the generic method-call chain. The answers
# must be the ones the chain gave (raku is the oracle; a quoted call stays on
# the generic path and is the in-process cross-check).

plan 31;

# plain values, both receiver forms (a literal and a variable)
my $i = 42; my $n = 4.2e0; my $s = 'str'; my $b = True; my $big = 2 ** 70;
is $i.WHAT.^name, 'Int',  '.WHAT of an Int variable';
is 42.WHAT.^name, 'Int',  '.WHAT of an Int literal';
is $n.WHAT.^name, 'Num',  '.WHAT of a Num';
is $s.WHAT.^name, 'Str',  '.WHAT of a Str';
is $b.WHAT.^name, 'Bool', '.WHAT of a Bool';
is $big.WHAT.^name, 'Int', '.WHAT of a big Int';
ok $i.WHAT === Int, '.WHAT is the Int type object';
is $i.HOW.^name, 'Perl6::Metamodel::ClassHOW', '.HOW of an Int is its ClassHOW';
ok $i.HOW === $s.HOW ?? False !! True, '.HOW differs between Int and Str';
ok $i.HOW === Int.HOW, '.HOW of a value is the type object\'s HOW';

# type objects: built-in, user class, role, enum, subset
class Foo { has $.x }
role Rl { }
enum Color <red green>;
subset Even of Int where * %% 2;
is Foo.WHAT.^name, 'Foo', '.WHAT of a user class type object';
ok Foo.WHAT === Foo, '.WHAT of a type object is itself';
is Rl.WHAT.^name, 'Rl', '.WHAT of a role';
is Color.WHAT.^name, 'Color', '.WHAT of an enum type object';
is Even.WHAT.^name, 'Even', '.WHAT of a subset';
is Foo.HOW.^name, 'Perl6::Metamodel::ClassHOW', '.HOW of a class';
is Rl.HOW.^name, 'Perl6::Metamodel::ParametricRoleGroupHOW', '.HOW of a role';
is Foo.HOW.name(Foo), 'Foo', 'the HOW answers its own metamethods';

# a quoted call goes the generic way and agrees
is $i."WHAT"().^name, $i.WHAT.^name, 'quoted .WHAT agrees with unquoted';
ok Foo."HOW"() === Foo.HOW, 'quoted .HOW agrees with unquoted';

# a user method of the same name is not reachable unquoted (macros), only quoted
class Shadow {
    method WHAT { 'user-what' }
    method HOW  { 'user-how' }
}
is Shadow.new.WHAT.^name, 'Shadow', 'unquoted .WHAT ignores a user method';
is Shadow.WHAT.^name, 'Shadow', 'unquoted .WHAT on the type object ignores it too';
is Shadow.new."WHAT"(), 'user-what', 'quoted .WHAT reaches the user method';
is Shadow."HOW"(), 'user-how', 'quoted .HOW reaches the user method';

# receivers the lane does not take still work
my @a = 1, 2; my %h = a => 1; my $f = Failure.new('x');
is @a.WHAT.^name, 'Array', '.WHAT of an Array';
is %h.WHAT.^name, 'Hash', '.WHAT of a Hash';
is (1, 2).WHAT.^name, 'List', '.WHAT of a List';
is Foo.new.WHAT.^name, 'Foo', '.WHAT of an instance';
is $f.WHAT.^name, 'Failure', '.WHAT of a Failure';
is (1/3).WHAT.^name, 'Rat', '.WHAT of a Rat';

# in a loop, to exercise the repeated path
my $acc = '';
for ^3 { $acc ~= $i.WHAT.^name ~ ',' ~ Foo.WHAT.^name ~ ';' }
is $acc, 'Int,Foo;' x 3, 'repeated calls agree';
