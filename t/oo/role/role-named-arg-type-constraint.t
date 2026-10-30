use Test;

# A type constraint `NT[Int, :coerce(&cf)]` names and checks as `NT[Int]`: the
# named arguments are no part of the type (#15504). Checked against rakudo.
plan 7;

role NT[::T, :&coerce] { method c { 'c' } }
sub cf($x) { $x }

class A {
    has NT[Int, :coerce(&cf)] $.d is rw;
    has NT[Int] $.e is rw;
}
my $a = A.new;
is $a.d.WHAT.^name, 'NT[Int]', 'the attribute type is named without the named argument';
is A.^attributes[0].type.^name, 'NT[Int]', 'so is its introspected type';

lives-ok { $a.d = NT[Int, :coerce(&cf)].new }, 'an instance is assignable to the attribute';
is $a.d.c, 'c', 'and works';

my NT[Int, :coerce(&cf)] $x;
is $x.WHAT.^name, 'NT[Int]', 'a variable declared with a named role argument is accepted';
lives-ok { my NT[Int, :coerce(&cf)] $y = NT[Int].new }, 'a plain instance satisfies it';
lives-ok { $a.e = NT[Int, :coerce(&cf)].new }, 'a named-argument instance satisfies the plain type';
