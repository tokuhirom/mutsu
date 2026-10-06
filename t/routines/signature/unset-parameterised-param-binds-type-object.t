use Test;

plan 5;

# An unset optional/named parameter with a parameterised type binds the type
# object WITH its type arguments (#12105).
sub f(Positional[Int] $x?) { $x.raku }
is f(), 'Positional[Int]', 'unset optional positional keeps its parameterisation';
sub g(Array[Int] :$x) { $x.raku }
is g(), 'Array[Int]', 'unset named keeps its parameterisation';

class Y {
    has Positional[Int] $.a;
    submethod BUILD(Positional[Int] :$a) { $!a := $a }
}
my $y;
lives-ok { $y = Y.new }, 'binding an unset parameterised param into an attribute passes the check';
nok $y.a.defined, 'the attribute holds the type object';
is $y.a.raku, 'Positional[Int]', 'with its parameterisation';

sub h(Positional[Int] :$x) { my Positional[Int] $z := $x; $z.raku }
is h(), 'Positional[Int]', 'binding into a typed lexical passes';
