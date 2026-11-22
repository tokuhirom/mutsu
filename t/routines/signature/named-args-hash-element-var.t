use Test;

# A `*%h` element bound from a named argument written as a variable holds the
# caller's container, and its `.VAR` reflects that `Scalar`, not the value's
# type (#18372).

plan 8;

my $y = 1;

sub f(*%h) { %h<a>.VAR.^name }
is f(:a($y)), 'Scalar', 'a sub slurpy element from a variable is a Scalar';
is f(:a(5)), 'Int', 'one from a literal is the value';

class C { method m(*%h) { %h<a>.VAR.^name } }
is C.m(:a($y)), 'Scalar', 'so is a method slurpy element';

sub of(*%h) { %h<a>.VAR.of.^name }
is of(:a($y)), 'Mu', 'and it answers the container .of';

sub w(*%h) { %h<a> = 7 }
w(:a($y));
is $y, 7, 'writing the element still writes the caller variable';

sub b(*%h) { my $x := %h<a>; $x = 8 }
b(:a($y));
is $y, 8, 'binding the element aliases the caller variable';

{
    my %g = a => 1;
    is %g<a>.VAR.^name, 'Scalar', 'an ordinary hash element is a Scalar';
    my $z = 3;
    %g<b> := $z;
    is %g<b>.VAR.^name, 'Scalar', 'a bound hash element is a Scalar';
}
