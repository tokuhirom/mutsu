use Test;

# A subset parameter reports a *constraint* failure only when the value is of
# the subset's refinee type and the `where` rejected it; a value of another
# type is a plain type-check failure. The expected type is named without a
# smiley. Core's `UInt` is a subset of Int (taurus checks this wording).

plan 5;

sub u(UInt $x) { }
sub ud(UInt:D $x) { }
subset Pos of Int where * > 0;
sub p(Pos:D $x) { }
sub q(Pos $x) { }

throws-like { u(-1) }, X::TypeCheck::Binding::Parameter,
    message => "Constraint type check failed in binding to parameter '\$x'; expected UInt but got Int (-1)",
    'UInt rejecting a negative Int is a constraint failure';
throws-like { ud(-1) }, X::TypeCheck::Binding::Parameter,
    message => "Constraint type check failed in binding to parameter '\$x'; expected UInt but got Int (-1)",
    'UInt:D names the subset without its smiley';
throws-like { p(-1) }, X::TypeCheck::Binding::Parameter,
    message => "Constraint type check failed in binding to parameter '\$x'; expected Pos but got Int (-1)",
    'user subset with :D smiley names the subset without it';
my $s = "a";
throws-like { q($s) }, X::TypeCheck::Binding::Parameter,
    message => "Type check failed in binding to parameter '\$x'; expected Pos but got Str (\"a\")",
    'a value outside the refinee type is a plain type-check failure';
lives-ok { u(0); ud(3); p(1); q(2) }, 'accepted values still bind';
