use Test;

# A curried role is smartmatched by its positional arguments only: the named
# arguments of `NT[Int, :coerce(&cf)]` are no part of the type (#15710).
# Checked against rakudo.
plan 6;

role NT[::T, :&coerce] { method c { 'c' } }
sub cf($x) { $x }

my $w = NT[Int, :coerce(&cf)].new;
my $p = NT[Int].new;

ok $w ~~ NT[Int], 'a named-argument instance matches the plain role';
ok $w ~~ NT[Int, :coerce(&cf)], '... and the role with the named argument';
ok $p ~~ NT[Int, :coerce(&cf)], 'a plain instance matches the role with the named argument';
nok $w ~~ NT[Str], 'a different positional argument still does not match';
ok NT[Int, :coerce(&cf)] ~~ NT[Int, :coerce(&cf)], 'the role matches itself';
is $w.WHAT.^name, 'NT[Int]', 'the instance type is named without the named argument';
