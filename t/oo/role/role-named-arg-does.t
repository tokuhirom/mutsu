use Test;

# `.does` of a curried role asks about its positional arguments only: the named
# arguments of `NT[Int, :coerce(&cf)]` are no part of the type (#15715).
# Checked against rakudo.
plan 4;

role NT[::T, :&coerce] { method c { 'c' } }
sub cf($x) { $x }

my $w = NT[Int, :coerce(&cf)].new;
my $p = NT[Int].new;

ok $w.does(NT[Int, :coerce(&cf)]), 'a named-argument instance does the role with the named argument';
ok $w.does(NT[Int]), '... and the plain role';
ok $p.does(NT[Int, :coerce(&cf)]), 'a plain instance does the role with the named argument';
nok $w.does(NT[Str, :coerce(&cf)]), 'a different positional argument still does not match';
