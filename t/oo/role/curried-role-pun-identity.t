use Test;

# A curried parametric role puns to one class: `R[Int,Str].^pun` is the very
# type `R[Int,Str].new` constructs, so an instance's `.WHAT =:=` the pun, and
# a named role argument is not part of the curried type's identity for a type
# check (`does R[Int,Str,:v]` satisfies `R[Int,Str]`). Reduced from the Rake
# distribution's t/01-basic.rakutest.

plan 7;

role R[*@types, :$v] { has $.x }

my $o = R[Int,Str].new;
ok $o.WHAT =:= R[Int,Str].^pun, '.WHAT of an instance =:= the pun';
ok $o.WHAT =:= R[Int,Str].^pun<>, 'decontainerized pun too';
is R[Int,Str].^pun.^name, 'R[Int,Str]', 'the pun is named after the curried role';
is R[Int,Str].^pun.raku, 'R[Int,Str]', '.raku of the pun';

class B does R[Int,Str,:v] { }
ok B ~~ R[Int,Str], 'type object with a named role argument';
ok B.new ~~ R[Int,Str], 'instance with a named role argument';
sub f(R[Int,Str] $x) { 'bound' }
is f(B.new), 'bound', 'binds to a curried-role-typed parameter';
