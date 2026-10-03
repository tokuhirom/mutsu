use Test;
# Seen in the Parameterizable distribution: parameterizing a role with
# arguments no candidate accepts throws at the `R[...]` expression.
plan 6;

role Solution[Str $question, Any:D $result] {}
role Typed[Int $n] {}

my $a = try Solution["Question", Int];
isa-ok $!, X::Role::Parametric::NoSuchCandidate, 'a type object for an :D parameter has no candidate';
ok !$a.defined, 'and try yields a type object';
my $b = try Typed["x"];
isa-ok $!, X::Role::Parametric::NoSuchCandidate, 'a Str for an Int parameter has no candidate';
ok !$b.defined, 'and try yields a type object';
my $c = try Solution["Question", 42];
nok $!.defined, 'accepted arguments parameterize';
my $d = try Typed[Int];
nok $!.defined, 'an Int type object satisfies an Int parameter';
