use Test;

# A parametric role and a plain role may share one name, as Definitely's
# `role Some[::Type]` and `role Some` do. Each composition must use its own
# candidate's construction submethods and attribute types:
# - `S[Int].new` used to run the plain `S`'s TWEAK;
# - the plain `S`'s untyped `$.value` used to be checked against the
#   parametric role's `Type`.

plan 7;

my @tweaks;
role S[::Type] { has Type $.value; method kind { 'param' } }
role S { has $.s; has $.value; method kind { 'plain' }; submethod TWEAK { @tweaks.push: 'plain' } }

my $p = S[Int].new(value => 5);
is $p.kind, 'param', 'S[Int] puns the parametric candidate';
is $p.value, 5, 'with its attribute';
is-deeply @tweaks, [], "and does not run the plain candidate's TWEAK";

my $q = S.new(s => 1);
is $q.kind, 'plain', 'S puns the plain candidate';
is-deeply @tweaks, ['plain'], 'which runs its own TWEAK';

role T[::Type] { has Type $.value }
role T { has $.s; has $.value; submethod TWEAK { $!value := $!s.value } }
my $t = T.new(s => T[Int].new(value => 3));
is $t.value, 3, "the plain candidate's untyped attribute binds any value";

dies-ok { S[Int].new(value => 'str') }, "the parametric candidate's type is still enforced";
