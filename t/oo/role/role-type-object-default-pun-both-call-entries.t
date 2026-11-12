use v6;
use Test;

# A method called on a parametric role's type object whose type parameters all
# have defaults runs on the pun of the default parameterization, through
# whichever method-call entry the call site compiles to: a plain receiver
# (`R.m`), a sigilless parameter, and an element read (`@a[0].m`, the
# non-container entry). mutsu remembers that redirect; the calls repeat so a
# stale or wrong one would show up. Verified against Rakudo.

plan 10;

role Defaulted[::T = Int, ::U = Str] {
    method types() { T.^name ~ '/' ~ U.^name }
    method twice($x) { $x * 2 }
}
role Plain { method hello() { 'hello' } }

sub via-param(Mu \R) { R.types }
my @roles = Defaulted, Plain;

my @got;
for ^20 -> $i {
    @got.push: Defaulted.types eq 'Int/Str';
    @got.push: via-param(Defaulted) eq 'Int/Str';
    @got.push: @roles[0].types eq 'Int/Str';
    @got.push: @roles[0].twice($i) == $i * 2;
    @got.push: @roles[1].hello eq 'hello';
    @got.push: Plain.hello eq 'hello';
}
ok @got.all, 'every entry runs the method on the default parameterization';
is Defaulted.types, 'Int/Str', 'a bare role type object';
is @roles[0].types, 'Int/Str', 'an element read';
is via-param(Defaulted), 'Int/Str', 'a sigilless parameter';
is Defaulted[Rat, Num].types, 'Rat/Num', 'an explicit parameterization is its own';
is Defaulted.^name, 'Defaulted', 'the role type object keeps its own name';
is @roles[0].^name, 'Defaulted', 'through an element read too';
ok Defaulted.new ~~ Defaulted, '.new still builds a pun instance';
is Defaulted.new.types, 'Int/Str', 'which sees the defaults';
is @roles[1].hello, 'hello', 'a role without type parameters still puns';
