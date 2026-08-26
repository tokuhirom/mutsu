use v6;
use Test;

# `R.^pun` must return the composition-keyed punned CLASS type object, not
# the role GROUP's own type object. Before this fix mutsu's `^pun` handed
# back a bare `Value::package(role_name)` -- the exact same value the role
# group itself resolves to (since `ensure_role_punned_to_class` registers
# the pun's `ClassDef` under the role's own name) -- so `.HOW`, `.^roles`,
# `~~`, and `.^candidates` all leaked the role group's behaviour into what
# should be an ordinary class. See
# news/2026-08/role-pun-metamethod-returns-punned-class.md and
# docs/adr/0060-mixin-what-is-a-composition-keyed-type-object.md.
plan 13;

role R {
    method m { 42 }
}

is R.^pun.^name, 'R', '^pun.^name is the bare role name';
is R.^pun.HOW.^name, 'Perl6::Metamodel::ClassHOW',
    "^pun reports ClassHOW, not the role group's ParametricRoleGroupHOW";
ok R.^pun === R.^pun, '^pun is stable across repeated calls';
ok R.^pun === R.new.WHAT, '^pun is the SAME value R.new.WHAT produces';
is R.^pun.^mro.gist, '((R) (Any) (Mu))', '^pun.^mro';
is-deeply R.^pun.^roles, (R,), '^pun.^roles lists the role it puns';
is R.^pun.new.m, 42, "a pun constructed via .new still runs the role's methods";
nok R ~~ R.^pun, 'the role itself does not smartmatch its own pun';
ok R.^pun ~~ R, 'the pun does smartmatch the role it puns';

throws-like { R.^pun.^candidates }, X::Method::NotFound,
    '^pun.^candidates throws -- ClassHOW has no .^candidates (only the role group does)';

class C does R { }
nok C === R.^pun, 'a class that DOES the role is not identical to its pun';

subtest 'a built-in role with no user-declared RoleDef also puns' => {
    plan 3;
    is Iterable.^pun.^name, 'Iterable', '^pun.^name';
    is Iterable.^pun.HOW.^name, 'Perl6::Metamodel::ClassHOW', '^pun.HOW is ClassHOW';
    is-deeply Iterable.^pun.flat, (Iterable.^pun,),
        '.flat on a punned type object preserves its own identity (ADR-0060)';
}

subtest '.flat on an ordinary role-mixed (but/does) value is unaffected' => {
    plan 2;
    my $x = 1 but R;
    is $x.flat[0].^name, 'Int+{R}',
        '.flat on a but-mixed scalar preserves the whole composition, not the bare inner';
    my %h = a => 1, b => 2;
    %h does R;
    is (%h.flat).elems, 2,
        '.flat on a role-mixed Hash still spills its pairs (container-shaped inner)';
}

done-testing;
