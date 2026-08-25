use Test;

# Rakudo keeps three distinct meta-objects for what source code writes as one
# `role R { ... }` plus its uses, and mutsu used to conflate them because `.HOW`
# was decided purely from a type *name* looked up in the role registry:
#
#   * ParametricRoleGroupHOW -- the installed NAME `R`; dispatches across every
#     same-named candidate.
#   * ParametricRoleHOW      -- one individual `role` declaration. This is what
#     the declaration EXPRESSION evaluates to, what `.^candidates` hands out,
#     and what `Metamodel::ParametricRoleHOW.new_type` produces.
#   * ClassHOW               -- an ordinary class, including the class Rakudo
#     synthesizes when you call `.new` on a bare role (punning).
#
# See docs/adr/0047 (type identity is a declaration site, not a registry name)
# and src/runtime/types/role_candidate.rs.

plan 22;

# --- the role group: the installed name -------------------------------------

role R { method m { 42 } }

is R.HOW.^name, 'Perl6::Metamodel::ParametricRoleGroupHOW',
    'the installed name of a role is the role GROUP';
is R.^name, 'R', 'the group reports the source-written name';

# --- an individual parametric role: the declaration expression --------------

my $decl = (role Zape2 { method z { 1 } });
is $decl.HOW.^name, 'Perl6::Metamodel::ParametricRoleHOW',
    'a role declaration expression evaluates to the INDIVIDUAL role';
is $decl.^name, 'Zape2', 'the individual role reports the source-written name';
is $decl.gist, '(Zape2)', 'and gists as an ordinary type object';

my $param-decl = (role Zape[::T] { method t { T } });
is $param-decl.HOW.^name, 'Perl6::Metamodel::ParametricRoleHOW',
    'a PARAMETERIZED role declaration expression is an individual role too';
is $param-decl.^name, 'Zape', 'and still reports the bare name';

# The declaration expression is a fully usable role type object: it composes,
# type-checks, and puns exactly like the group it belongs to.
my $mixed = 1 but $decl;
is $mixed.z, 1, 'the individual role composes with `but`';
ok $mixed ~~ Zape2, 'a value composed from the individual role does the GROUP';
ok $decl ~~ Zape2, 'the individual role type-checks against its own group';
nok $decl === Zape2, 'but it is not the same type object as the group';

# `.^candidates` hands out individual roles, not groups.
is R.^candidates.elems, 1, 'a single-declaration role group has one candidate';
is R.^candidates[0].HOW.^name, 'Perl6::Metamodel::ParametricRoleHOW',
    'a candidate is an individual parametric role';

# --- a parameterized role application: CurriedRoleHOW -----------------------

is Zape[Int].HOW.^name, 'Perl6::Metamodel::CurriedRoleHOW',
    'applying type arguments to a role group yields a curried role';

# --- punning: `.new` on a bare role gives an ordinary class -----------------

my $punned = R.new;
is $punned.m, 42, 'a punned role instance runs the role method';
is $punned.HOW.^name, 'Perl6::Metamodel::ClassHOW',
    'an instance of a punned role has an ordinary class metaclass';
is $punned.^mro[0].HOW.^name, 'Perl6::Metamodel::ClassHOW',
    'and the head of its MRO is that class, not the role group';
ok $punned ~~ R, 'the punned class still does the role';
nok $punned.WHAT === R, 'the punned class is NOT the role group type object';

# --- controls: these already matched Rakudo and must keep doing so ----------

is (class Foo { }).HOW.^name, 'Perl6::Metamodel::ClassHOW',
    'control: a class declaration expression is a ClassHOW';
class Bar does R { }
is Bar.new.HOW.^name, 'Perl6::Metamodel::ClassHOW',
    'control: an explicit class composing a role is a ClassHOW';

# --- Metamodel::*HOW.new_type mints a type with THAT metaclass --------------

my \zipi = Metamodel::ParametricRoleHOW.new_type(name => 'zape', group => 'Zape');
is zipi.HOW.^name, 'Perl6::Metamodel::ParametricRoleHOW',
    'new_type on a metaclass yields a type whose .HOW is that metaclass';
