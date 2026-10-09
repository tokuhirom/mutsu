use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

# `my %h does Role` is one VarDeclaration with a `Trait::Does` and, after it,
# the initializer; the role is mixed into the fresh container before the
# initializer fills it.
# rakudo 2026.09 cannot oracle the EVAL checks below: its own EVAL of these ASTs
# does not apply the role (and rejects an initializer after a does trait), so the
# expected values are those of the equivalent plain programs.
plan 13;

my $decl = Q[role R { }; my %h does R].AST.statements[1].expression;
is $decl.^name, 'RakuAST::VarDeclaration::Simple', 'a does declaration is a variable declaration';
is $decl.sigil, '%', 'with its sigil';
is $decl.traits.elems, 1, 'and one trait';
is $decl.traits[0].^name, 'RakuAST::Trait::Does', 'the role is a does trait';
is $decl.traits[0].type.^name, 'RakuAST::Type::Simple', 'over the role type';
nok $decl.initializer.defined, 'without an initializer';

my $param = Q[role R[::T] { }; my @a does R[Int]].AST.statements[1].expression;
is $param.traits[0].type.^name, 'RakuAST::Type::Parameterized', 'a parameterized role keeps its arguments';

is EVAL(Q[role R { method m { 42 } }; my %h does R; %h.m].AST), 42, 'EVAL mixes the role into the hash';
# rakudo's own EVAL rejects a does declaration that has an initializer, so the
# initializer forms are checked as plain programs (the round-trip mode lowers
# them through the same AST).
{
    role S { method m { 42 } }
    my @a does S = 1, 2;
    is "{@a.m} {@a.join(',')}", '42 1,2', 'the initializer fills the container that has the role';
    my %h does S = a => 1;
    is "{%h.m} {%h<a>}", '42 1', 'a hash keeps both';
    our @o does S = 3;
    is @o.m, 42, 'an our declaration too';
}
is EVAL(Q[role R[::T] { method m { T.^name } }; my @a does R[Int]; @a.m].AST), 'Int',
    'a parameterized role receives its argument';
is EVAL(Q[role R { }; my %h does R; %h.WHAT.^name].AST), 'Hash+{R}', 'the container is a mixin';
