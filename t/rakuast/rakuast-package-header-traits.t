use Test;

# The traits written in the header of a class, role or grammar, in the order
# the source wrote them. Measured on rakudo 2026.09: `is P`, `does R`, `hides H`,
# `is rw` and a user `is NAME(ARG)` / `is NAME<WORDS>` share one `traits` list
# in source order, a parenthesised argument is a `Circumfix::Parentheses` (a
# list `(1, 2)` its comma application), an angle argument a words quote.

plan 31;

my $ast = Q:to/END/.AST;
    role R[::T] { }
    class Base { }
    class Baz { }
    multi sub trait_mod:<is>(Mu:U $c, :$mytrait!) { }
    multi sub trait_mod:<is>(Mu:U $c, :$tw!) { }
    multi sub trait_mod:<is>(Mu:U $c, :$plain!) { }
    class F1 is Base is rw is mytrait(1, 2) is tw<a b> is plain does R[Int] hides Baz { }
    class F2 does R[Int] is Base is rw { }
    class F3 hides Baz is Base { }
    class F4 { }
    role Wide is rw does R[Int] { }
    grammar G is Base does R[Int] { token TOP { a } }
    class F5 is rw { also is Base; }
    class F6 is plain is tw<x> is plain { }
    my $anon = class :: is Base does R[Int] is rw { };
    END

sub kinds($node) { $node.traits.map(*.^name.subst('RakuAST::Trait::', '')).join(' ') }
sub declared($name) {
    $ast.statements.map(*.expression).first({
        .can('name') && .name.defined && .name.canonicalize eq $name
    });
}

my $mixed = declared('F1');
is kinds($mixed), 'Is Is Is Is Is Does Hides', 'is / does / hides and flags keep written order';
isa-ok $mixed.traits[0].type, RakuAST::Type::Simple, 'a parent is a type-valued Trait::Is';
is $mixed.traits[0].type.name.canonicalize, 'Base', 'naming the parent';
is $mixed.traits[1].name.canonicalize, 'rw', '`is rw` stays where it was written';
is $mixed.traits[2].name.canonicalize, 'mytrait', 'a user trait is a named Trait::Is';
isa-ok $mixed.traits[2].argument, RakuAST::Circumfix::Parentheses, 'its argument is a parenthesised list';
is $mixed.traits[2].argument.semilist.statements[0].expression.operands.elems, 2,
    'a list argument keeps both items';
isa-ok $mixed.traits[3].argument, RakuAST::QuotedString, 'an angle argument is a words quote';
is $mixed.traits[3].argument.segments[0].value, 'a b', 'with its words';
nok $mixed.traits[4].argument.defined, 'a trait without an argument has none';
isa-ok $mixed.traits[5].type, RakuAST::Type::Parameterized, '`does R[Int]` keeps its arguments';

is kinds(declared('F2')), 'Does Is Is', 'a composed role before a parent keeps its place';
is kinds(declared('F3')), 'Hides Is', '`hides` before `is` keeps its place';
is declared('F4').traits.elems, 0, 'a class without a header trait has none';

# A role header: parents, flags and the user trait in one list.
my $role = declared('Wide');
is kinds($role), 'Is Does', 'a role header keeps its written order';
is $role.traits[0].name.canonicalize, 'rw', 'the flag first';
isa-ok $role.traits[1].type, RakuAST::Type::Parameterized, 'then the composed role';

# A grammar header.
my $grammar = declared('G');
isa-ok $grammar, RakuAST::Grammar, 'a grammar is a Grammar node';
is kinds($grammar), 'Is Does', 'its header keeps the written order';

# `also` clauses in the body are statements, not header traits.
my $also = declared('F5');
is kinds($also), 'Is', 'only the header trait is a trait';
is $also.traits[0].name.canonicalize, 'rw', 'the written `is rw`';

# A repeated trait is kept each time, in written order.
my $twice = declared('F6');
is kinds($twice), 'Is Is Is', 'a repeated trait is kept each time';
is $twice.traits.map({ .name.canonicalize }).join(','), 'plain,tw,plain', 'in written order';

my $anon = $ast.statements.tail.expression.initializer.expression;
is kinds($anon), 'Is Does Is', 'an anonymous class keeps the written order too';
isa-ok $anon.traits[1].type, RakuAST::Type::Parameterized, 'with its role arguments';
is $anon.traits[2].name.canonicalize, 'rw', 'and the trailing flag';

# The ordinary frontend hands a list argument to the trait, whole.
my $got;
multi sub trait_mod:<is>(Mu:U $c, :$listarg!) { $got = $listarg }
class WithList is listarg(1, 2) { }
is-deeply $got, (1, 2), 'a class trait receives its whole argument list';

# EVAL of a tree built from a header runs the traits it names.
my $built = 0;
multi sub trait_mod:<is>(Mu:U $c, :$builtmark!) { $built++ }
is EVAL(Q[class Built is builtmark { }; Built.^name].AST), 'Built',
    'a round-tripped class with a user trait evaluates';
is $built, 1, 'and its trait ran once';

# A parent declared by the scope that calls `.AST` is a type, not a named trait.
class CallerBase { }
my $decl = Q[class FromCaller is CallerBase { }].AST.statements[0].expression;
is $decl.traits[0].type.^name, 'RakuAST::Type::Simple', 'a caller-declared parent is a Type::Simple';
is $decl.traits[0].type.name.canonicalize, 'CallerBase', 'naming the parent';
