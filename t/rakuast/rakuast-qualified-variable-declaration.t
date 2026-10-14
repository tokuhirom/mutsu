use v6;
use Test;

# `my $pkg::x` declares a package-qualified variable. Measured on rakudo
# 2026.09, its desigilname is `Name.from-identifier-parts("pkg", "x")`, the
# declaration creates the stub package `pkg`, and a later `OUR::pkg` / `MY::A`
# over a name that resolves is a `Term::Name` of the qualified name.

plan 9;

my $decl = Q[my $qpkg::member = 7].AST.statements[0].expression;
is $decl.^name, 'RakuAST::VarDeclaration::Simple', 'a qualified declaration is VarDeclaration::Simple';
is $decl.desigilname.canonicalize, 'qpkg::member', 'its desigilname is the qualified name';
is $decl.desigilname.parts.elems, 2, '... in two parts';

my $term = Q[my $qpkg2::member = 7; OUR::qpkg2].AST.statements[1].expression;
is $term.^name, 'RakuAST::Term::Name', 'OUR::pkg of the stub package is a Term::Name';
is Q[class QA { }; MY::QA].AST.statements[1].expression.^name, 'RakuAST::Term::Name',
    'MY::A of a declared class is a Term::Name';

# EVAL of the round-tripped trees.
is EVAL(Q[my $qpkg3::member = 7; $qpkg3::member].AST), 7,
    'EVAL of a qualified declaration reads the variable back';
is EVAL(Q[my @qpkg4::list = 1, 2; @qpkg4::list.elems].AST), 2,
    'an array works the same way';
is EVAL(Q[my $qpkg5::member = 7; OUR::qpkg5.WHO.keys.join].AST), '$member',
    'the stub package carries the variable';

# Semantics of the parsed program.
my $qpkg6::member = 9;
is OUR::qpkg6.WHO<$member>, 9, 'the variable lives in the stub package';
