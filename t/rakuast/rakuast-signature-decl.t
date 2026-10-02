use Test;

# Signature declarations (`my ($a, @b) = …`) across the RakuAST boundary
# (ADR-10723 Stage 1). Measured on rakudo 2026.09: one
# `VarDeclaration::Signature` whose parameters are default-rw variable
# targets. This file passes under both mutsu and raku.

plan 20;

sub decl(Str $src) { $src.AST.statements[0].expression }

{
    my $d = decl(Q[my ($a, @b) = 1, 2, 3]);
    isa-ok $d, RakuAST::VarDeclaration::Signature, 'my (…) = … is a VarDeclaration::Signature';
    my @p = $d.signature.parameters;
    is @p.elems, 2, 'one parameter per element';
    is @p.map(*.target.name).join(','), '$a,@b', 'the elements as variable targets';
    ok @p[0].default-rw, 'an element defaults to a writable container';
    isa-ok $d.initializer, RakuAST::Initializer::Assign, '= is an Initializer::Assign';
    isa-ok $d.initializer.expression, RakuAST::ApplyListInfix, 'over the comma list';
}

isa-ok decl(Q[my ($a, $b) := 1, 2]).initializer, RakuAST::Initializer::Bind, ':= is an Initializer::Bind';
nok decl(Q[my ($a, $b)]).initializer, 'a bare declaration has no initializer';
like decl(Q[our ($a, $b) = 1, 2]).gist, /'scope       => "our"'/, 'our renders its scope';
unlike decl(Q[my ($a, $b) = 1, 2]).gist, /'scope'/, 'my renders none';

# Write direction: the declaration runs as written.
is EVAL(Q[my ($a, $b) = 1, 2; $a + $b].AST), 3, 'list assignment round-trips';
is EVAL(Q[my ($a, @b) = 1, 2, 3; @b.elems].AST), 2, 'an array element slurps the rest';
is EVAL(Q[my ($a, $b); $a.^name].AST), 'Any', 'a bare declaration declares';
is EVAL(Q[my ($a, $b) := (7, 8); $b].AST), 8, 'a bind round-trips';
is EVAL(Q[(my ($a, $b) = 3, 4).raku].AST), '(3, 4)', 'the declaration yields the assigned list';
is EVAL(Q[my $r; if my ($a, $b) = 1, 2 { $r = $b }; $r].AST), 2, 'in a condition';
is EVAL(Q[our ($oa, $ob) = 5, 6; $oa].AST), 5, 'our round-trips';
is EVAL(Q[sub f { state ($a, $b) = 1, 0; $b++ }; f(); f()].AST), 1, 'state assigns once';
is EVAL(Q[my $x = 5; { my ($x, $y) = $x, 2; $x.defined }].AST), False,
    'the right-hand side sees the new variables';
is EVAL(Q[my ($a, $b) = 1, 2, 3; $b].AST), 2, 'surplus values are dropped';
