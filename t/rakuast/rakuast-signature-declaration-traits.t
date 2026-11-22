use experimental :rakuast;
use Test;

# `my ($a, $b) is default(42)` (#18597): the group trait is a `Trait::Is`
# in the declaration's `traits`, and in each parameter's declaration, as in
# Rakudo 2026.11. `.raku` leaves it out; `visit-children` walks it, placed.

plan 9;

sub placed($src) {
    my @l;
    sub w($n) {
        my $class = $n.^name.subst('RakuAST::', '');
        with $n.origin { @l.push($class ~ '@' ~ .from ~ '-' ~ .to) unless $class ~~ /Statement/ }
        $n.visit-children(&w);
    }
    w($src.AST);
    @l.join(' ')
}

my $decl = Q[my ($a, $b) is default(42);].AST.statements[0].expression;
is $decl.traits.elems, 1, 'the declaration has one trait';
is $decl.traits[0].^name, 'RakuAST::Trait::Is', 'a Trait::Is';
is $decl.traits[0].name.canonicalize, 'default', 'named default';
is $decl.signature.parameters[0].target.declaration.traits.elems, 1,
    'a parameter declaration carries the trait';
is Q[my ($a, $b);].AST.statements[0].expression.traits.elems, 0,
    'a declaration without traits has none';
unlike $decl.raku, /Trait/, '.raku leaves the traits out';

is placed(Q[my ($a, $b) is default(42);]),
    'VarDeclaration::Signature@0-26 Signature@4-10 Parameter@4-6 ParameterTarget::Var@4-6 VarDeclaration::Simple@4-6 Trait::Is@12-26 Name@15-22 Circumfix::Parentheses@22-26 SemiList@23-25 IntLiteral@23-25 Parameter@8-10 ParameterTarget::Var@8-10 VarDeclaration::Simple@8-10 Trait::Is@12-26 Name@15-22 Circumfix::Parentheses@22-26 SemiList@23-25 IntLiteral@23-25 Trait::Is@12-26 Name@15-22 Circumfix::Parentheses@22-26 SemiList@23-25 IntLiteral@23-25',
    'the traits are visited and placed';
is placed(Q[my ($c, $d) is default(1) = 3, 4;]),
    'VarDeclaration::Signature@0-32 Signature@4-10 Parameter@4-6 ParameterTarget::Var@4-6 VarDeclaration::Simple@4-6 Trait::Is@12-25 Name@15-22 Circumfix::Parentheses@22-25 SemiList@23-24 IntLiteral@23-24 Parameter@8-10 ParameterTarget::Var@8-10 VarDeclaration::Simple@8-10 Trait::Is@12-25 Name@15-22 Circumfix::Parentheses@22-25 SemiList@23-24 IntLiteral@23-24 Initializer::Assign@26-32 ApplyListInfix@28-32 Infix@29-30 IntLiteral@28-29 IntLiteral@31-32 Trait::Is@12-25 Name@15-22 Circumfix::Parentheses@22-25 SemiList@23-24 IntLiteral@23-24',
    'after the initializer';

is EVAL(Q[my ($c, $d) is default(7) = 1; $c = Nil; "$c $d"].AST), '7 7',
    'EVAL of the tree keeps the group default';
