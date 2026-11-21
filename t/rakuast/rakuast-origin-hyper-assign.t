use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: a hyper assignment
# (`@a >>+=>> 1`, a `MetaInfix::Assign` over a `MetaInfix::Hyper`) places the
# assignment over the markers, the operator and the `=`, and the `Infix`
# inside; Rakudo gives the hyper itself no origin.

plan 4;
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
is placed(Q[my ($a, $b); ($a, $b) »~=» <p q>]),
    'VarDeclaration::Signature@0-11 Signature@4-10 Parameter@4-6 ParameterTarget::Var@4-6 VarDeclaration::Simple@4-6 Parameter@8-10 ParameterTarget::Var@8-10 VarDeclaration::Simple@8-10 ApplyInfix@13-32 MetaInfix::Assign@22-26 Infix@23-24 ArgList@13-32 Circumfix::Parentheses@13-21 SemiList@14-20 ApplyListInfix@14-20 Infix@16-17 Var::Lexical@14-16 Var::Lexical@18-20 QuotedString@27-32 StrLiteral@28-31',
    'a hyper concatenation assigned, Unicode markers';
is placed(Q[my @a = 1, 2; @a >>+=>> 1]),
    'VarDeclaration::Simple@0-12 Initializer::Assign@6-12 ApplyListInfix@8-12 Infix@9-10 IntLiteral@8-9 IntLiteral@11-12 ApplyInfix@14-25 MetaInfix::Assign@17-23 Infix@19-20 ArgList@14-25 Var::Lexical@14-16 IntLiteral@24-25',
    'a hyper addition assigned, ASCII markers';
is placed(Q[my @a = 1, 2; @a «*=« (3, 4)]),
    'VarDeclaration::Simple@0-12 Initializer::Assign@6-12 ApplyListInfix@8-12 Infix@9-10 IntLiteral@8-9 IntLiteral@11-12 ApplyInfix@14-28 MetaInfix::Assign@17-21 Infix@18-19 ArgList@14-28 Var::Lexical@14-16 Circumfix::Parentheses@22-28 SemiList@23-27 ApplyListInfix@23-27 Infix@24-25 IntLiteral@23-24 IntLiteral@26-27',
    'a hyper multiplication assigned, left-pointing markers';
is placed(Q[my @a = 1, 2; @a >>~=<< <x y>]),
    'VarDeclaration::Simple@0-12 Initializer::Assign@6-12 ApplyListInfix@8-12 Infix@9-10 IntLiteral@8-9 IntLiteral@11-12 ApplyInfix@14-29 MetaInfix::Assign@17-23 Infix@19-20 ArgList@14-29 Var::Lexical@14-16 QuotedString@24-29 StrLiteral@25-28',
    'a hyper concatenation assigned, inward markers';
