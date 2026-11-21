use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: a parenthesized right
# operand of an assignment the parser left unplaced (`10 R+= ($a ||= 42)`)
# runs from after the operator to the end of the application.

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
is placed(Q[my $a; 10 R+= ($a ||= 42)]),
    'VarDeclaration::Simple@0-5 ApplyInfix@7-25 MetaInfix::Reverse@10-13 MetaInfix::Assign@11-13 Infix@11-12 ArgList@7-25 IntLiteral@7-9 Circumfix::Parentheses@14-25 SemiList@15-24 ApplyInfix@15-24 MetaInfix::Assign@18-21 Infix@18-20 ArgList@15-24 Var::Lexical@15-17 IntLiteral@22-24',
    'a parenthesized assignment on the right of a reversed one';
is placed(Q[my $a = 1; $a R-= (3)]),
    'VarDeclaration::Simple@0-9 Initializer::Assign@6-9 IntLiteral@8-9 ApplyInfix@11-21 MetaInfix::Reverse@14-17 MetaInfix::Assign@15-17 Infix@15-16 ArgList@11-21 Var::Lexical@11-13 Circumfix::Parentheses@18-21 SemiList@19-20 IntLiteral@19-20',
    'a parenthesized literal on the right of a reversed assignment';
is placed(Q[my ($a, $b); $a = ($b = 2)]),
    'VarDeclaration::Signature@0-11 Signature@4-10 Parameter@4-6 ParameterTarget::Var@4-6 VarDeclaration::Simple@4-6 Parameter@8-10 ParameterTarget::Var@8-10 VarDeclaration::Simple@8-10 ApplyInfix@13-26 Assignment@16-17 ArgList@13-26 Var::Lexical@13-15 Circumfix::Parentheses@18-26 SemiList@19-25 ApplyInfix@19-25 Assignment@22-23 ArgList@19-25 Var::Lexical@19-21 IntLiteral@24-25',
    'a parenthesized assignment on the right of an assignment';
is placed(Q[my $a = 2; $a R*= ($a + 1)]),
    'VarDeclaration::Simple@0-9 Initializer::Assign@6-9 IntLiteral@8-9 ApplyInfix@11-26 MetaInfix::Reverse@14-17 MetaInfix::Assign@15-17 Infix@15-16 ArgList@11-26 Var::Lexical@11-13 Circumfix::Parentheses@18-26 SemiList@19-25 ApplyInfix@19-25 Infix@22-23 ArgList@19-25 Var::Lexical@19-21 IntLiteral@24-25',
    'a parenthesized sum on the right of a reversed assignment';
