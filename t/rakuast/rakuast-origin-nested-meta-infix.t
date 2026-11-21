use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: a meta-operator nested in
# another (`R+=`, `[+]=`, `[R-]=`) places each operator inside it where its
# spelling puts it.

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
is placed(Q[my $x = 1; $x R+= 2]),
    'VarDeclaration::Simple@0-9 Initializer::Assign@6-9 IntLiteral@8-9 ApplyInfix@11-19 MetaInfix::Reverse@14-17 MetaInfix::Assign@15-17 Infix@15-16 ArgList@11-19 Var::Lexical@11-13 IntLiteral@18-19',
    'a reversed assignment operator';
is placed(Q[my $x = 1; $x R-= (2)]),
    'VarDeclaration::Simple@0-9 Initializer::Assign@6-9 IntLiteral@8-9 ApplyInfix@11-21 MetaInfix::Reverse@14-17 MetaInfix::Assign@15-17 Infix@15-16 ArgList@11-21 Var::Lexical@11-13 Circumfix::Parentheses@18-21 SemiList@19-20 IntLiteral@19-20',
    'a reversed assignment operator before parentheses';
is placed(Q[my $x = 1; $x [+]= 2]),
    'VarDeclaration::Simple@0-9 Initializer::Assign@6-9 IntLiteral@8-9 ApplyInfix@11-20 MetaInfix::Assign@14-18 BracketedInfix@14-17 Infix@15-16 ArgList@11-20 Var::Lexical@11-13 IntLiteral@19-20',
    'a bracketed operator assigned';
is placed(Q[my $x = 3; $x [R-]= 2]),
    'VarDeclaration::Simple@0-9 Initializer::Assign@6-9 IntLiteral@8-9 ApplyInfix@11-21 MetaInfix::Assign@14-19 BracketedInfix@14-18 MetaInfix::Reverse@15-17 Infix@16-17 ArgList@11-21 Var::Lexical@11-13 IntLiteral@20-21',
    'a bracketed reversed operator assigned';
