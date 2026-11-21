use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: a statement with a
# modifier in a one-line anonymous body runs through the modifier; a bare
# `@` assigned to as a statement is a `VarDeclaration::Anonymous`; an
# indirect name called with no arguments (`GLOBAL::(q[$x])`) has a `Name`
# over the whole call and an empty `ArgList` at its end.

plan 5;
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
is placed(Q[my &f = sub ($y) { say $y if $y }]),
    'VarDeclaration::Simple@0-33 Initializer::Assign@6-33 Sub@8-33 Signature@13-15 Parameter@13-15 ParameterTarget::Var@13-15 VarDeclaration::Simple@13-15 Blockoid@17-33 Call::Name::WithoutParentheses@19-25 Name@19-22 ArgList@23-25 Var::Lexical@23-25 Var::Lexical@29-31',
    'a one-line body statement with a condition modifier';
is placed(Q[my &g = sub ($y) { say $y for 1..2 }]),
    'VarDeclaration::Simple@0-36 Initializer::Assign@6-36 Sub@8-36 Signature@13-15 Parameter@13-15 ParameterTarget::Var@13-15 VarDeclaration::Simple@13-15 Blockoid@17-36 Call::Name::WithoutParentheses@19-25 Name@19-22 ArgList@23-25 Var::Lexical@23-25 ApplyInfix@30-34 Infix@31-33 ArgList@30-34 IntLiteral@30-31 IntLiteral@33-34',
    'a one-line body statement with a loop modifier';
is placed(Q[@ = 1, 2;]),
    'ApplyInfix@0-8 Assignment@2-3 ArgList@0-8 VarDeclaration::Anonymous@0-1 ApplyListInfix@4-8 Infix@5-6 IntLiteral@4-5 IntLiteral@7-8',
    'a statement-level anonymous array';
is placed(Q[our $x = 1; say GLOBAL::(q[$x]), 17;]),
    'VarDeclaration::Simple@0-10 Initializer::Assign@7-10 IntLiteral@9-10 Call::Name::WithoutParentheses@12-35 Name@12-15 ArgList@16-35 Call::Name@16-31 Name@16-31 QuotedString@25-30 StrLiteral@27-29 ArgList@31-31 IntLiteral@33-35',
    'an indirect name called with no arguments';
is Q[@ = 1, 2;].AST.statements[0].expression.left.^name,
    'RakuAST::VarDeclaration::Anonymous', 'a bare @ at statement level is anonymous';
