use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: the statements of a
# parenthesized semilist (`(1, 2; 3)`) whose terms share their shape with
# other terms of the statement are placed from a fragment parse of the text
# inside the parentheses.

plan 3;
sub placed($src) {
    my @l;
    sub w($n) {
        my $class = $n.^name.subst('RakuAST::', '');
        with $n.origin { @l.push($class ~ '@' ~ .from ~ '-' ~ .to) }
        $n.visit-children(&w);
    }
    w($src.AST);
    @l.join(' ')
}
is placed(Q[say (1,2;3), 1]),
    'StatementList@0-14 Statement::Expression@0-14 Call::Name::WithoutParentheses@0-14 Name@0-3 ArgList@4-14 Circumfix::Parentheses@4-11 SemiList@5-10 Statement::Expression@5-8 ApplyListInfix@5-8 Infix@6-7 IntLiteral@5-6 IntLiteral@7-8 Statement::Expression@9-10 IntLiteral@9-10 IntLiteral@13-14',
    'a semilist beside a term of the same shape';
is placed(Q[say (1;2), 1]),
    'StatementList@0-12 Statement::Expression@0-12 Call::Name::WithoutParentheses@0-12 Name@0-3 ArgList@4-12 Circumfix::Parentheses@4-9 SemiList@5-8 Statement::Expression@5-6 IntLiteral@5-6 Statement::Expression@7-8 IntLiteral@7-8 IntLiteral@11-12',
    'one-term statements beside a term of the same shape';
is placed(Q[my $x = (1, 2; 3), 3]),
    'StatementList@0-20 Statement::Expression@0-20 ApplyListInfix@0-20 Infix@17-18 VarDeclaration::Simple@0-17 Initializer::Assign@6-17 Circumfix::Parentheses@8-17 SemiList@9-16 Statement::Expression@9-13 ApplyListInfix@9-13 Infix@10-11 IntLiteral@9-10 IntLiteral@12-13 Statement::Expression@15-16 IntLiteral@15-16 IntLiteral@19-20',
    'a semilist in an initializer list';
