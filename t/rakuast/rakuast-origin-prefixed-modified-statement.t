use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: the statement a prefix
# holds with a statement modifier (`try die "x" for 1`) runs from after the
# keyword to the end of the modifier, and its expression is placed from a
# fragment parse of the text before the modifier.

plan 7;
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
is placed(Q[try (die "x") for 1]),
    'StatementList@0-19 Statement::Expression@0-19 StatementPrefix::Try@0-19 Statement::Expression@4-19 Circumfix::Parentheses@4-13 SemiList@5-12 Statement::Expression@5-12 Call::Name::WithoutParentheses@5-12 Name@5-8 ArgList@9-12 QuotedString@9-12 StrLiteral@10-11 StatementModifier::For@14-19 IntLiteral@18-19',
    'a parenthesized call under try, looped';
is placed(Q[try die "x" for 1]),
    'StatementList@0-17 Statement::Expression@0-17 StatementPrefix::Try@0-17 Statement::Expression@4-17 Call::Name::WithoutParentheses@4-11 Name@4-7 ArgList@8-11 QuotedString@8-11 StrLiteral@9-10 StatementModifier::For@12-17 IntLiteral@16-17',
    'a call under try, looped';
is placed(Q[try 1 for 1]),
    'StatementList@0-11 Statement::Expression@0-11 StatementPrefix::Try@0-11 Statement::Expression@4-11 IntLiteral@4-5 StatementModifier::For@6-11 IntLiteral@10-11',
    'a literal under try, looped';
is placed(Q[do (1) for 1]),
    'StatementList@0-12 Statement::Expression@0-12 StatementPrefix::Do@0-12 Statement::Expression@3-12 Circumfix::Parentheses@3-6 SemiList@4-5 Statement::Expression@4-5 IntLiteral@4-5 StatementModifier::For@7-12 IntLiteral@11-12',
    'a parenthesized literal under do, looped';
is placed(Q[try (1, 2) for 1]),
    'StatementList@0-16 Statement::Expression@0-16 StatementPrefix::Try@0-16 Statement::Expression@4-16 Circumfix::Parentheses@4-10 SemiList@5-9 Statement::Expression@5-9 ApplyListInfix@5-9 Infix@6-7 IntLiteral@5-6 IntLiteral@8-9 StatementModifier::For@11-16 IntLiteral@15-16',
    'a parenthesized list under try, looped';
is placed(Q[try say 1 for 1, 2]),
    'StatementList@0-18 Statement::Expression@0-18 StatementPrefix::Try@0-18 Statement::Expression@4-18 Call::Name::WithoutParentheses@4-9 Name@4-7 ArgList@8-9 IntLiteral@8-9 StatementModifier::For@10-18 ApplyListInfix@14-18 Infix@15-16 IntLiteral@14-15 IntLiteral@17-18',
    'a call under try, looped over a list';
is placed(Q[try say 1 if 1]),
    'StatementList@0-14 Statement::Expression@0-14 StatementPrefix::Try@0-14 Statement::Expression@4-14 Call::Name::WithoutParentheses@4-9 Name@4-7 ArgList@8-9 IntLiteral@8-9 StatementModifier::If@10-14 IntLiteral@13-14',
    'a call under try, conditional';
