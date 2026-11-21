use experimental :rakuast;
use Test;

# A method call at the start of a continuation line after a parenthesized
# term (`(1 ~ 2)\n    .chars`) is a dotty infix whose left side keeps its
# `Circumfix::Parentheses`, as on rakudo 2026.11 (which drops them around a
# postfix operand only); its parts are placed (#15780).

plan 8;
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
is placed("say (1 ~ 2)\n    .chars"),
    'StatementList@0-22 Statement::Expression@0-22 Call::Name::WithoutParentheses@0-22 Name@0-3 ArgList@4-22 ApplyDottyInfix@4-22 Circumfix::Parentheses@4-11 SemiList@5-10 Statement::Expression@5-10 ApplyInfix@5-10 Infix@7-8 ArgList@5-10 IntLiteral@5-6 IntLiteral@9-10 DottyInfix::Call@16-17 Call::Method@17-22 Name@17-22 ArgList@22-22',
    'a parenthesized concatenation: origins';
is-deeply "say (1 ~ 2)\n    .chars".AST.DEPARSE.EVAL, EVAL("say (1 ~ 2)\n    .chars"), 'a parenthesized concatenation: round-trips';
is placed("say (1)\n .chars"),
    'StatementList@0-15 Statement::Expression@0-15 Call::Name::WithoutParentheses@0-15 Name@0-3 ArgList@4-15 ApplyDottyInfix@4-15 Circumfix::Parentheses@4-7 SemiList@5-6 Statement::Expression@5-6 IntLiteral@5-6 DottyInfix::Call@9-10 Call::Method@10-15 Name@10-15 ArgList@15-15',
    'a parenthesized literal: origins';
is-deeply "say (1)\n .chars".AST.DEPARSE.EVAL, EVAL("say (1)\n .chars"), 'a parenthesized literal: round-trips';
is placed("my \$n = (1, 2)\n    .elems"),
    'StatementList@0-25 Statement::Expression@0-25 VarDeclaration::Simple@0-25 Initializer::Assign@6-25 ApplyDottyInfix@8-25 Circumfix::Parentheses@8-14 SemiList@9-13 Statement::Expression@9-13 ApplyListInfix@9-13 Infix@10-11 IntLiteral@9-10 IntLiteral@12-13 DottyInfix::Call@19-20 Call::Method@20-25 Name@20-25 ArgList@25-25',
    'a parenthesized list in an initializer: origins';
is-deeply "my \$n = (1, 2)\n    .elems".AST.DEPARSE.EVAL, EVAL("my \$n = (1, 2)\n    .elems"), 'a parenthesized list in an initializer: round-trips';
is placed("say ((1 + 2))\n  .Str"),
    'StatementList@0-20 Statement::Expression@0-20 Call::Name::WithoutParentheses@0-20 Name@0-3 ArgList@4-20 ApplyDottyInfix@4-20 Circumfix::Parentheses@4-13 SemiList@5-12 Statement::Expression@5-12 Circumfix::Parentheses@5-12 SemiList@6-11 Statement::Expression@6-11 ApplyInfix@6-11 Infix@8-9 ArgList@6-11 IntLiteral@6-7 IntLiteral@10-11 DottyInfix::Call@16-17 Call::Method@17-20 Name@17-20 ArgList@20-20',
    'doubled parentheses: origins';
is-deeply "say ((1 + 2))\n  .Str".AST.DEPARSE.EVAL, EVAL("say ((1 + 2))\n  .Str"), 'doubled parentheses: round-trips';
