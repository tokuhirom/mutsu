use experimental :rakuast;
use Test;

# A conditional or `for` / `given` statement written in parentheses and called
# on (`(for <a b> { .uc }).join`) keeps its `Circumfix::Parentheses`, as on
# rakudo 2026.11 (a `do` statement drops them), and a statement-control
# statement in expression position is placed (#15780).

plan 15;
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
is placed(Q[(for <a b> { .uc }).join]),
    'StatementList@0-24 Statement::Expression@0-24 ApplyPostfix@0-24 Circumfix::Parentheses@0-19 SemiList@1-18 Statement::For@1-18 QuotedString@5-10 StrLiteral@6-9 Block@11-18 Blockoid@11-18 StatementList@13-16 Statement::Expression@13-16 Term::TopicCall@13-16 Call::Method@14-16 Name@14-16 ArgList@16-16 Call::Method@20-24 Name@20-24 ArgList@24-24',
    'a for loop in parentheses, called on: origins';
is Q[(for <a b> { .uc }).join].AST.statements[0].expression.operand.^name, 'RakuAST::Circumfix::Parentheses', 'a for loop in parentheses, called on: operand';
is Q[(for <a b> { .uc }).join].AST.DEPARSE.EVAL.raku, Q["AB"], 'a for loop in parentheses, called on: round-trips';
is placed(Q[(if 1 { 2 }).Str]),
    'StatementList@0-16 Statement::Expression@0-16 ApplyPostfix@0-16 Circumfix::Parentheses@0-12 SemiList@1-11 Statement::If@1-11 IntLiteral@4-5 Block@6-11 Blockoid@6-11 StatementList@8-9 Statement::Expression@8-9 IntLiteral@8-9 Call::Method@13-16 Name@13-16 ArgList@16-16',
    'an if statement in parentheses, called on: origins';
is Q[(if 1 { 2 }).Str].AST.statements[0].expression.operand.^name, 'RakuAST::Circumfix::Parentheses', 'an if statement in parentheses, called on: operand';
is Q[(if 1 { 2 }).Str].AST.DEPARSE.EVAL.raku, Q["2"], 'an if statement in parentheses, called on: round-trips';
is placed(Q[(given 1 { 2 }).Str]),
    'StatementList@0-19 Statement::Expression@0-19 ApplyPostfix@0-19 Circumfix::Parentheses@0-15 SemiList@1-14 Statement::Given@1-14 IntLiteral@7-8 Block@9-14 Blockoid@9-14 StatementList@11-12 Statement::Expression@11-12 IntLiteral@11-12 Call::Method@16-19 Name@16-19 ArgList@19-19',
    'a given statement in parentheses, called on: origins';
is Q[(given 1 { 2 }).Str].AST.statements[0].expression.operand.^name, 'RakuAST::Circumfix::Parentheses', 'a given statement in parentheses, called on: operand';
is Q[(given 1 { 2 }).Str].AST.DEPARSE.EVAL.raku, Q["2"], 'a given statement in parentheses, called on: round-trips';
is placed(Q[(unless 0 { 2 }).Str]),
    'StatementList@0-20 Statement::Expression@0-20 ApplyPostfix@0-20 Circumfix::Parentheses@0-16 SemiList@1-15 Statement::Unless@1-15 IntLiteral@8-9 Block@10-15 Blockoid@10-15 StatementList@12-13 Statement::Expression@12-13 IntLiteral@12-13 Call::Method@17-20 Name@17-20 ArgList@20-20',
    'an unless statement in parentheses, called on: origins';
is Q[(unless 0 { 2 }).Str].AST.statements[0].expression.operand.^name, 'RakuAST::Circumfix::Parentheses', 'an unless statement in parentheses, called on: operand';
is Q[(unless 0 { 2 }).Str].AST.DEPARSE.EVAL.raku, Q["2"], 'an unless statement in parentheses, called on: round-trips';
is placed(Q[(do for <a> { 1 }).join]),
    'StatementList@0-23 Statement::Expression@0-23 ApplyPostfix@0-23 StatementPrefix::Do@1-17 Statement::For@4-17 QuotedString@8-11 StrLiteral@9-10 Block@12-17 Blockoid@12-17 StatementList@14-15 Statement::Expression@14-15 IntLiteral@14-15 Call::Method@19-23 Name@19-23 ArgList@23-23',
    'a do for loop in parentheses, called on: origins';
is Q[(do for <a> { 1 }).join].AST.statements[0].expression.operand.^name, 'RakuAST::StatementPrefix::Do', 'a do for loop in parentheses, called on: operand';
is Q[(do for <a> { 1 }).join].AST.DEPARSE.EVAL.raku, Q["1"], 'a do for loop in parentheses, called on: round-trips';
