use v6;
use experimental :rakuast;
use Test;

plan 5;

my $node = Q[sub declared-here() { 5 } given 3].AST.statements[0];
isa-ok $node, RakuAST::Statement::Expression,
    'a sub declaration with a postfix given remains an expression statement';
isa-ok $node.expression, RakuAST::Sub,
    'the expression is the sub declaration';
isa-ok $node.loop-modifier, RakuAST::StatementModifier::Given,
    'the topic is represented by a given loop modifier';
isa-ok $node.loop-modifier.expression, RakuAST::IntLiteral,
    'the modifier retains its topic as an integer literal';
is Q[sub declared-here() { 5 } given 3; declared-here()].AST.EVAL, 5,
    'the modified declaration remains available after evaluation';
