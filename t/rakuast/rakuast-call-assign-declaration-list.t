use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

plan 6;

# `my Int $c .= new(), 5` is one comma application whose first operand is the
# `.=` declaration (RakuAST::Initializer::CallAssign).
my $ast = Q[my Int $c .= new(), 5, 6].AST;
my $list = $ast.statements.tail.expression;
isa-ok $list, RakuAST::ApplyListInfix, 'the statement is one comma application';
is $list.operands.elems, 3, 'the declaration and both list items are operands';
isa-ok $list.operands[0], RakuAST::VarDeclaration::Simple, 'the declaration leads';
isa-ok $list.operands[0].initializer, RakuAST::Initializer::CallAssign,
    'the .= initializer is kept';

is EVAL(Q[my Int $d .= new(); $d].AST), 0, 'the declaration alone still round-trips';
is EVAL(Q[my Int $e .= new(), 5; $e].AST), 0, 'the declaration with list items round-trips';
