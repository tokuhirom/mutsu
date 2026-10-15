use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

plan 2;

my $ast = Q[supply { emit 42; done }].AST;
is $ast.statements[0].expression.^name, 'RakuAST::StatementPrefix::Supply', 'a supply body remains a RakuAST supply prefix';
is EVAL($ast).list, [42], 'the lowered supply emits the done payload before completion';
