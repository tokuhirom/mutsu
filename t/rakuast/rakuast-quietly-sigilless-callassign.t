use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

plan 2;

my $ast = Q[quietly my \z .= Numeric].AST;
my $prefix = $ast.statements[0].expression;
is $prefix.^name, 'RakuAST::StatementPrefix::Quietly', 'quietly is a statement prefix';
is EVAL(Q[quietly my \z .= Numeric; z].AST), 0, 'the lowered sigilless call-assignment still initializes the term';
