use Test;
use experimental :rakuast;

plan 4;

my $e = Q[{ my enum Index <r w x e d>; e.Int }].AST;
is $e.^name, 'RakuAST::StatementList', 'the enum unit parses to RakuAST';
is EVAL($e), 3, 'an enum word shadows the setting e term';

my $constants = Q[{ my enum Constants «pi tau»; pi.Int + tau.Int }].AST;
is EVAL($constants), 1, 'quoted words shadow both math terms';
is EVAL(Q[{ my enum Short <a e>; e.Int }].AST), 1,
    'the shadowed enum word keeps its declared ordinal';
