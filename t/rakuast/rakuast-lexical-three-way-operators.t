use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

plan 2;

is EVAL(q[{ my &infix:<cmp> = -> $a, $b { 'local-cmp' }; 1 cmp 2 }].AST),
    'local-cmp', 'AST EVAL keeps a lexical cmp operator';
is EVAL(q[{ my &infix:<leg> = -> $a, $b { 'local-leg' }; 'a' leg 'b' }].AST),
    'local-leg', 'AST EVAL keeps a lexical leg operator';
