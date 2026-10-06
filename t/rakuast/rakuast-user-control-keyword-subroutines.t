use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

plan 5;

is EVAL(Q[sub last($x) { "last $x" }; last(3)].AST), 'last 3',
    'RakuAST preserves a user sub call named last';
is EVAL(Q[sub next($x) { "next $x" }; next(4)].AST), 'next 4',
    'RakuAST preserves a user sub call named next';
is EVAL(Q[sub redo($x) { "redo $x" }; redo(5)].AST), 'redo 5',
    'RakuAST preserves a user sub call named redo';
is EVAL(Q[sub proceed($x) { "proceed $x" }; proceed(6)].AST), 'proceed 6',
    'RakuAST preserves a user sub call named proceed';
is EVAL(Q[sub return($x) { "return $x" }; return(7)].AST), 'return 7',
    'RakuAST preserves a user sub call named return';
