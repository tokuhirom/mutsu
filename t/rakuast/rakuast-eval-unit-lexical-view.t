use Test;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;

plan 2;

is EVAL(Q[my $y = 6; sub f() { UNIT::<$y> }; f()].AST), 6,
    'UNIT:: in a sub sees the tree unit';
is EVAL(Q[use Test; my $y = 6; sub f() { UNIT::<$y> }; $y = 7; f()].AST), 7,
    'an import and a later write keep the tree unit visible';
