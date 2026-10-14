use v6;
use experimental :rakuast;
use Test;

# `:a<2/3>` is quote-words, so its single word is an allomorph (RatStr) even
# though a bare `<2/3>` term is a Rat; the AST evals back to the same value.

plan 5;

is EVAL(Q{(:a<2/3>).value.^name}.AST), 'RatStr', ':a<2/3> value is a RatStr after the round trip';
is EVAL(Q{(:a<1>).value.^name}.AST), 'IntStr', ':a<1> value is an IntStr';
is EVAL(Q{(:a<x>).value.^name}.AST), 'Str', ':a<x> value is a Str';
is EVAL(Q{(:a<1 2>).value.elems}.AST), 2, 'a multi-word value stays a list';
is EVAL(Q{(a => <2/3>).value.^name}.AST), 'Rat', 'a bare <2/3> term stays a Rat';
