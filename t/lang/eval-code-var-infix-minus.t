use v6;
use Test;

# `&infix:<->` is the symbolic subtraction operator, not a word-shaped infix
# nothing declares: EVAL (string or AST) resolves it like `&infix:<+>`.

plan 6;

is EVAL('&infix:<->(5, 2)'), 3, 'EVAL string: &infix:<->(...)';
is EVAL('my &f = &infix:<->; f(9, 4)'), 5, 'EVAL string: bound to a code variable';
is EVAL(Q{&infix:<->(7, 2)}.AST), 5, 'EVAL of the AST';
is EVAL('&infix:<*>(2, 3)'), 6, 'a neighbouring symbolic operator';
throws-like { EVAL('&infix:<not-an-op>') }, X::Undeclared::Symbols,
    'an undeclared word-shaped infix is still refused';
ok EVAL('&infix:<->').defined, 'the bare reference is defined';
