use v6;
use Test;

# `EVAL` compiles in its caller's lexical scope, so a routine the EVAL'd text
# declares sees the caller's `my &g` over an outer `sub g`, for `&g()`, a bare
# `g()` and `&g` as a value (#11154).

plan 7;

sub g() { 'sub' }

{
    my &g = -> { 'lexical' };
    is EVAL(q[sub z1 { &g() }; z1()]), 'lexical', '&g() in an EVAL-declared sub';
    is EVAL(q[sub z2 { g() }; z2()]), 'lexical', 'g() in an EVAL-declared sub';
    is EVAL(q[sub z3 { &g.name }; z3()]), '', '&g as a value in an EVAL-declared sub';
    is EVAL(q[my $c = -> { &g() }; $c()]), 'lexical', '&g() in an EVAL-declared closure';
}

sub with-param(&g) { EVAL q[sub z4 { g() }; z4()] }
is with-param(-> { 'param' }), 'param', 'a &g parameter of the EVAL caller';

is EVAL(q[sub z5 { &g() }; z5()]), 'sub', 'no caller binding: the sub';
{
    my &g = -> { 'lexical' };
    is EVAL(q[sub z6 { my &g = -> { 'own' }; &g() }; z6()]), 'own',
        'the EVAL-declared sub\'s own my &g still wins';
}
