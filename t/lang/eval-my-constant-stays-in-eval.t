use Test;

# A `my constant` at an `EVAL`'s top level is the EVAL's own lexical: a later
# `EVAL` does not see it, whatever the case of its name, and a same-named
# constant of the caller is left alone. Checked against rakudo.

plan 4;

EVAL q[my constant KONST = 5; 1];
is (try EVAL q[KONST]) // 'gone', 'gone', 'an uppercase `my constant` stays in its EVAL';
EVAL q[my constant konst = 5; 1];
is (try EVAL q[konst]) // 'gone', 'gone', 'so does a lowercase one';

my constant KEPT = 1;
EVAL q[my constant KEPT = 5; 1];
is KEPT, 1, 'the caller\'s same-named constant is untouched';
is EVAL(q[KEPT]), 1, 'and a later EVAL sees the caller\'s';
