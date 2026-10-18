use Test;

# An `EVAL` that assigns a code variable of the scope around it (`my &h`)
# stores into that variable, which keeps the new value after the EVAL; a
# routine the EVAL declares itself still stays inside it (#13657). Checked
# against rakudo.
plan 6;

my &h;
EVAL q[&h = { 1 }];
is h(), 1, 'an EVAL assigns the caller\'s my &h';

my &g = sub { 0 };
EVAL q[&g = { 2 }];
is g(), 2, 'over an existing value';
EVAL q[is g(), 2, 'a later EVAL sees the new value'];

sub setter { my &k; EVAL q[&k = { 'set' }]; k() }
is setter(), 'set', 'a routine\'s my &k';

EVAL q[sub declared-in-eval { 7 }; 1] for ^2;
pass 'an EVAL may declare the same helper sub again';
is (try EVAL q[declared-in-eval()]) // 'gone', 'gone', 'a sub the EVAL declares stays in it';
