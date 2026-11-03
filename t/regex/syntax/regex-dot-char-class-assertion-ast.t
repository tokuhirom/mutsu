use v6;
use Test;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;

plan 12;

# `<.:Lu>`, `<.[b]>`, `<.+:N>`: a leading `.` before a character-class form is
# the class itself (rakudo builds the same tree as without the dot), not a
# non-capturing subrule call. The regex tree parser did not accept it, so `.AST`
# of the whole unit failed, and the runtime read `<.[b]>` as a subrule call.

is ~('aB' ~~ /<.:Lu>/), 'B', '<.:Lu> matches an uppercase letter';
is ~('ab' ~~ /<.[b]>/), 'b', '<.[b]> is a class';
is ~('a1' ~~ /<.+:N>/), '1', '<.+:N> adds the property';
is ~('x1' ~~ /<.-[x]>/), '1', '<.-[x]> negates';
is ~('ab' ~~ /<.[ab]>**2/), 'ab', 'the class takes a quantifier';
nok ('x' ~~ /<.:Lu>/).so, 'and rejects other characters';

my $dotted = Q[token t { <.:Lu> }].AST;
my $plain = Q[token t { <:Lu> }].AST;
lives-ok { $dotted }, '.AST of a unit with <.:Lu> succeeds';
is $dotted.statements[0].expression.body.raku, $plain.statements[0].expression.body.raku,
    'the tree is the same as for <:Lu>';
is EVAL(Q[my regex r { <.:So> }; ~('a☃' ~~ /<r>/)].AST), '☃', 'EVAL of that tree matches';
is EVAL(Q[my token t { <.[b]> }; ~('ab' ~~ /<t>/)].AST), 'b', 'also for a bracket class';
is ~('ab' ~~ /<.alpha>/), 'a', 'a non-capturing named subrule is still a subrule';
is ('a' ~~ /<.alpha>/).keys.elems, 0, 'and still captures nothing';
