use Test;

# `make` is `sub make(Mu \made)`: one positional, no nameds. A bare colonpair
# or identifier-keyed fat arrow is a NAMED argument, so `make :s(1)` passes
# no positional at all; parenthesize it (or use a non-identifier key) to make
# a Pair. Expectations checked against rakudo (#10523).

plan 10;

grammar G { token TOP { a } }

class Named { method TOP($/) { make :s(1) } }
throws-like { G.parse("a", :actions(Named)) }, Exception,
    message => /'Too few positionals passed; expected 1 argument but got 0'/,
    'make :s(1) in an action passes a named, not a Pair';

class Paren { method TOP($/) { make (:s(1)) } }
is-deeply G.parse("a", :actions(Paren)).made, (:s(1)), 'make (:s(1)) makes the Pair';

"a" ~~ /a/;
throws-like { make s => 1 }, Exception,
    message => /'Too few positionals passed'/, 'identifier-keyed fat arrow is named';
throws-like { make 1, :s(2) }, Exception,
    message => /"Unexpected named argument 's' passed"/, 'positional plus a named';

make "s" => 1;
is-deeply $/.made, ("s" => 1), 'string-keyed fat arrow is a positional Pair';

my $k = "t";
make $k => 2;
is-deeply $/.made, ("t" => 2), 'variable-keyed fat arrow is a positional Pair';

my $p = :u(3);
make $p;
is-deeply $/.made, (:u(3)), 'a Pair in a variable is positional';

make 5 if True;
is $/.made, 5, 'statement modifier still ends the argument list';

make [1, 2];
is-deeply $/.made, [1, 2], 'an Array argument';

make (1, 2);
is-deeply $/.made, (1, 2), 'a parenthesized list is one positional';
