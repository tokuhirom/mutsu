use Test;

# ADR-15737 / #15786: a call through a `my proto sub f(|) {*}` dispatcher picks
# its candidate from a plan cached per (candidate set, argument types). Every
# observable stays what the per-call walk gave: the chosen candidate, named
# arguments, `where` clauses (value-dependent, so never planned away),
# callsame chains.

plan 15;

my proto sub f(|) {*}
my multi sub f(Int $x) { "Int($x)" }
my multi sub f(Str $x) { "Str($x)" }
my multi sub f(Int $x, Int $y) { "Int2($x,$y)" }
my multi sub f(Int $x, :$force!) { "Int+force($x)" }
my multi sub f(Any $x) { "Any" }

for ^3 {
    is f(1),         'Int(1)',       "Int (round $_)";
    is f('a'),       'Str(a)',       "Str (round $_)";
}
is f(1, 2),          'Int2(1,2)',    'two positionals';
is f(1, :force),     'Int+force(1)', 'a named argument picks its own candidate';
is f(1, :force),     'Int+force(1)', 'and again, from the plan';
is f(1.5),           'Any',          'a Rat falls to Any';

# where clauses depend on the value, not the type
my proto sub g(|) {*}
my multi sub g(Int $x where * > 10) { 'big' }
my multi sub g(Int $x) { 'small' }
is g(5),  'small', 'where: failing value';
is g(50), 'big',   'where: passing value, same argument types';
is g(5),  'small', 'where: failing value again';

# callsame through the captured candidates
my proto sub h(|) {*}
my multi sub h(Int $x) { "Int>" ~ callsame }
my multi sub h(Any $x) { "Any" }
is h(1), 'Int>Any', 'callsame defers to the next captured candidate';
is h(1), 'Int>Any', 'and again, from the plan';
