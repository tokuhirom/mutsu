use Test;

# `sub f(Inf)` / `sub f(NaN)` are literal-value parameters: rakudo 2026.09's
# tree is a Num parameter with that value. mutsu rendered a type named `Inf`.
# This file passes under both mutsu and raku.

plan 6;

my $p = Q[sub f(Inf) {}].AST.statements[0].expression.signature.parameters[0];
is $p.type.name.canonicalize, 'Num', 'the type is Num';
is $p.value, Inf, 'the value is Inf';
ok Q[sub f(NaN) {}].AST.statements[0].expression.signature.parameters[0].value.isNaN,
    'NaN';

my $multi = Q[
    multi g(Inf) { 'inf' }
    multi g(NaN) { 'nan' }
    multi g($x) { 'other' }
    (g(Inf), g(NaN), g(1)).join(',')
];
is EVAL($multi.AST), 'inf,nan,other', 'the parsed tree round-trips';
is EVAL($multi), 'inf,nan,other', 'the same as the ordinary frontend';
is EVAL(Q[sub h(Inf) { 'ok' }; h(Inf)].AST), 'ok', 'a single literal routine';
