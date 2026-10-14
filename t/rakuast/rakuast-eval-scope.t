use Test;

# `EVAL` of a RakuAST tree is a compilation unit of its own (#13274), measured
# on rakudo 2026.09: the tree's `my` variables and subs are lexical to that unit
# and invisible to the caller afterwards, while an assignment to a caller's
# variable writes through, as for a string `EVAL`.

plan 9;

Q[my $q1 = 1].AST.EVAL;
throws-like Q[$q1], X::Undeclared, 'a `my` of an EVAL\'d tree does not leak into the caller';

Q[sub ev-scope-sub { 42 }].AST.EVAL;
throws-like Q[ev-scope-sub()], X::Undeclared::Symbols,
    'a sub of an EVAL\'d tree does not leak into the caller';

my $outer = 5;
Q[$outer = 7].AST.EVAL;
is $outer, 7, 'an assignment to a caller variable writes through';

my $shadow = 'outer';
Q[my $shadow = 'inner'].AST.EVAL;
is $shadow, 'outer', 'a `my` that shadows a caller variable leaves the caller\'s value';

is Q[my $z = 3; $z * 2].AST.EVAL, 6, 'the tree still sees its own `my`';
is Q[my $q1 = 10; $q1 + 1].AST.EVAL, 11, 'a later tree may declare the same name again';
is Q[my $q1 = 20; $q1 + 2].AST.EVAL, 22, '... and again';

# The same tree evaluated twice declares its variable twice, in two units.
my $ast = Q[my $twice; $twice++; $twice].AST;
is $ast.EVAL, 1, 'a tree EVAL\'d once';
is $ast.EVAL, 1, 'the same tree EVAL\'d again starts afresh';
