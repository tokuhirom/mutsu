use Test;

# An assignment to something that is neither a variable nor a subscript
# (`("a" R~ "b") = 1`, `(key => $v) = 1`, `@a[0;0]:!exists = 1`,
# `try @a[0;0]:foo = 1`) is the assignment node with the target as written. Its
# lowering writes through the target's result, so the `.AST` of the program
# evaluates as the program does (#15012). Checked against rakudo 2026.09.

plan 7;

sub run($src) { EVAL($src.AST) }

is Q[my $v; (key => $v) = 1].AST.statements[1].expression.left.WHAT.^name,
    'RakuAST::Circumfix::Parentheses', 'a pair target keeps its parentheses';
isa-ok Q[("a" R~ "b") = 1].AST.statements[0].expression.infix, RakuAST::Assignment,
    'a reversed operator result is an assignment target';

throws-like { run(Q[("a" R~ "b") = 1]) }, X::Assignment::RO,
    'assigning to a reversed operator result refuses';
throws-like { run(Q[(key => 5) = 1]) }, X::Assignment::RO,
    'assigning to a pair refuses';

my @array = [[1, 2]];
throws-like { run(Q[@array[0;0]:!exists = 999]) }, X::Assignment::RO,
    'assigning to an exists adverb result refuses';
is run(Q[try @array[0;0]:foo = 999; 1]), 1,
    'a try over an adverb error target is the assignment node';
is-deeply @array, [1, 2], 'and the array is untouched';
