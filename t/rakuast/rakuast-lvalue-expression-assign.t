use Test;

# Assignments to a parenthesised lvalue expression in RakuAST, measured on
# rakudo 2026.09: `(COND ?? $a !! $b) = v` and `($a || $b) = v` are a plain
# `ApplyInfix(Assignment)` whose left side is `Circumfix::Parentheses` over the
# expression. EVAL of the tree writes through the selected lvalue as the parsed
# program does.

plan 11;

my $ternary = Q[my ($x, $y); (1 ?? $x !! $y) = 99].AST.statements[1].expression;
isa-ok $ternary.infix, RakuAST::Assignment, 'a ternary lvalue assignment is an assignment';
isa-ok $ternary.left, RakuAST::Circumfix::Parentheses, 'to a parenthesised left side';
isa-ok $ternary.left.semilist.statements.head.expression, RakuAST::Ternary, 'holding the ternary';
is $ternary.right.value, 99, 'with the assigned value as the right side';

my $logic = Q[my ($u, $z); ($u || $z) = 9].AST.statements[1].expression;
isa-ok $logic.left.semilist.statements.head.expression, RakuAST::ApplyInfix,
    '`($u || $z) = 9` holds the infix application';

sub run($src) { EVAL($src.AST) }
is run(Q[my ($x, $y) = (0, 0); (1 ?? $x !! $y) = 5; "$x $y"]), '5 0', 'the true branch is written';
is run(Q[my ($x, $y) = (0, 0); (0 ?? $x !! $y) = 5; "$x $y"]), '0 5', 'the false branch is written';
is run(Q[my ($u, $z) = (0, 0); ($u || $z) = 9; "$u $z"]), '0 9', '|| writes the picked operand';
is run(Q[my ($u, $z) = (1, 0); ($u && $z) = 3; "$u $z"]), '1 3', '&& writes the picked operand';
is run(Q[my @i = 1, 2; nqp::atposref_i(@i, 0) = 5; @i.join(",")]), '5,2',
    'an nqp op result is written';
throws-like { run(Q[my ($u, $z) = (1, 0); (1 || $z) = 3]) }, X::Assignment::RO,
    'a picked non-container still refuses';
