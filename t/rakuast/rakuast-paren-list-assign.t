use Test;

# An assignment to a parenthesised list in RakuAST, measured on rakudo
# 2026.09: `ApplyInfix(left => Circumfix::Parentheses(SemiList(…)),
# Assignment, right)`. EVAL of the tree assigns item by item as the parsed
# program does.

plan 9;

my $assign = Q[my ($a, $b); ($a, $b) = 1, 2].AST.statements[1].expression;
isa-ok $assign.infix, RakuAST::Assignment, '`($a, $b) = 1, 2` is an assignment';
isa-ok $assign.left, RakuAST::Circumfix::Parentheses, 'to the parenthesised list';
isa-ok $assign.right, RakuAST::ApplyListInfix, 'of the comma list';

sub run($src) { EVAL($src.AST) }
is run(Q[my ($a, $b) = 1, 2; ($a, $b) = ($b, $a); "$a $b"]), '2 1', 'a swap';
is run(Q[my ($a, @r); ($a, @r) = 10, 20, 30; "$a|@r[]"]), '10|20 30', 'an array takes the rest';
is run(Q[my $a; my %h; ($a, %h<k>) = 5, 6; "$a %h<k>"]), '5 6', 'a subscript is a target';
is run(Q[my $x; ($x, *) = 7, 8, 9; $x]), 7, 'a lone target among `*` takes one item';
is run(Q[my ($a, $b); my $n = (($a, $b) = 3, 4); "{$n.elems} $a $b"]), '2 3 4',
    'the assignment is an expression too';
is run(Q[my ($p, $q); ($p, $q) = 1; "$p {$q.defined}"]), '1 False', 'a short list leaves the rest undefined';
