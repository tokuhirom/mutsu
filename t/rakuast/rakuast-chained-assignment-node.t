use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

# A parenthesised assignment, and an assignment to one (`($x = 5) = 6`), is the
# typed assignment node: `.AST` matches rakudo 2026.09, and the tree lowers to
# what the parsed program does (ADR-10723 Stage 2, #14948). This file passes
# under both mutsu and raku.

plan 12;

sub last-statement(Str $src) { $src.AST.statements[*-1].expression }

my $chained = last-statement(Q[my $x; ($x = 5) = 6]);
is $chained.left.^name, 'RakuAST::Circumfix::Parentheses', 'an assignment target keeps its parentheses';
is $chained.left.semilist.statements[0].expression.infix.raku, 'RakuAST::Assignment.new(:item)',
    'around the inner item assignment';

my $element = last-statement(Q[my @a; (@a[0] = 5) = 7]);
is $element.left.semilist.statements[0].expression.postfix.assignee.^name, 'RakuAST::IntLiteral',
    'an element assignment inside is the subscript with its assignee';

my $list = last-statement(Q[my $x; my @r = ($x = 1, 3, 4)]).initializer.expression;
is $list.semilist.statements[0].expression.^name, 'RakuAST::ApplyListInfix',
    'a scalar assignment inside parentheses binds tighter than the comma';

my $array = last-statement(Q[my @a; my $n = (@a = 1, 2)]).initializer.expression;
is $array.semilist.statements[0].expression.right.^name, 'RakuAST::ApplyListInfix',
    'an array assignment takes the comma list';

# The trees run like the parsed program.
is EVAL(Q[my $x; ($x = 5) = 6; $x].AST), 6, 'a chained scalar assignment writes through';
is EVAL(Q[my @a = 0, 0; (@a[0] = 5) = 7; @a.join(",")].AST), '7,0', 'a chained element assignment';
is EVAL(Q[my %h; (%h<k> = 1) = 2; %h<k>].AST), 2, 'a chained hash element assignment';
is EVAL(Q[my @b = 0 xx 4; (@b[0, 1] = 1, 2) = 8, 9; ~@b].AST), '8 9 0 0', 'a chained slice assignment';
is EVAL(Q[my $x; my @r = ($x = 1, 3, 4); "$x {@r.elems}"].AST), '1 3', 'a scalar assignment in a list';
is EVAL(Q[my @a; my $n = (@a = 1, 2); @a.elems].AST), 2, 'an array assignment in parentheses';

# Rebuilding a tree from its `.raku` gives the same tree.
my $ast = Q[my @a; (@a[0] = 5) = 7].AST;
is EVAL($ast.raku).raku, $ast.raku, 'round trip';
