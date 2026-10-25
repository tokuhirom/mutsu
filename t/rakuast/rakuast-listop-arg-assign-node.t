use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

# An assignment in a no-paren call argument (`one $x = 2 > 1 ?? "P" !! "Q"`)
# is the typed assignment node, like every other `=`: `.AST` matches rakudo
# 2026.09, and the tree lowers to what the parsed program does (ADR-10723
# Stage 2, #14948). Multi-dimensional subscript assignments keep their shape.
# This file passes under both mutsu and raku.

plan 9;

sub last-statement(Str $src) { $src.AST.statements[*-1].expression }

my $arg = last-statement(Q[sub one($x) { $x }; my $x; one $x = 2 > 1 ?? "P" !! "Q"]).args.args[0];
is $arg.^name, 'RakuAST::ApplyInfix', 'the argument is an infix application';
is $arg.infix.^name, 'RakuAST::Assignment', 'of the assignment';
is $arg.right.^name, 'RakuAST::Ternary', 'whose right side is the whole ternary';

is last-statement(Q[my @a; @a[0;1] = 5]).infix.^name, 'RakuAST::Assignment',
    'a multi-dimensional subscript assignment';

# The trees run like the parsed program.
is EVAL(Q[sub one($x) { $x }; my $x; one $x = 2 > 1 ?? "P" !! "Q"].AST), 'P',
    'the call receives the assigned value';
is EVAL(Q[sub one($x) { $x }; my $x; one $x = 2 > 1 ?? "P" !! "Q"; $x].AST), 'P',
    'the variable holds it';
is EVAL(Q[my @a; @a[0;1] = 5; @a[0][1]].AST), 5, 'a multi-dimensional subscript assignment';
is EVAL(Q[my @a; @a[0;1] = 5; my $r = (@a[1;0] = 7); $r].AST), 7, '... in parentheses';

# Rebuilding a tree from its `.raku` gives the same tree.
my $ast = Q[sub one($x) { $x }; my $x; one $x = 1].AST;
is EVAL($ast.raku).raku, $ast.raku, 'round trip';
