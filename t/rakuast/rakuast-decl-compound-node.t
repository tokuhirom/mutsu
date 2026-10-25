use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

# `my $x OP= EXPR` and a private attribute's `has $!g OP= EXPR` are the typed
# declaration compound node: `.AST` makes the declaration the assignment's left
# operand, as rakudo 2026.09 does, over a built-in, word or user infix, and
# the tree runs like the parsed program (ADR-10723 Stage 2, #14948). This file
# passes under both mutsu and raku.

plan 8;

sub first-expression(Str $src) { $src.AST.statements[*-1].expression }

is first-expression(Q[my $n mod= 5]).infix.infix.operator, 'mod', 'a word infix';
is first-expression(Q[sub infix:<+%+>($a, $b) { $a + $b * 2 }; my $x +%+= 5]).infix.infix.operator,
    '+%+', 'a user infix';
my $attr = Q[class A { has $!g //= 5 }].AST.statements[0].expression.body.body.statement-list.statements[0].expression;
is $attr.left.scope, 'has', 'an attribute is the left operand';
is $attr.infix.infix.operator, '//', 'of its compound assignment';

# The parsed program and its tree agree.
for (
    Q[my $p *= 5; $p],
    Q[my $s ~= "x"; $s],
    Q[my $q += $_ for 2, 3; $q],
) -> $src {
    is EVAL($src.AST), EVAL($src), "the tree runs like the program: $src";
}

# Rebuilding a tree from its `.raku` gives the same tree.
my $ast = Q[my $n mod= 5].AST;
is EVAL($ast.raku).raku, $ast.raku, 'round trip';
