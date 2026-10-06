use Test;

# The atomic operators in RakuAST, measured on rakudo 2026.09. They are plain
# operator nodes, the atomicity being in the operator's name:
#
# - `⚛$x`      is `ApplyPrefix(Prefix "⚛", Var::Lexical)`;
# - `$x ⚛= 5`  is `ApplyInfix(Infix "⚛=")`, and `$x ⚛+= 2` the infix `"⚛+="`;
# - `$x⚛++`    is `ApplyPostfix(Postfix operator => "⚛++")`;
# - `++⚛$x`    is a prefix `"++⚛"`.
#
# mutsu's parser spells the fetch and the plain store as reserved calls that
# name the variable by a string, and the rest as calls named for the operator;
# the lowering rebuilds exactly those, so the round trip is the parsed program.

plan 29;

sub exprs($src) { $src.AST.statements.map(*.expression) }
sub run($src) { my $parsed = EVAL($src); my $round = EVAL($src.AST); ($parsed, $round) }
sub same($src, $expected, $desc) {
    my ($parsed, $round) = run($src);
    is "$parsed|$round", "$expected|$expected", $desc;
}

# --- the tree
{
    my @e = exprs(Q[my atomicint $x = 0; ⚛$x; $x ⚛= 5; $x⚛++; ++⚛$x; $x ⚛+= 2; $x⚛--; --⚛$x; $x ⚛-= 1]);
    isa-ok @e[1], RakuAST::ApplyPrefix, '`⚛$x` is an ApplyPrefix';
    is @e[1].prefix.operator, '⚛', 'with the `⚛` prefix';
    isa-ok @e[1].operand, RakuAST::Var::Lexical, 'over the variable';
    isa-ok @e[2], RakuAST::ApplyInfix, '`$x ⚛= 5` is an ApplyInfix';
    is @e[2].infix.operator, '⚛=', 'with the `⚛=` infix';
    isa-ok @e[2].left, RakuAST::Var::Lexical, 'on the variable';
    isa-ok @e[2].right, RakuAST::IntLiteral, 'of the value';
    isa-ok @e[3], RakuAST::ApplyPostfix, '`$x⚛++` is an ApplyPostfix';
    is @e[3].postfix.operator, '⚛++', 'with the `⚛++` postfix';
    isa-ok @e[4], RakuAST::ApplyPrefix, '`++⚛$x` is an ApplyPrefix';
    is @e[4].prefix.operator, '++⚛', 'with the `++⚛` prefix';
    is @e[5].infix.operator, '⚛+=', '`$x ⚛+= 2` is the `⚛+=` infix';
    is @e[6].postfix.operator, '⚛--', '`$x⚛--`';
    is @e[7].prefix.operator, '--⚛', '`--⚛$x`';
    is @e[8].infix.operator, '⚛-=', '`$x ⚛-= 1`';
    my @m = Q[my atomicint $x = 0; my $y = ⚛$x].AST.statements;
    isa-ok @m[1].expression.initializer.expression, RakuAST::ApplyPrefix, 'as an initializer';
}

# --- the round trip counts like the parsed program
same Q[my atomicint $x = 0; $x ⚛= 5; ⚛$x], 5, 'a store then a fetch';
same Q[my atomicint $x = 0; $x⚛++; $x⚛++; ⚛$x], 2, 'a postfix increment';
same Q[my atomicint $x = 0; $x⚛++], 0, 'which yields the old value';
same Q[my atomicint $x = 0; ++⚛$x], 1, 'a prefix increment yields the new one';
same Q[my atomicint $x = 5; $x⚛--; ⚛$x], 4, 'a postfix decrement';
same Q[my atomicint $x = 5; --⚛$x], 4, 'a prefix decrement';
same Q[my atomicint $x = 1; $x ⚛+= 2; ⚛$x], 3, 'an atomic add';
same Q[my atomicint $x = 9; $x ⚛-= 4; ⚛$x], 5, 'an atomic subtract';
same Q[my atomicint $x = 0; my $y = ⚛$x; $y], 0, 'a fetch as an initializer';
same Q[my atomicint $x = 0; await (^4).map({ start { $x⚛++ for ^100 } }); ⚛$x], 400,
    'increments from several threads are not lost';
same Q[my atomicint $x = 7; sub f { ⚛$x }; f()], 7, 'a fetch inside a routine';
same Q[my atomicint $x = 0; $x ⚛= 3 if True; ⚛$x], 3, 'a store under a statement modifier';
same Q[my atomicint $x = 1; atomic-fetch-inc($x); ⚛$x], 2, 'the routine forms are ordinary calls';
