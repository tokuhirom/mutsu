use v6;
use experimental :rakuast;
use Test;

# `my $x OP= EXPR` is an `ApplyInfix` whose `left` is the declaration and
# whose infix is `MetaInfix::Assign` in rakudo's tree (measured on 2026.09).

plan 9;

my $infix = Q[my $p *= 5].AST.statements[0].expression;
isa-ok $infix, RakuAST::ApplyInfix, 'a compound declaration is an infix application';
isa-ok $infix.left, RakuAST::VarDeclaration::Simple, 'its left operand is the declaration';
nok $infix.left.initializer.defined, 'the declaration has no initializer of its own';
isa-ok $infix.infix, RakuAST::MetaInfix::Assign, 'the infix is the assignment metaoperator';
is $infix.infix.infix.operator, '*', 'over the written operator';

my $loop = Q[my $q += $_ for 2, 3].AST.statements[0];
isa-ok $loop.loop-modifier, RakuAST::StatementModifier::For,
    'a postfix for is the statement loop modifier';

is-deeply EVAL(Q[my $p *= 5; my Int $t += 4; my $s ~= 'x'; ($p, $t, $s)].AST),
    (5, 4, 'x'), 'compound declarations start from the neutral element';

is EVAL(Q[my $q += $_ for 2, 3; $q].AST), 5,
    'the declaration runs once before a postfix for';

is EVAL(Q[my $z **= 3; $z].AST), 1, '**= starts from 1';
