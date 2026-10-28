use Test;

# `my &code = EXPR or die ...` is one expression in RakuAST with the `&`
# declaration as the leftmost operand of the loose `or` (measured on rakudo
# 2026.09); the converter used to refuse it as "a declaration with a loose
# tail" (Protocol::Postgres: `my &decoder = %!decoder-for{...} or die ...`).

plan 4;

my $ast = Q[my &d = &say or die "x"].AST;
my $expr = $ast.statements.head.expression;
isa-ok $expr, RakuAST::ApplyInfix, 'the statement is one infix application';
isa-ok $expr.left, RakuAST::VarDeclaration::Simple, 'with the declaration as its left operand';
is $expr.left.sigil, '&', 'of the & sigil';

sub f { 42 }
is EVAL(Q[my &d = &f or die "x"; d()].AST), 42, 'and it still runs after the round trip';
