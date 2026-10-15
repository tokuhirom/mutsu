use v6;
use experimental :rakuast;
use Test;

# A signature literal binding is an ApplyInfix over a FakeSignature (#7564, S10).

plan 8;

my $ast = Q[my ($a); :(Int $a) := \(42); $a].AST;
my $binding = $ast.statements[1].expression;
isa-ok $binding, RakuAST::ApplyInfix, 'signature binding is an infix application';
isa-ok $binding.left, RakuAST::FakeSignature, 'the left operand is a FakeSignature';
is $binding.infix.operator, ':=', 'the operator is the bind infix';
is EVAL($ast), 42, 'EVAL binds and returns the parameter value';

my $count = 0;
is EVAL(Q[my ($a); :(Int $a) := \(++$count); $a].AST), 1,
    'the binding evaluates its right-hand side once';
is $count, 1, 'the right-hand side increment ran once';

throws-like { EVAL(Q[my ($a); :(Int $a) := \("wrong"); $a].AST) },
    X::TypeCheck::Binding::Parameter,
    'a type mismatch keeps the signature binder exception';
is EVAL(Q[my ($a, $b); :($a, $b) := \(2, 3); $a + $b].AST), 5,
    'multiple untyped parameters bind';

done-testing;
