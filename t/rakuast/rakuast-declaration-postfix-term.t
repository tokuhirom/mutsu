use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

plan 5;

# A declaration with a postfix as a call argument: `foo(my $k.defined, 3)`.
my $ast = Q[sub f14607(|c) { c.elems }; f14607(my $k.defined, 3)].AST;
my $call = $ast.statements.tail.expression;
my $first = $call.args.args[0];
isa-ok $first, RakuAST::ApplyPostfix, 'the first argument is a postfix application';
isa-ok $first.operand, RakuAST::VarDeclaration::Simple, 'its operand is the declaration';
isa-ok $first.postfix, RakuAST::Call::Method, 'the postfix is the method call';
is EVAL($ast), 2, 'the converted call evaluates';
is EVAL(Q[sub f14607b(|c) { c.list[0] }; f14607b(my $k.defined, 3)].AST), False,
    'the declared variable is read through the postfix';
