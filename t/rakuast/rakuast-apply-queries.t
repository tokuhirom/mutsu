use Test;

# `operator`, `operands` and `return-type` of the operator applications
# (`ApplyInfix`, `ApplyPrefix`, `ApplyPostfix`, `ApplyListInfix`), measured on
# rakudo 2026.09 (#14916).

plan 17;

use experimental :rakuast;

sub names(@items) { @items.map(*.^name.subst('RakuAST::', '')).join('/') }

my $u = q[my ($x, $a, $b) = 1,2,3; 1 + 2; -3; $x++; 4 + 5 + 6; $a.foo(1); !$b; (1,2) Z (3,4);].AST;
my @e = $u.statements.skip(1).map(*.expression);

is names(@e[0].operands), 'IntLiteral/IntLiteral', 'infix operands';
is @e[0].operator.^name, 'RakuAST::Infix', 'infix operator';
is @e[0].return-type.^name, 'Mu', 'infix return-type';
is names(@e[1].operands), 'IntLiteral', 'prefix operands';
is @e[1].operator.^name, 'RakuAST::Prefix', 'prefix operator';
is names(@e[2].operands), 'Var::Lexical', 'postfix operands';
is @e[2].operator.^name, 'RakuAST::Postfix', 'postfix operator';
is names(@e[3].operands), 'ApplyInfix/IntLiteral', 'a left-nested infix chain';
is @e[4].operator.^name, 'RakuAST::Call::Method', 'a method call is a postfix operator';
is names(@e[4].operands), 'Var::Lexical', 'its operand is the invocant';
is names(@e[5].operands), 'Var::Lexical', 'a negation operand';
is @e[6].^name, 'RakuAST::ApplyListInfix', 'a list infix';
is names(@e[6].operands), 'Circumfix::Parentheses/Circumfix::Parentheses', 'list infix operands';
is @e[6].return-type.^name, 'Mu', 'list infix return-type (inherited)';

# Hand-built.
my $n = RakuAST::ApplyInfix.new(
    left => RakuAST::IntLiteral.new(1), infix => RakuAST::Infix.new("+"),
    right => RakuAST::IntLiteral.new(2));
is $n.operator.^name, 'RakuAST::Infix', 'hand-built: operator';
is $n.operands.elems, 2, 'hand-built: two operands';
is $n.return-type.^name, 'Mu', 'hand-built: return-type';
