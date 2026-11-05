use Test;

# #14916 (accessor survey): `RakuAST::ArgList`'s `from-comma-list` (a class
# method), `unshift` (a mutator that answers the node it added, like `push`)
# and `invocant`, as on rakudo 2026.09.

plan 17;

sub n($v) { RakuAST::IntLiteral.new($v) }
sub comma(*@ops) {
    RakuAST::ApplyListInfix.new(infix => RakuAST::Infix.new(','), operands => @ops)
}

# from-comma-list takes the operands of a list infix.
my $list = RakuAST::ArgList.from-comma-list(comma(n(1), n(2)));
isa-ok $list, RakuAST::ArgList, 'from-comma-list builds an ArgList';
is $list.arity, 2, 'one argument per operand';
is $list.DEPARSE, '1, 2', 'and they deparse as the comma list';
is RakuAST::ArgList.from-comma-list(comma()).arity, 0, 'an empty comma list gives an empty ArgList';
is RakuAST::ArgList.from-comma-list(
    RakuAST::ApplyListInfix.new(infix => RakuAST::Infix.new('+'), operands => (n(1), n(2)))
).arity, 2, 'the infix is not checked, only the node class';

throws-like { RakuAST::ArgList.from-comma-list(n(7)) }, X::TypeCheck::Binding::Parameter,
    'a node that is not an ApplyListInfix is rejected';
throws-like { RakuAST::ArgList.from-comma-list(Nil) }, X::TypeCheck::Binding::Parameter,
    'so is Nil';

# unshift puts the argument first, in place, and answers it.
my $args = RakuAST::ArgList.new(n(1), n(2));
my $x = RakuAST::Var::Lexical.new('$x');
my $ret = $args.unshift($x);
ok $ret === $x, 'unshift answers the added node';
is $args.arity, 3, 'the list grew';
is $args.arg-at-pos(0).raku, $x.raku, 'the node is first';
is $args.DEPARSE, '$x, 1, 2', 'and deparses first';
my $other = RakuAST::ArgList.new;
$other.unshift($x);
is $other.arity, 1, 'unshift onto an empty list';
$args.unshift($x);
is $args.DEPARSE, '$x, $x, 1, 2', 'unshift twice';

# invocant: an ArgList has none; rakudo answers the Expression role.
is $args.invocant.^name, 'RakuAST::Expression', 'invocant is the Expression type object';
ok !$args.invocant.defined, 'which is a type object';
ok RakuAST::ArgList.new.invocant === $args.invocant, 'the same for every list';
my $parsed = 'say(1, 2)'.AST.statements[0].expression.args;
is $parsed.invocant.^name, 'RakuAST::Expression', 'also for a parsed call';
