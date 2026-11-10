use Test;
use experimental :rakuast;

plan 7;

# `die` with several arguments: the .AST holds them as separate arguments of
# the call, and EVAL of that tree concatenates them like the source does.
my $ast = Q[try { die "a", "b" }; $!.message].AST;
is $ast.gist.comb('RakuAST::ApplyListInfix').elems, 0, 'the arguments are not one comma expression';
is EVAL($ast), 'ab', 'EVAL of a block-body die joins the arguments';

is EVAL(Q[try die "a", "b"; $!.message].AST), 'ab', 'statement-prefix try joins the arguments';
is EVAL(Q[try die <x y>; $!.message].AST), 'x y', 'a lone word list is one argument';
is EVAL(Q[try { die <x y> }; $!.message].AST), 'x y', 'a lone word list in a block';
is EVAL(Q[try die (1, 2); $!.message].AST), '1 2', 'a parenthesised list is one argument';
is EVAL(Q[my @a = <p q>; try die @a; $!.message].AST), 'p q', 'an array argument';
