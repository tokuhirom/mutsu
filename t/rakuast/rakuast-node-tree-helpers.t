use Test;
use experimental :rakuast;

# The tree helpers rakudo 2026.09 gives every RakuAST::Node: `map`, `grep`
# and `first` walk the descendants (pre-order, or post-order with
# `:depth-first`), `find-nodes` walks breadth first. This file passes under
# both mutsu and raku.

plan 18;

my $a = Q[say 1 + 2; say 3].AST;

is $a.map({ .^name }).elems, 14, 'map visits every descendant';
is $a.map(*.^name).head(2).join(' '),
    'RakuAST::Statement::Expression RakuAST::Call::Name::WithoutParentheses', 'map is pre-order';
is $a.map(*.^name, :depth-first).head(3).join(' '),
    'RakuAST::Name RakuAST::Infix RakuAST::IntLiteral', 'map :depth-first is post-order';
is $a.map({ $_ ~~ RakuAST::IntLiteral ?? .value !! Empty }).join(','), '1,2,3',
    'Empty results are dropped';

is $a.grep(RakuAST::IntLiteral).map(*.value).join(','), '1,2,3', 'grep smartmatches';
is $a.grep(RakuAST::IntLiteral, :k).join(','), '0,1,2', 'grep :k';
is $a.grep(RakuAST::IntLiteral, :kv).map({ $_ ~~ Int ?? $_ !! .value }).join(','),
    '0,1,1,2,2,3', 'grep :kv';
is $a.grep(RakuAST::IntLiteral, :p).map({ .key ~ '=' ~ .value.value }).join(','),
    '0=1,1=2,2=3', 'grep :p';

is $a.first(RakuAST::IntLiteral).value, 1, 'first';
is $a.first(RakuAST::IntLiteral, :end).value, 3, 'first :end';
is $a.first(RakuAST::ApplyInfix).infix.operator, '+', 'first by type';
is $a.first(RakuAST::Call::Name, :end).args.args[0].value, 3, 'first :end of a class';
nok $a.first(RakuAST::Block).defined, 'first without a match is Nil';

is $a.find-nodes(RakuAST::IntLiteral).elems, 3, 'find-nodes';
is $a.find-nodes(RakuAST::IntLiteral, :condition(*.value > 1)).map(*.value).join(','), '3,2',
    'find-nodes is breadth first, with a condition';
is $a.find-nodes(RakuAST::IntLiteral, :stopper(RakuAST::ApplyInfix)).map(*.value).join(','), '3',
    'a type stopper does not look below a match';
is $a.find-nodes(RakuAST::IntLiteral, :stopper({ $_ ~~ RakuAST::ApplyInfix })).map(*.value).join(','),
    '3', 'a code stopper skips the subtree';
is $a.find-nodes(RakuAST::IntLiteral).^name, 'List', 'find-nodes returns a List';
