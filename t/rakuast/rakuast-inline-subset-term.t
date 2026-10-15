use v6;
use Test;

plan 4;

my $ast = Q[42 ~~ (subset :: where Int|Str)].AST;
ok $ast.gist.contains('RakuAST::Type::Subset'),
    'read direction keeps an inline subset as a subset declaration';
is EVAL($ast), True, 'EVAL lowers the inline subset term and matches Int';
is EVAL(Q[4.2 ~~ (subset :: where Int|Str)].AST), False,
    'the round trip keeps the subset predicate';
is EVAL(Q[my $s = subset :: of Real where * > 3; 5 ~~ $s].AST), True,
    'the round trip returns the declared subset type object';

done-testing;
