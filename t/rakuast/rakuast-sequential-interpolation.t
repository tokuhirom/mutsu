use v6;
use experimental :rakuast;
use Test;

# A lone `||@array` interpolation keeps its sequential flag through the
# RakuAST round trip: the array's branches are tried in order, not by LTM.

plan 4;

my $ast = Q{my @list = <x xx xxxx>; 'xxxxxxx' ~~ / ||@list /; ~$/}.AST;
my $regex = $ast.statements[1].expression.right;
ok $regex.gist.contains('sequential => True'), 'the interpolation records sequential => True';
is EVAL($ast), 'x', '||@list picks the first successful branch';

is EVAL(Q{my @l = <x xx xxxx>; 'xxxxxxx' ~~ / @l /; ~$/}.AST), 'xxxx', 'plain @list takes the longest';
is EVAL(Q{my $s = 'x'; 'xxx' ~~ / ||$s /; ~$/}.AST), 'x', 'a scalar sequential interpolation matches';
