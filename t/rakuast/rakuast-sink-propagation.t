use Test;

# Parsed RakuAST sink context follows StatementList and each statement's
# propagation rule. SemiList statements have no block-statement context.
plan 18;

my $decl = Q[my $x = 1; say $x].AST;
ok $decl.statements[0].sunk, 'a non-final statement is sunk';
nok $decl.statements[1].sunk, 'the final statement keeps its value';
ok $decl.statements[0].expression.sunk, 'sink reaches a declaration expression';
is $decl.statements[0].dump-markers, '⚓▪𝄞', 'sunk block statement has both markers';
is $decl.statements[1].dump-markers, '▪𝄞', 'final statement has no sink marker';

my $calls = Q[say 1; say 2].AST;
ok $calls.statements[0].expression.sunk, 'sink reaches the call';
nok $calls.statements[1].expression.sunk, 'final call is not sunk';
is $calls.statements[0].expression.dump-markers, '⚓', 'dump marks the sunk call';

my $semi = Q[(1; 2)].AST.statements[0].expression.semilist;
nok $semi.statements[0].is-block-statement, 'SemiList statement is not a block statement';
nok $semi.statements[0].sunk, 'SemiList does not sink its first statement';

my $for = Q[for ^2 { say 1 }; say 3].AST.statements[0];
ok $for.is-block-statement, 'for is a block statement';
ok $for.sunk, 'non-final for is sunk';
ok $for.body.sunk, 'sink reaches the for body';
ok $for.body.body.statement-list.statements[0].sunk, 'sink reaches the body final statement';

ok Q[loop { say 1 }].AST.statements[0].is-block-statement,
    'loop is a block statement';

my $if = Q[if True { say 1 }; say 2].AST.statements[0];
ok $if.then.sunk, 'sink reaches the if branch';
ok $if.then.body.statement-list.statements[0].sunk,
    'sink reaches the final statement of the if branch';
ok Q[while True { say 1 }; say 2].AST.statements[0].is-block-statement,
    'while inherits the loop block-statement marker';
