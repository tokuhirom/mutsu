use Test;

plan 21;

my $ast = Q[* ~ "x"].AST;
my $expression = $ast.statements[0].expression;
my @thunks;
$expression.visit-thunks(-> $thunk { @thunks.push($thunk) });

is @thunks.elems, 1, 'a WhateverCode expression exposes one thunk';
is @thunks[0].^name, 'RakuAST::PrimeThunk', 'the thunk has the PrimeThunk class';
is @thunks[0].thunk-kind, 'WhateverCode', 'the thunk reports its kind';
is @thunks[0].thunk-details, '⋐🔆⋑', 'the thunk reports one curry parameter';
is $expression.dump-extras(2), "  🧠 WhateverCode ⋐🔆⋑\n", 'dump-extras renders the thunk';
ok $ast.dump.contains("      🧠 WhateverCode ⋐🔆⋑\n"), 'dump includes the thunk before its children';
is EVAL($ast)('a'), 'ax', 'a viewed thunk still lowers and executes';

my $two = Q[* + *].AST.statements[0].expression;
my @two-thunks;
$two.visit-thunks(-> $thunk { @two-thunks.push($thunk) });
is @two-thunks[0].thunk-details, '⋐🔆🔆⋑', 'two curry parameters appear in details';
is EVAL(Q[* + *].AST)(2, 3), 5, 'two curry parameters keep their execution semantics';

my @plain-thunks;
Q[1 + 2].AST.statements[0].expression.visit-thunks(-> $thunk { @plain-thunks.push($thunk) });
is @plain-thunks.elems, 0, 'an ordinary expression exposes no thunks';

my $lazy = Q[my $x = 2; 1 andthen $x].AST;
my $right = $lazy.statements[1].expression.operands[1];
my @block-thunks;
$right.visit-thunks(-> $thunk { @block-thunks.push($thunk) });
is @block-thunks.elems, 1, 'a lazy dynamic operand exposes one block thunk';
is @block-thunks[0].^name, 'RakuAST::BlockThunk', 'the lazy operand has a BlockThunk';
is @block-thunks[0].thunk-kind, 'Block thunk', 'the block thunk reports its kind';
is @block-thunks[0].thunk-details, '', 'the block thunk has no details';
ok $lazy.dump.contains("        🧠 Block thunk \n"), 'dump includes the lazy operand thunk';
is EVAL($lazy), 2, 'a lazy operand still lowers and executes';

my @literal-thunks;
Q[1 andthen 2].AST.statements[0].expression.operands[1].visit-thunks(
    -> $thunk { @literal-thunks.push($thunk) }
);
is @literal-thunks.elems, 0, 'a compile-time operand needs no block thunk';
ok Q[1 orelse 2 + 3].AST.dump.contains('🧠 Block thunk'), 'orelse carries a dynamic operand thunk';
ok Q[1 notandthen 2 + 3].AST.dump.contains('🧠 Block thunk'), 'notandthen carries a dynamic operand thunk';
ok Q[1 && * + 2].AST.dump.contains('🧠 WhateverCode'), 'a curried && operand keeps its prime thunk';
ok Q[1 xx * + 2].AST.dump.contains('🧠 WhateverCode'), 'a curried xx operand keeps its prime thunk';
