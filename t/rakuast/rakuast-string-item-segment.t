use Test;

# `"a$(EXPR)b"` in RakuAST, measured on rakudo 2026.09: the interpolated
# `$(...)` is a `Contextualizer::Item` over a `StatementSequence` holding one
# `Statement::Expression`, between the `StrLiteral` segments. EVAL of the tree
# interpolates the value as the parsed program does.

plan 12;

sub segments($src) { $src.AST.statements[0].expression.args.args[0].segments }

my @seg = segments(Q[say "a$(1 + 2)b"]);
is @seg.elems, 3, 'three segments';
isa-ok @seg[0], RakuAST::StrLiteral, 'the text before is a StrLiteral';
isa-ok @seg[1], RakuAST::Contextualizer::Item, '`$(...)` is a Contextualizer::Item';
isa-ok @seg[1].target, RakuAST::StatementSequence, 'holding a StatementSequence';
isa-ok @seg[1].target.statements[0], RakuAST::Statement::Expression, 'of one statement';
isa-ok @seg[1].target.statements[0].expression, RakuAST::ApplyInfix, 'whose expression is the infix';
isa-ok @seg[2], RakuAST::StrLiteral, 'the text after is a StrLiteral';

sub run($src) { EVAL($src.AST) }
is run(Q["a$(1 + 2)b"]), 'a3b', 'the value is interpolated';
is run(Q["$(1, 2)"]), '1 2', 'a list is stringified itemized';
is run(Q[my $x = 4; "$($x * 2)"]), '8', 'a variable expression';
is run(Q[sub f { 'r' }; "<$(f())>"]), '<r>', 'a call';
is run(Q["{ 1 + 2 }$(3)"]), '33', 'next to a code block segment';
