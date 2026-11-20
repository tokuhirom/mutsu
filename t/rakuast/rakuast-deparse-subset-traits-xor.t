use Test;
use experimental :rakuast;

plan 9;

# A subset without traits answers an empty `traits` list, which the vendored
# deparser iterates (#14845; roast/integration/advent2012-day03.t died here).
my $bare = 'subset A where 1;'.AST.statements[0].expression;
is $bare.traits.elems, 0, 'a bare subset has no traits';
is 'subset Size where { .does(Int) or .does(Str) };'.AST.DEPARSE.lines[0],
    'subset Size where {', 'a subset with a block constraint deparses';
my $typed = 'subset B of Int where 1;'.AST.statements[0].expression;
is $typed.traits.elems, 1, 'the of trait is one of the traits';
is $typed.traits[0].^name, 'RakuAST::Trait::Of', 'as a Trait::Of';
is 'subset C of Int where 1;'.AST.DEPARSE.trim, 'subset C of Int where 1', 'a typed subset deparses';

# The loose `xor` keeps its spelling; `^^` keeps its own.
is 'say 1 xor 0;'.AST.DEPARSE.trim, 'say 1 xor 0', 'xor deparses as xor';
is '1 xor 0 xor 1'.AST.statements[0].expression.infix.operator, 'xor', 'Infix("xor")';
is '1 ^^ 0'.AST.statements[0].expression.infix.operator, '^^', 'Infix("^^")';

is-deeply EVAL('(1 xor 0), (1 xor 1), (0 xor 0)'.AST), (1, Nil, 0), 'EVAL of xor lowers to ^^';
