use experimental :rakuast;
use Test;

# `| ($x)`, an anonymous capture unpacked into a one-parameter sub-signature,
# is a capture parameter, as `| ($x, $y)` is. It used to be flattened into a
# plain `$x` parameter, which lost the capture in RakuAST (`.DEPARSE` printed
# `($x)`) and in the signature's arity (#14880, roast S06-multi/proto.t).

plan 9;

is Q{sub f (| ($a)) { }}.AST.statements[0].DEPARSE.chomp, 'sub f (| ($a)) { }', '.DEPARSE keeps the capture';
is Q{proto sub bar (| ($x)) {*}}.AST.statements[0].DEPARSE.chomp, 'proto sub bar (| ($x)) {*}',
    'in a proto';

sub g (| ($a)) { $a }
is g(5), 5, 'one positional binds';
nok (try g(1, 2)), 'two positionals do not';
nok (try g(a => 1)), 'a named argument does not';
is &g.arity, 0, 'the arity is the capture\'s';
is &g.count, Inf, 'and so is the count';

multi h (| (Int $x)) { 'int' }
multi h (| (Str $x)) { 'str' }
is h(1) ~ h('a'), 'intstr', 'multi dispatch through the sub-signature';

is EVAL(Q{sub k (| ($a)) { $a * 2 }; k(4)}.AST), 8, 'EVAL of the .AST runs it';
