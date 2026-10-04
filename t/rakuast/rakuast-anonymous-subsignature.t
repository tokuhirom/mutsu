use Test;

# Anonymous destructuring parameters in RakuAST, measured on rakudo 2026.09:
# a `Parameter` with no target holding the `sub-signature`. `[…]` marks that
# signature `is-array`; `(…)` carries the parameter's type (the implicit
# `Any` of a sub signature, or the written one). EVAL of the tree binds as the
# parsed program does.

plan 12;

sub param($src) {
    $src.AST.statements.head.expression.signature.parameters.head
}

my $bracket = param(Q[sub f([$a, $b]) { }]);
nok $bracket.target.defined, '`[…]` has no target';
ok $bracket.sub-signature.is-array, 'and its sub-signature is an array one';
is $bracket.sub-signature.parameters.elems, 2, 'holding both parameters';

my $paren = param(Q[sub g(($x, $y)) { }]);
nok $paren.target.defined, '`(…)` has no target either';
nok $paren.sub-signature.is-array, 'nor an array sub-signature';
is param(Q[sub p(Pair (:key($k))) { }]).type.name.canonicalize, 'Pair',
    'a written type is the parameter\'s';
isa-ok param(Q[sub cap(| ($a, $b)) { }]).slurpy, RakuAST::Parameter::Slurpy::Capture,
    '`| (…)` is a capture with a sub-signature';

is EVAL(Q[sub f([$a, $b]) { "$a-$b" }; f([1, 2])].AST), '1-2', '`[…]` binds';
is EVAL(Q[sub g(($x, $y)) { $x + $y }; g((3, 4))].AST), 7, '`(…)` binds';
is EVAL(Q[my &h = -> [$p, $q], ($r) { "$p$q$r" }; h([5, 6], (7,))].AST), '567',
    'both forms in a pointy block';
is EVAL(Q[sub p(Pair (:key($k), :value($v))) { "$k=$v" }; p((a => 1))].AST), 'a=1',
    'a typed one destructures by name';
is EVAL(Q[sub cap(| ($a, $b)) { "$a $b" }; cap(1, 2)].AST), '1 2',
    'and a capture destructures its arguments';
