use Test;

# A `for` loop's destructuring parameters in RakuAST, measured on rakudo
# 2026.09: like a signature's anonymous ones, a `Parameter` with no target
# holding the `sub-signature`, `[…]` marked `is-array`. EVAL of the tree
# unpacks each iteration value as the parsed loop does.

plan 10;

sub params($src) {
    $src.AST.statements.head.body.signature.parameters
}

my @p = params(Q[for () -> [$a, $b] { }]);
is @p.elems, 1, 'one parameter';
nok @p[0].target.defined, 'with no target';
ok @p[0].sub-signature.is-array, '`[…]` is an array sub-signature';
my @q = params(Q[for () -> ($c), [$d] { }]);
nok @q[0].sub-signature.is-array, '`(…)` is not';
ok @q[1].sub-signature.is-array, 'and the forms stay apart among several';

sub run($src) { EVAL($src.AST) }
is run(Q[my @r; for [1, 2], [3, 4] -> [$a, $b] { @r.push: $a + $b }; @r.join(',')]), '3,7',
    '`-> [$a, $b]` unpacks each element';
is run(Q[my @r; for (1, 2), (3, 4) -> ($a, $b) { @r.push: $a * $b }; @r.join(',')]), '2,12',
    '`-> ($a, $b)` does too';
is run(Q[my @r; for (1, 2), [3, 4] -> ($a, $b), [$c, $d] { @r.push: "$a$b$c$d" }; @r.join(',')]),
    '1234', 'several patterns take one element each';
is run(Q[my @r; for 1, [2, 3] -> $x, [$y, $z] { @r.push: "$x$y$z" }; @r.join(',')]), '123',
    'a pattern after a plain parameter';
is run(Q[my @r; for (a => 1, b => 2) -> Pair (:key($k), :value($v)) { @r.push: "$k=$v" }; @r.join(',')]),
    'a=1,b=2', 'a typed pattern destructures by name';
