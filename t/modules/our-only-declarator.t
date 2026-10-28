use Test;

# GH-15259 (annotations 0.1.0): `our only` / `my only` declare a plain routine.
plan 6;

our only f($x) { $x * 2 }
is f(3), 6, 'our only name';
our only sub g($x) { $x + 1 }
is g(3), 4, 'our only sub name';
my only sub h($x) { $x - 1 }
is h(3), 2, 'my only sub name';
our only infix:<plus2>($a, $b) { $a + $b + 2 }
is 1 plus2 2, 5, 'our only operator';
module M { our only k($x) { "k$x" } }
is M::k(1), 'k1', 'in a module, reachable qualified';
is M::.keys.sort.join(','), '&k', 'and listed in its stash';
