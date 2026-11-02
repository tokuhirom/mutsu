use v6;
use lib $*PROGRAM.parent(2).add('lib-tail-block-scope');
use Test;

plan 9;

# A block-scoped `use` of a module exporting multi operator families runs on
# every call of the routine. Its later runs replay the first one; the result
# must be indistinguishable from importing again.

sub f($v) {
    use FamilyReplay;
    ("a" + "b", "x" - "y", tag($v));
}

is-deeply f("one"), ("a|b", "x~y", "tag:one"), 'first call';
is-deeply f("two"), ("a|b", "x~y", "tag:two"), 'second call';
is-deeply f("three"), ("a|b", "x~y", "tag:three"), 'third call';

# The import is still scoped: outside the routine the core operators win.
is 5 + 3, 8, 'core + outside the importing block';
is 5 - 3, 2, 'core - outside the importing block';

# Recursion shares the set between nested activations.
sub g($n) {
    use FamilyReplay;
    $n == 0 ?? "a" + "b" !! g($n - 1);
}
is g(3), "a|b", 'a recursive routine importing the family';
is g(2), "a|b", 'and again';

# A routine that is called in a loop keeps the family's fallback to the core
# operator for operands no candidate accepts.
sub h($x, $y) {
    use FamilyReplay;
    $x + $y;
}
is h(1, 2), 3, 'Int operands fall back to the core +';
is h("p", "q"), "p|q", 'Str operands take the family candidate';
