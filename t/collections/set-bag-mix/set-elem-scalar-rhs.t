use Test;

# `∈`/`(elem)` coerces its RHS to a Set. When the RHS is a plain scalar (not
# already a QuantHash, List, or Range), Rakudo's `.Set` produces a one-element
# Set containing exactly that value -- so membership reduces to `===` against
# that single value. mutsu's `set_contains` matched on the container's
# ValueView and fell through every arm (Set/Bag/Mix/Hash/List/Range) straight
# to `false` for a bare scalar, so `'simple' ∈ 'simple'` -- and, via a single
# angle-bracket word like `<simple>`, which is itself just a Str -- wrongly
# tested False.
#
# Found investigating FunctionalParsers 0.1.10's `t/17-grammar-graph.rakutest`
# (tokuhirom/mutsu#8526): `given $style { when $_.lc ∈ <simple> { ... } }`
# never matched a single-word style spec.

plan 9;

ok 'simple' ∈ 'simple', 'Str (elem) the identical Str';
nok 'simple' ∈ 'other', 'Str is not (elem) a different Str';
ok 'simple' ∈ <simple>, 'Str (elem) a single-word angle-bracket literal (itself a Str)';
ok 42 ∈ 42, 'Int (elem) the identical Int';
nok '42' ∈ 42, 'Str "42" is not (elem) the Int 42 (allomorph identity)';
nok 42 ∈ <42>, 'Int 42 is not (elem) the IntStr <42> (allomorph identity)';

my Any $undefined;
nok 'simple' ∈ $undefined, 'nothing is (elem) an undefined Any (coerces to the empty Set)';

# The reverse direction, `∋`/`(cont)`, shares the same containment logic.
ok 'simple' ∋ 'simple', 'Str (cont) the identical Str';
nok 'simple' ∋ 'other', 'Str does not (cont) a different Str';

done-testing;
