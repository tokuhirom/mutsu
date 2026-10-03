use Test;

# `|$stash` slips the stash's symbol pairs, as `|%h` does: a Stash is a Map.
# Found in UML::Translators, whose namespace walker builds
# `($packageName => $pkg, |$pkg.WHO)` and reads `.key`/`.value` of each.

plan 4;

class Outer { class Inner { }; class Two { } }
my $st = Outer.WHO;

is (|$st).map(*.key).sort.join(','), 'Inner,Two', '|$stash slips its pairs';
is ('x' => 1, |$st).map(*.key).sort.join(','), 'Inner,Two,x', 'slipped into a list literal';
is (|$st).first(*.key eq 'Inner').value.^name, 'Outer::Inner', 'pair values are the symbols';
my @l = |$st;
is @l.elems, 2, 'array assignment from a slipped stash';
