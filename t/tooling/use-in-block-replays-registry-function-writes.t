use v6;
use lib $*PROGRAM.parent(2).add('lib-tail-block-scope');
use Test;

plan 8;

# A module that exports a `multi trait_mod:<is>` keeps that candidate in the
# routine registry (not in the block's lexical set), so a block-scoped `use`
# of it writes the registry on every run. The replay of the first run must
# put the same candidate back each time, and the block's exit must take it out
# again (#9170).

sub f($v) {
    use TraitReplay;
    sub traced($x) is replay-traced { "body:$x" }
    (tag($v), traced($v));
}

is-deeply f("one"), ("tag:one", "traced:body:one"), 'first call';
is-deeply f("two"), ("tag:two", "traced:body:two"), 'second call';
is-deeply f("three"), ("tag:three", "traced:body:three"), 'third call replays the import';
is-deeply f("four"), ("tag:four", "traced:body:four"), 'and the fourth';

# The import is still scoped: outside the routine the trait is unknown.
throws-like { EVAL 'sub plain($x) is replay-traced { 1 }; 1' }, Exception,
    'the trait candidate does not outlive the importing block';
ok !(try EVAL 'tag("x")').defined, 'the exported routine is not visible outside either';

# A recursive routine shares the registry candidate between activations.
sub g($n) {
    use TraitReplay;
    sub leaf($x) is replay-traced { "leaf:$x" }
    $n == 0 ?? leaf("end") !! g($n - 1);
}
is g(3), "traced:leaf:end", 'a recursive routine importing the trait';
is g(2), "traced:leaf:end", 'and again after its replayed runs';
