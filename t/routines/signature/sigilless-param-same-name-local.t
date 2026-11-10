use Test;

plan 4;

# A sigilless parameter aliases the caller's variable. When the callee declares
# its own lexical with the caller variable's name, the frame-exit write-back of
# the parameter must not land in that unrelated callee binding.

sub outer(Mu \value) { middle(value) }
sub middle(Mu \value) { inner(value) }
sub inner(\object) {
    my $pair = object.pairs[0];
    my \value = $pair.value;
    value;
}

my %h = a => 1;
is inner(%h), 1, 'the callee still sees its own binding';
is-deeply %h, {a => 1}, 'the hash is untouched (direct call)';
outer(%h);
is-deeply %h, {a => 1}, 'the hash is untouched (nested sigilless params)';

my %big = a => 1, b => 2, c => 3;
sub walk(\object) {
    for object.pairs.sort(*.key) -> $pair {
        my \value = $pair.value;
    }
}
sub top(Mu \value) { walk(value) }
top(%big);
is-deeply %big, {a => 1, b => 2, c => 3}, 'a loop body declaration does not corrupt the last element';

done-testing;
