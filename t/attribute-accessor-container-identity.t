use Test;

# `method items { @!items }` hands back the attribute's own Array -- in raku it
# IS that object -- so assigning through the accessor stores INTO that
# container; it does not rebind the attribute to a fresh one. mutsu rebuilt the
# whole attribute map around a new node, so every alias of the container went
# stale, and the container's address moved on every write.
#
# The second consequence was the cross-thread one (ADR-0068 §4 step 3): with the
# node moving, the element-store guard keyed on it locked a different stripe per
# write and excluded nothing, and `write_back_sharing`'s whole-map commit
# clobbered concurrent writes to other attributes as well. The concurrency rows
# live in `t/concurrent-attribute-element-store.t`; these are the deterministic,
# single-threaded halves.

plan 8;

# The bug as first measured: an alias taken before the assignment.
{
    class H { has @.seen; method bag() { @!seen } }
    my $h = H.new(seen => []);
    my @alias := $h.bag;
    $h.bag = (1, 2, 3);
    is-deeply @alias.List, (1, 2, 3), 'a whole-container store through a user accessor is seen by an alias';
    is-deeply $h.seen.List, (1, 2, 3), 'and by the attribute itself';
}

# The auto-generated accessor always behaved this way; pin that the user-written
# one now matches it.
{
    class H2 { has @.seen; }
    my $g = H2.new(seen => []);
    my @alias := $g.seen;
    $g.seen = (4, 5, 6);
    is-deeply @alias.List, (4, 5, 6), 'the generated accessor stores through the container too';
}

# The container object survives the store, so `===` holds across it.
{
    class H3 { has @.seen; method bag() { @!seen } }
    my $h = H3.new(seen => []);
    my $before = $h.bag;
    $h.bag = (7, 8);
    ok $before === $h.bag, 'the accessor keeps handing back the same container';
}

# An element store through the accessor lands in the same container.
{
    class H4 { has @.seen; method bag() { @!seen } }
    my $h = H4.new(seen => []);
    my @alias := $h.bag;
    $h.bag[2] = 'x';
    is @alias[2], 'x', 'an element store through a user accessor reaches the alias';
}

# The hash twin.
{
    class H5 { has %.seen; method bag() { %!seen } }
    my $h = H5.new(seen => {});
    my %alias := $h.bag;
    $h.bag = (a => 1, b => 2);
    is %alias<b>, 2, 'a whole-container store through a user hash accessor is seen by an alias';
    $h.bag<c> = 3;
    is %alias<c>, 3, 'and so is a key store';
}

# Storing the container into itself must not empty it.
{
    class H6 { has @.seen; method bag() { @!seen } }
    my $h = H6.new(seen => [1, 2, 3]);
    $h.bag = $h.bag;
    is-deeply $h.seen.List, (1, 2, 3), 'assigning the container to itself is a no-op';
}
