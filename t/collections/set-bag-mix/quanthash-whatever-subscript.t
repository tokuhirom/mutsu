use v6;
use Test;

# `%quanthash{*}` (whatever subscript) returns ALL values of a Set/Bag/Mix:
# - Set: each element's membership value (True)
# - Bag/Mix: the element weights
# Previously a Bag/Mix `{*}` fell through to the scalar-key arm and returned 0
# (the Whatever `*` stringified to a non-existent key).

plan 10;

# Bag: all weights (order is unordered, so compare sorted).
{
    my $bag = (orange => 1, apple => 3).Bag;
    is-deeply $bag{*}.sort.List, (1, 3), 'Bag{*} returns all weights';
    is $bag{*}.sum, 4, 'Bag{*} weights sum to total';
    is $bag{*}.WHAT.^name, 'List', 'Bag{*} is a List';
    is-deeply $bag{()}.List, ().List, 'Bag{()} is empty';
}

# Set: all membership values are True.
{
    my $set = <a b c>.Set;
    is $set{*}.elems, 3, 'Set{*} has one value per element';
    ok $set{*}.all === True, 'Set{*} values are all True';
}

# Mix: all weights (may be fractional).
{
    my $mix = (a => 1.5, b => 2.5).Mix;
    is-deeply $mix{*}.sort.List, (1.5, 2.5), 'Mix{*} returns all weights';
    is $mix{*}.sum, 4.0, 'Mix{*} weights sum';
}

# Bound-hash form (the doc example uses `:=`).
{
    my %bag := (x => 2, y => 5).Bag;
    is-deeply %bag{*}.sort.List, (2, 5), 'bound Bag{*} returns all weights';
}

# Plain Hash {*} still returns values (no regression).
{
    my %h = a => 1, b => 2;
    is-deeply %h{*}.sort.List, (1, 2), 'plain Hash{*} unaffected';
}
