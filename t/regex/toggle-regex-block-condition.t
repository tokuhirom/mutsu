use Test;

# From List::MoreUtils (after.rakutest): a toggle condition that is a block
# ending in a regex literal boolifies by matching against the block's lexical
# topic, not by the Regex object being truthy.
plan 3;

my @v = <bar baz>;
my &c = { /foo/ };
is-deeply @v.toggle(&c, :off).List, (), 'regex-literal block never switches on';
is-deeply @v.toggle({ c($_) }, :off).List, (), 'wrapped regex-literal block';
is-deeply (1..6).toggle({ $_ > 2 }, :off).List, (3, 4, 5, 6), 'plain condition unchanged';
