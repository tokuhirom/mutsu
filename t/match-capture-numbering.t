use Test;

# Regression pins for two Match capture behaviours that roast
# S05-capture/array-alias.t (whitelisted, 50/50) depends on, kept as a focused,
# fast local test so a change to capture numbering / container-kind can't
# silently regress them.

plan 8;

# (1) A named capture with an explicit `$<x>=(...)` does NOT also occupy a
#     positional slot: the following bare `(...)` groups keep their own 0-based
#     numbering. mutsu once stored the named capture in a positional slot too,
#     shifting every later positional index.
{
    ok 'abc' ~~ / $<x>=(.) (.) /, 'named + positional capture matches';
    is ~$<x>, 'a', 'named capture $<x> holds the first group';
    is $/.list.elems, 1, 'exactly one positional capture (named one is not counted)';
    is ~$0, 'b', 'positional $0 is the *second* group, not the named one';
    nok $1.defined, 'there is no positional $1';
}

# (2) `my @m = m:g/.../` gives @m an Array (not a List): its `.gist` renders with
#     square brackets `[...]` and `.elems` counts the match objects. mutsu once
#     dual-stored the receiver as a List-kind and `.gist` rendered `(...)`.
{
    my @m = 'a1b2' ~~ m:g/ (\w) (\d) /;
    is @m.elems, 2, 'two global matches land in @m';
    ok @m.gist.starts-with('['), '@m.gist renders as an Array [...] not a List (...)';
    is (~@m[0][0], ~@m[0][1]).join(','), 'a,1', 'first match keeps its positional captures';
}
