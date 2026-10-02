use Test;

# An unmatched `(x)?` reserves a positional slot so a *later* capture keeps
# its index, but raku's capture list only extends to the last bound slot: a
# trailing unmatched optional is not an element (#10650).

plan 20;

{
    my $m = "12" ~~ / (\d) (y)? /;
    is $m.list.elems, 1, 'trailing (y)? is not an element of .list';
    is $m.elems, 1, '.elems agrees';
    is $m.caps.elems, 1, '.caps holds only the bound capture';
    is $m.pairs.elems, 1, '.pairs holds only the bound capture';
    nok $m[1].defined, '$m[1] is still undefined';
}

{
    my $seen;
    "12" ~~ / (\d) (y)? { $seen = $/.list.elems } /;
    is $seen, 1, 'the mid-match $/ in a code block drops it too';
}

{
    my $m = "1b" ~~ / (a)? (\d) (y)? (b) (z)? (q)? /;
    is $m.list.elems, 4, 'interior unmatched slots stay, trailing ones go';
    nok $m[0].defined, 'interior unmatched $0 stays undefined';
    is ~$m[3], 'b', 'later capture keeps its index';
    is $m.caps.map(*.key).join(','), '1,3', '.caps skips interior unbound slots';
}

is ("x" ~~ / (y)? x /).list.elems, 0, 'a lone unmatched optional leaves an empty list';
is ("a" ~~ / [ (a) | (b) (c) ] (d)? /).list.elems, 1,
    'alternation padding followed by an unmatched optional is dropped';
is ("a" ~~ / ((a) (b)?) /)[0].list.elems, 1, 'nested group drops its trailing unmatched optional';
is ("a" ~~ / (a) [ (b) ]? /).list.elems, 1, 'unmatched [ (b) ]? is dropped too';
is ("aa" ~~ / [ (a) (b)? ]+ /).list.elems, 2, 'a zero-iteration quantified slot is still an element';

{
    my @seen;
    "1,2" ~~ / [ (\d) ] +% ',' [ (y)? { @seen.push: $/.list.elems } ] /;
    is @seen[*-1], 1, 'trailing (y)? after a quantifier is not listed mid-match';
}

# `$N` for a dropped trailing slot is undefined, not the empty string.
{
    "1" ~~ /^ (\d+) ["-" [(\d+) || ("*")]]? $/;
    nok $1.defined, '$1 of a dropped trailing slot is undefined';
    is ($1 // "none"), "none", '... so // falls through';
    "1-3" ~~ /^ (\d+) ["-" [(\d+) || ("*")]]? $/;
    is ~$1, "3", '$1 bound when the optional group matched';
    "2" ~~ /^ (\d+) ["-" (\d+)]? $/;
    nok $1.defined, 'a stale $1 from the previous match is cleared';
}
