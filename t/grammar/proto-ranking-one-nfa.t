use v6;
use Test;

# A call to a proto ranks its candidates by one run of one NFA built for the
# whole proto (ADR-0046, ADR-0125), as Rakudo does. What each candidate found
# in that run must be what an NFA of its own would have found: a fate, a `||`
# or an `_LL` literal belongs to the candidate whose path met it, and the rules
# the candidates share are walked once per candidate, not once for all of them.
# Each case below fails if one candidate's result leaks into another's.

plan 6;

{
    # `y` ends its declarative prefix in a fate (a code block) after 'abc', so
    # it ranks first (prefix 3) and then fails to match for real. `x` and `z`
    # are sound prefixes of 1 and 2: if `x` took over `y`'s fate it would tie
    # `y` at 3 and be tried before `z`.
    grammar Fate {
        token TOP { <t> }
        proto token t {*}
        token t:sym<x> { 'a' }
        token t:sym<y> { 'abc' { } 'X' }
        token t:sym<z> { 'ab' }
    }
    class FateActs {
        method TOP($/) { make $<t>.made }
        method t:sym<x>($/) { make 'x' }
        method t:sym<y>($/) { make 'y' }
        method t:sym<z>($/) { make 'z' }
    }
    is Fate.subparse('abc!', :actions(FateActs)).made, 'z',
        "a candidate's fate is not another candidate's";
}

{
    # All three reach 4 characters; the longest literal prefix breaks the tie
    # (3: 4, 1: 2, 2: 0), whatever order they were declared in.
    grammar Lit {
        token TOP { <t> }
        proto token t {*}
        token t:sym<c2> { <[a..z]> ** 4 }
        token t:sym<c1> { 'ab' <[a..z]> ** 2 }
        token t:sym<c3> { 'abcd' }
    }
    class LitActs {
        method TOP($/) { make $<t>.made }
        method t:sym<c1>($/) { make 'c1' }
        method t:sym<c2>($/) { make 'c2' }
        method t:sym<c3>($/) { make 'c3' }
    }
    is Lit.parse('abcd', :actions(LitActs)).made, 'c3',
        "a candidate's literals are counted for that candidate only";
}

{
    # Both candidates run through the one rule `w` and end differently: the
    # threads in `w`'s body are one per candidate, so neither is dropped for
    # being the other's.
    grammar Shared {
        token TOP { <t> }
        token w { \w }
        proto token t {*}
        token t:sym<a> { <w> 'x' }
        token t:sym<b> { <w> 'y' <w> }
    }
    class SharedActs {
        method TOP($/) { make $<t>.made }
        method t:sym<a>($/) { make 'a' }
        method t:sym<b>($/) { make 'b' }
    }
    is Shared.parse('ayz', :actions(SharedActs)).made, 'b',
        'candidates that call the same rule are ranked separately';
    is Shared.parse('ax', :actions(SharedActs)).made, 'a',
        'the other candidate wins where it is the longer';
}

{
    # A proto that calls itself through its candidates (`item` inside `list`
    # inside `item`): the recursion is cut per candidate, so the nesting
    # ranks as it would for each candidate on its own.
    grammar Nest {
        token TOP { <item> }
        proto token item {*}
        token item:sym<num>  { \d+ }
        token item:sym<list> { '[' ~ ']' [ <item>+ % ',' ] }
    }
    class NestActs {
        method TOP($/) { make $<item>.made }
        method item:sym<num>($/) { make +$/ }
        method item:sym<list>($/) { make [ $<item>.map(*.made) ] }
    }
    is-deeply Nest.parse('[1,[2,3],[4]]', :actions(NestActs)).made,
        [1, [2, 3], [4]],
        'a proto nested in its own candidates is ranked at every level';
}

{
    # A candidate that can never match here is dropped from the ranking, and
    # the ones that remain still try in order: the winner is the first that
    # matches.
    grammar Drop {
        token TOP { <t> }
        proto token t {*}
        token t:sym<no>   { 'zzz' }
        token t:sym<some> { 'ab' }
        token t:sym<more> { 'abc' 'X' }
    }
    class DropActs {
        method TOP($/) { make $<t>.made }
        method t:sym<no>($/) { make 'no' }
        method t:sym<some>($/) { make 'some' }
        method t:sym<more>($/) { make 'more' }
    }
    is Drop.subparse('abcd', :actions(DropActs)).made, 'some',
        'a candidate that fails still leaves the rest in rank order';
}
