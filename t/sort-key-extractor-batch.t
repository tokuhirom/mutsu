use Test;

plan 23;

# `.sort(&key-extractor)` extracts all keys with one batched `.map` when that is
# faithful, and falls back to a call per element otherwise (`sort_keys_batched`).
# These pin that both routes agree with each other and with raku across every
# callable shape, element shape and adverb the batch form has to get right.

my @words = <banana Apple cherry date>;

is-deeply @words.sort(*.chars).List, ('date', 'Apple', 'banana', 'cherry'),
    'WhateverCode key extractor';
is-deeply @words.sort({ $_.chars }).List, ('date', 'Apple', 'banana', 'cherry'),
    'bare block key extractor';
is-deeply @words.sort(-> $x { $x.chars }).List, ('date', 'Apple', 'banana', 'cherry'),
    'pointy block key extractor';
is-deeply @words.sort({ $^x.chars }).List, ('date', 'Apple', 'banana', 'cherry'),
    'placeholder block key extractor';
is-deeply @words.sort({ .lc }).List, ('Apple', 'banana', 'cherry', 'date'),
    'bare-method mapper block';

# The key extractor is called exactly once per element (a Schwartzian
# transform), not once per comparison.
{
    my $calls = 0;
    my @s = (1..100).reverse.sort({ $calls++; $_ });
    is $calls, 100, 'key extractor runs exactly once per element';
    is-deeply @s[0, 99].List, (1, 100), 'and still sorts correctly';
}

# Sort is stable across equal keys.
is-deeply <bb aa cc a>.sort(*.chars).List, ('a', 'bb', 'aa', 'cc'),
    'equal keys keep their original order';

# Element shapes the batch form must not mangle.
my %h = a => 3, b => 1, c => 2;
is-deeply %h.sort(*.value).map(*.key).List, ('b', 'c', 'a'),
    'Pair elements (hash sort) by value';
is-deeply %h.sort({ .value }).map(*.key).List, ('b', 'c', 'a'),
    'Pair elements via a bare-method block';
is-deeply %h.sort(-*.value).map(*.key).List, ('a', 'c', 'b'),
    'Pair elements, negated key';
is-deeply (1, 2).Set.sort(*.key).map(*.key).List, (1, 2), 'Set decomposes to pairs';

# A key extractor returning a Slip must not be flattened into the key vector.
is-deeply <a bb>.sort({ slip($_.chars, 9) }).List, ('a', 'bb'), 'Slip-returning key extractor';

# `$_` inside a WhateverCode is the CALLER's topic, not the element.
{
    $_ = 2;
    is-deeply <x yy zzz>.sort(*.chars == $_).List, ('x', 'zzz', 'yy'),
        'WhateverCode key extractor sees the outer topic';
}

# Adverbs and negative / non-numeric keys.
is-deeply <ccc a bb>.sort(*.chars, :k).List, (1, 2, 0), ':k returns source indices';
is-deeply (1..5).sort(-*).List, (5, 4, 3, 2, 1), 'negated numeric key';
is-deeply <a bb ccc>.sort({ -$_.chars }).List, ('ccc', 'bb', 'a'), 'negative key in a block';
is-deeply (3, 1, 2).sort(* * -1).List, (3, 2, 1), 'arithmetic WhateverCode key';

# Edge shapes.
is-deeply ().sort(*.chars).List, (), 'empty source';
is-deeply ('only',).sort(*.chars).List, ('only',), 'single element';
is-deeply (1, 1, 1).sort(*.Int).List, (1, 1, 1), 'all keys equal';

# A comparator (arity >= 2) is unaffected by the key-extractor batching.
is-deeply <ccc a bb>.sort({ $^a.chars <=> $^b.chars }).List, ('a', 'bb', 'ccc'),
    'two-arg comparator still works';
is-deeply (3, 1, 2).sort({ $^b <=> $^a }).List, (3, 2, 1), 'reversed comparator still works';
