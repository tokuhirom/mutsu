use Test;

# From the Sway::Config ecosystem distribution: `$/.caps.hyper.map(*.value.made)`
# inside a grammar action. A Str-keyed Pair element handed to the hyper/race
# worker was bound as a named argument, so the WhateverCode died with
# "Too few positionals passed".

plan 4;

my @pairs = (a => 1, b => 2);
is-deeply @pairs.hyper.map(*.key).List, ('a', 'b'), 'hyper.map over Str-keyed pairs';
is-deeply @pairs.race.map(*.value).List, (1, 2), 'race.map over Str-keyed pairs';
is-deeply @pairs.hyper.map(-> $p { $p.key ~ $p.value }).List, ('a1', 'b2'), 'pointy block gets the pair';

grammar G {
    token TOP { <w>+ % ' ' }
    token w { \w+ }
}
class A {
    method TOP($/) { make $/.caps.hyper.map(*.key).join(',') }
}
is G.parse('x y', :actions(A.new)).made, 'w,w', 'caps.hyper.map inside an action';
