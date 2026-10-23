use v6;
use Test;

plan 6;

# In a multi-parameter pointy block, `$_` named as one of the parameters is that
# parameter, not the first element map/grep would otherwise topicalize.
is (1, [3]).kv.map(-> $n, $_ { "$n:" ~ .join("+") }).join(' '), '0:1 1:3',
    'map: the second parameter $_ keeps its own value';
is (1, [3]).map(-> $n, $_ { "$n:" ~ .join("+") }).join(' '), '1:3',
    'map over a plain list';
is (1, [3]).kv.map(-> $n, $_ { when Positional { "p$n" }; default { "d$n" } }).join(' '), 'd0 p1',
    'when smartmatches the $_ parameter';
is (1, 2, 3, 4).map(-> $a, $b { "$a-$b" }).join(' '), '1-2 3-4',
    'ordinary two-parameter map is unchanged';
is (1, 2, 3, 4).map({ $_ * 2 }).join(' '), '2 4 6 8', 'implicit topic is unchanged';
my &f = -> $n, $_ { when Positional { "p$n" }; default { "d$n" } };
is f(1, [3]), 'p1', 'a direct call agrees';
