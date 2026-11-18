use Test;

# The start rule's rendered pattern is remembered when it is a function of
# the rule's literal alone (no parameters, no arguments). Rules that share a
# literal, proto variants, parameterized start rules and subrule argument
# lists must still render and match as before.

plan 8;

grammar A { token TOP { <x> }; token x { a } }
grammar B { token TOP { <x> }; token x { b } }
is (^3).map({ so A.parse('a') }).join(','), 'True,True,True', 'A parses repeatedly';
ok B.parse('b'), 'B shares the literal of A\'s TOP, but its own x';
nok B.parse('a'), 'and does not match A\'s x';

grammar P {
    proto token TOP {*}
    token TOP:sym<p> { <sym> 'q' }
}
ok P.parse('pq'), 'a proto start rule with a <sym> variant';
ok P.parse('pq'), 'again';

grammar Arg {
    token TOP($c) { $c+ }
}
ok Arg.parse('xxx', :args(('x',))), 'a parameterized start rule with one argument';
nok Arg.parse('xxx', :args(('y',))), 'and with another';

grammar Sub {
    token TOP { <rep('z', 2)> }
    token rep($c, $n) { $c ** {$n} }
}
ok (^2).map({ so Sub.parse('zz') }).all, 'a subrule argument list in the start rule';
