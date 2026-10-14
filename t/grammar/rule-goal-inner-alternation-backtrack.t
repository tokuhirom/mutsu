use v6;
use Test;

# Found via CSS::Specification t/00basic.t (`icc-color(<name> [,<number>]{2}?)`):
# in a `rule`, the inner term of `A ~ GOAL TERM` is followed by the goal, so an
# `||` inside it can give its first branch back when the goal then fails.
# A `token` stays ratcheted.

plan 8;

grammar G {
    rule  r1 { '(' ~ ')' [ 'x' || 'x y' ] }
    rule  r2 { '(' ~ ')' [ 'x' || 'x y' ]}
    rule  r3 { '(' [ 'x' || 'x y' ] ')' }
    token t1 { '(' ~ ')' [ 'x' || 'x y' ] }
    rule  r4 { '(' ~ ')' 'x'+ }
    rule  r5 { '(' ~ ')' [ 'x' | 'x y' ] }
    token t2 { '(' ~ ')' [ 'x' | 'x y' ] }
    token t3 { '(' ~ ')' [ 'x y' | 'x' ] }
}

ok  G.parse('(x y)', :rule<r1>), 'rule: ~ inner || backtracks into the second branch';
nok G.parse('(x y)', :rule<r2>), 'no trailing whitespace: the rule stays ratcheted';
ok  G.parse('(x y)', :rule<r3>), 'plain sequence form';
nok G.parse('(x y)', :rule<t1>), 'token: ratcheted';
ok  G.parse('(x)', :rule<r4>), 'goal still matches';
ok  G.parse('(x y)', :rule<r5>), 'LTM alternation';
nok G.parse('(x y)', :rule<t2>), 'token ratchet does not backtrack into a later inner LTM branch';
ok  G.parse('(x y)', :rule<t3>), 'token goal uses source order before ratcheting';
