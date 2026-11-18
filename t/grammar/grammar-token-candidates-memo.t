use Test;

# A rule's candidates are resolved once per declaration state and remembered.
# Two grammars naming the same rules, a grammar declared later, and a
# subclass overriding one rule must still each see their own candidates.

plan 7;

grammar G1 { token TOP { <a> <b> }; token a { x }; token b { y } }
grammar G2 { token TOP { <a> <b> }; token a { z }; token b { y } }

is (^3).map({ so G1.parse('xy') }).join(','), 'True,True,True', 'G1 parses repeatedly';
is (^3).map({ so G2.parse('zy') }).join(','), 'True,True,True', 'G2 with the same rule names';
nok G1.parse('zy'), 'G1 keeps its own a after G2 ran';
nok G2.parse('xy'), 'G2 keeps its own a after G1 ran';

# A subclass declared after the memo filled overrides one rule.
my $sub = EVAL 'grammar G3 is G1 { token a { w } }; G3';
ok $sub.parse('wy'), 'a later subclass sees its own a';
nok $sub.parse('xy'), 'and not its parent\'s';

# A proto with candidates added after a parse.
grammar P { token TOP { <item>+ }; proto token item {*}; token item:sym<x> { x } }
P.parse('xx');
my $more = EVAL 'grammar P2 is P { token item:sym<y> { y } }; P2';
ok $more.parse('xyx'), 'a proto variant declared later joins the candidates';
