use v6;
use Test;

plan 9;

# A regex with code blocks is carried through finalization: the code inside it is
# finalized where it stands (#8033, ADR-15760 slice 2).

my @seen;
grammar G {
    token TOP { <a> { @seen.push('a') } <b> }
    token a { x }
    token b { y { @seen.push('b') } }
    rule r { (\w+) { @seen.push(~$0) } }
}
ok G.parse('xy'), 'a grammar whose tokens hold code blocks parses';
is @seen.join(','), 'a,b', 'the blocks ran in order';

my $t = token { \d+ { @seen.push('t') } };
ok '12' ~~ /<$t>/, 'an anonymous token with a block, interpolated';
is @seen.tail, 't', 'its block ran';

my $n = 0;
'aaa' ~~ / [a { $n++ }]+ /;
is $n, 3, 'a block in a quantified group runs per iteration';
ok 'ab' ~~ /a <?{ $n > 1 }> b/, 'a code assertion sees the lexical';
is ('ab' ~~ / $<x>=[a] { make ~$<x> } b /).made, 'a', 'make inside a block';

# A smart match against m/.../ runs the value's own copy of the code.
my $caught = "oops!";
ok("abc" ~~ m/a(bc){$caught = $0}/, 'a block assigning a capture to an outer lexical');
is $caught, "bc", 'the outer lexical was assigned';
