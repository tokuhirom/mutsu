use Test;

plan 11;

# `List.ACCEPTS(List:D: $topic)` for a non-Iterable topic answers its own
# truthiness when its first element is a Match, and False otherwise (#13251).
grammar G {
    token TOP { 'a' <b>* <c>? }
    token b { 'b' }
    token c { 'c' }
}
my $mm = G.parse('abb');
ok $mm ~~ $mm<b>, 'a Match accepts its own quantified capture list';
ok $mm ~~ [$mm<b>[0]], 'list of one Match';
nok $mm ~~ [1, 2], 'list of non-Matches';
nok $mm ~~ [], 'empty list';
nok 5 ~~ [5,], 'a non-Match first element is False';
ok $mm ~~ ($mm, 5), 'first element decides';
nok $mm ~~ (5, $mm), 'first element decides (not any)';
nok $mm ~~ $mm<c>, 'an absent optional capture';
ok 5 ~~ (my @g = 'abab' ~~ m:g/a/), 'm:g result accepts any topic';

class A {
    multi method TOP($/ where $<b>) { make "has b" }
    multi method TOP($/)            { make "plain" }
}
is G.parse('abb', :actions(A.new)).made, 'has b', 'where $<b> picks the candidate';
is G.parse('a', :actions(A.new)).made, 'plain', 'no capture falls to the plain candidate';
