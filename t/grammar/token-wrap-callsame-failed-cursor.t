use Test;

# Inside a token's `.wrap` wrapper, `callsame` returns the rule's cursor; when
# the rule does not match that is a failed cursor of the grammar (`.pos` -3,
# with `.orig`/`.from` of the attempt), not Nil. `.parse` still answers Nil.

plan 7;

grammar G { token TOP { \d+ } }
my $seen;
G.^find_method('TOP').wrap: sub (|c) { $seen := callsame; $seen };

nok G.parse('abc'), 'a failed parse is still falsy';
is $seen.^name, 'G', 'callsame returned a cursor of the grammar';
is $seen.orig, 'abc', '.orig of the failed attempt';
is $seen.from, 0, '.from of the failed attempt';
is $seen.pos, -3, '.pos is the failure marker';
nok $seen.so, 'the failed cursor is false';

ok G.parse('42'), 'a successful parse still matches';
