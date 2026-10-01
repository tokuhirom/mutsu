use Test;

# ADR-10488: a separated quantifier whose atom files named captures only
# (`<item>+ % ','`) no longer matches each iteration in a capture level of
# its own; the iterations file straight into the enclosing level and the
# names they filed are marked list-valued at the end. Expected values are
# rakudo 2026.07's.

plan 19;

grammar G1 { token TOP { <item>+ % ',' }; token item { \w } }
my $m = G1.parse("a");
is $m<item>.^name, 'Array', 'one iteration still files a list';
is $m<item>.elems, 1, 'one iteration, one entry';
is $m.list.elems, 0, 'no positional captures';
is G1.parse("a,b,c")<item>».Str.join("|"), 'a|b|c', 'entries in match order';

grammar G2 { token TOP { <item>* % ',' }; token item { \w } }
$m = G2.parse("");
is $m<item>.^name, 'Array', 'zero iterations leave an empty list';
is $m<item>.elems, 0, 'zero iterations, zero entries';

grammar G3 { token TOP { [ <k> '=' <v> ]+ % ';' }; token k { \w }; token v { \d } }
$m = G3.parse("a=1;b=2");
is $m<k>».Str.join("|"), 'a|b', 'first name of a group atom';
is $m<v>».Str.join("|"), '1|2', 'second name of a group atom';
$m = G3.parse("a=1");
is $m<k>.^name, 'Array', 'a group atom name is a list after one iteration';
is $m<v>.^name, 'Array', 'so is the other';

grammar G4 { token TOP { [ <d>+ ]+ % '-' }; token d { \d } }
$m = G4.parse("12-3");
is $m<d>.^name, 'Array', 'a quantifier inside the atom';
is $m<d>».Str.join("|"), '1|2|3', 'its entries across iterations';

grammar G5 { token TOP { [ <x=item> ]+ % ',' }; token item { \w } }
$m = G5.parse("p,q");
is $m<x>».Str.join("|"), 'p|q', 'an alias inside the atom';
is $m<item>».Str.join("|"), 'p|q', 'and the rule name it also files under';
ok $m<x>[1] === $m<item>[1], 'both names hold the same Match';

grammar G6 { token TOP { <item>+ % ',' }; token item { \w } }
class A6 { has @.seen; method item($/) { @!seen.push(~$/) }; method TOP($/) { make @!seen.join("") } }
is G6.parse("x,y,z", :actions(A6.new)).made, 'xyz', 'actions run once per iteration, in order';

grammar G8 { token TOP { <.item>+ % ',' }; token item { <c> }; token c { \w } }
class A8 { has @.seen; method item($/) { @!seen.push(~$/) }; method TOP($/) { make @!seen.join("") } }
is G8.parse("m,n", :actions(A8.new)).made, 'mn', 'a silent subrule with an action, per iteration';

# A non-ratchet callee with no registers (`regex r { a* }`) returns leaving a
# choice point behind; a later failure in the caller resumes inside it, after
# the caller pushed choice points of its own (ADR-10488 D4).
grammar G7 { regex TOP { <r> [ x | 'ab' ] }; regex r { a* } }
$m = G7.parse("aab");
ok $m, 'backtracking into a returned callee with no registers';
is ~$m<r>, 'a', 'the callee gave one iteration back';
