use Test;

plan 11;

# #13195: List methods over a list whose elements are Matches.
my $m = "9 10" ~~ /(\d+) \s (\d+)/;
my @l := $m.list.List;

is @l.minmax.raku, '10..9', 'minmax compares the Matches, then numifies the endpoints';
is $m.minmax.raku, '10..9', 'Match.minmax';
isa-ok @l.minmax.max, Int, 'minmax endpoint is a Real';

my $bad = "12 ab" ~~ /(\d+) \s (\w+)/;
isa-ok $bad.list.minmax.max, Failure, 'a non-numeric Match endpoint is a Failure';

my $c = @l.chrs;
isa-ok $c, Failure, 'chrs refuses Match elements';
is $c.exception.^name, 'X::TypeCheck', 'chrs failure is X::TypeCheck';
is (65, 66).chrs, 'AB', 'chrs still converts Ints';

is (1, 2, 1).minpairs.^name, 'List', 'minpairs answers a List';
is (1, 2, 2).maxpairs.^name, 'List', 'maxpairs answers a List';
is $m.minpairs.elems, 1, 'Match.minpairs reads the positional captures';

throws-like { (1, 2).rotor }, X::Multi::NoMatch, 'rotor without a cycle has no candidate';
