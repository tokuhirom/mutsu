use Test;
plan 5;

my $r = do { my ($a, $b) andthen "y" };
is $r, "y", 'group declaration followed by andthen';

my @seen;
my ($c, $d) andthen @seen.push("ran");
is @seen.elems, 1, 'andthen tail runs for a bare group declaration';

my ($e, $f) orelse @seen.push("orelse");
is @seen.elems, 1, 'orelse tail is short-circuited (declared list is defined)';

my @and;
my ($g, $h) and @and.push("and");
is @and.elems, 1, 'and tail runs: the declared list is true';
$g = 5;
is $g, 5, 'group variables stay usable in the enclosing scope';
