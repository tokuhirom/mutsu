use v6;
use Test;

plan 10;

# An itemized array/list value passed as map/grep's list argument is a single
# item — it must NOT be flattened into its elements. This must hold whether the
# value comes from a literal, a variable, or a sub-call result (the sub-call
# case is the P27 `group` recursion in roast/integration/99problems-21-to-30.t).

# literal $[...] / $(...)
is (map -> $g { "x" }, $[1, 2]).elems, 1, 'map over literal $[1,2] iterates once';
is (map -> $g { "x" }, $(1, 2, 3)).elems, 1, 'map over literal $(1,2,3) iterates once';

# variable
my $x = $[];
is (map -> $g { "got:" ~ $g.raku }, $x).elems, 1, 'map over $x holding $[] iterates once';

# sub-call result
sub itemized-empty() { return $[] }
sub itemized-pair() { return $[1, 2] }
my @r = map -> $g { "got:" ~ $g.raku }, itemized-empty();
is @r.elems, 1, 'map over sub-call returning $[] iterates once';
is @r[0], 'got:$[]', 'map topic is the whole itemized array';
my @r2 = map -> $g { $g.raku }, itemized-pair();
is @r2.raku, '["\$[1, 2]"]', 'map over sub-call returning $[1,2] gets one item';

# grep: same non-flattening rule
is (grep { True }, $(1, 2, 3)).elems, 1, 'grep over $(1,2,3) sees one item';
is (grep { True }, $[1, 2, 3]).elems, 1, 'grep over $[1,2,3] sees one item';
is (grep { True }, itemized-pair()).elems, 1, 'grep over sub-call returning $[1,2] sees one item';

# non-itemized values still flatten
is (map -> $g { $g }, (1, 2, 3)).elems, 3, 'map over bare (1,2,3) still flattens';
