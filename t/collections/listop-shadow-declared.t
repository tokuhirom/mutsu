use v6;
use Test;

# The declared-in-this-unit half of the listop shadow (see
# t/listop-shadow-imported.t for the imported half): a `sub push` declared at
# unit scope wins over the builtin, and the builtin must not mutate the array
# behind its back. This is the shape P5push's own module body relies on — one of
# its `multi sub pop` candidates calls its *declared* `pop`, not the builtin.

plan 4;

sub push(@a, *@v) { "mine:" ~ @v.join(',') }
sub pop(@a) { @a.elems ?? @a.pop !! 'empty' }

my @x = 1;
is push(@x, 2), 'mine:2', 'a declared `push` wins over the builtin';
is-deeply @x, [1], 'and the builtin did not mutate the array behind its back';

my @y = 7;
is pop(@y), 7, 'a declared `pop` delegating to the method still returns the element';
is pop(@y), 'empty', 'and its own empty-array branch wins over the builtin Failure';
