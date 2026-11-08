use v6.e.PREVIEW;
use Test;

# `self` inside the arguments of a token's method call (`<.helper(self)>`) is
# the grammar cursor at that position.

plan 2;

my @seen;
grammar G {
    token TOP { 'a' <.chk(self)> 'b' }
    method chk($c) { @seen.push($c.^name); self }
}
G.parse('ab');
is @seen.elems, 1, 'the helper ran once';
is @seen[0], 'G', 'self was passed as the grammar cursor';
