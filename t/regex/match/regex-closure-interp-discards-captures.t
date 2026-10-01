use Test;

plan 4;

# The pattern produced by <{ code }> is matched as its own regex; its
# captures do not leak into the caller's match (matches Rakudo).
my $m = "a12b" ~~ / a <{ '(\d)(\d)' }> b /;
ok $m.so, 'match succeeds';
is $m.list.elems, 0, 'positional captures of the interpolated pattern are discarded';

my $n = "xay" ~~ / x <{ '$<m>=a' }> y /;
ok $n.so, 'match with named capture inside interpolation succeeds';
is $n.hash.elems, 0, 'named captures of the interpolated pattern are discarded';
