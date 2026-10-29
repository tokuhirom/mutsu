use Test;

# Data::Dump::Tree prints `.WHICH` as the node's perl address: `List|N` for a
# List and `Class+{Role}|N` for a `but`-mixed object.
plan 6;

role R { has $.role }
class G { has $.x }

like (1, 2).WHICH.Str, /^ 'List|' \d+ $/, 'a List is List|N';
like [1, 2].WHICH.Str, /^ 'Array|' \d+ $/, 'an Array is Array|N';
my $g = G.new but R;
like $g.WHICH.Str, /^ 'G+{R}|' \d+ $/, 'a but-mixed object is G+{R}|N';
is $g.WHICH.Str, $g.WHICH.Str, 'stable across reads';
ok $g === $g, 'identity of the mixed object';
isnt $g.WHICH.Str, (G.new but R).WHICH.Str, 'distinct objects differ';
