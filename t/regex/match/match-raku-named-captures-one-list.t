use Test;
plan 3;

# `Match.raku` renders the named captures as ONE list of pairs, like Map.raku.
my $g = "ab" ~~ /$<x>=(a) $<y>=(b)/;
is $g.raku,
    'Match.new(:orig("ab"), :from(0), :pos(2), :hash(Map.new((:x(Match.new(:orig("ab"), :from(0), :pos(1))),:y(Match.new(:orig("ab"), :from(1), :pos(2)))))))',
    "two named captures";

my $one = "ab" ~~ /$<x>=(a) b/;
is $one.raku,
    'Match.new(:orig("ab"), :from(0), :pos(2), :hash(Map.new((:x(Match.new(:orig("ab"), :from(0), :pos(1)))))))',
    "one named capture";

is Map.new((a => 1, b => 2)).raku, 'Map.new((:a(1),:b(2)))', "Map.raku has the same shape";
