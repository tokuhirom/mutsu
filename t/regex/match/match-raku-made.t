use Test;

# `Match.raku` lists `.made` last, as Rakudo does.

plan 4;

my $m = 'ab' ~~ /a { make 5 } b/;
is $m.raku, 'Match.new(:orig("ab"), :from(0), :pos(2), :made(5))', 'inline make';

my $n = 'ab' ~~ /(a)/;
$n.make([1, 2]);
is $n.raku,
    'Match.new(:orig("ab"), :from(0), :pos(1), :list((Match.new(:orig("ab"), :from(0), :pos(1)),)), :made([1, 2]))',
    'made after the positional captures';

grammar G { token TOP { <a> }; token a { 'a' } }
class A { method TOP($/) { make 42 } }
is G.parse('a', :actions(A)).raku,
    'Match.new(:orig("a"), :from(0), :pos(1), :hash(Map.new((:a(Match.new(:orig("a"), :from(0), :pos(1)))))), :made(42))',
    'made after the named captures of a parse';

is ('x' ~~ /x/).raku, 'Match.new(:orig("x"), :from(0), :pos(1))', 'no :made without a make';
