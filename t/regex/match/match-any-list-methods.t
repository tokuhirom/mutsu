use Test;

# ADR-11276 §9.98, #13161: `Any` declares its list methods as
# `self.list.METHOD`, and a `Match` is a `Capture`, whose `.list` is its
# positional captures. So a `Match` answers them on those captures, never on
# itself as one element; a grammar match with only named captures lists as
# `()`. `Hash` reads the named captures. The answers below are Rakudo's.

plan 37;

my $m = "12 ab" ~~ /(\d+)\s(\w+)/;
grammar G { token TOP { <a> " " <b> }; token a { \d+ }; token b { \w+ } }
my $p = G.parse("12 ab");

is ~$m.min, '12', 'Match.min is the least positional capture';
is ~$m.max, 'ab', 'Match.max';
is $p.min, Inf, 'a grammar match with no positional capture: min is Inf';
is $p.max, -Inf, '... max is -Inf';
is ~$m.minmax.min, '12', 'Match.minmax starts at the least capture';
is-deeply $p.unique.List, (), 'Match.unique of a grammar match is empty';
is $m.unique.map(~*).join(','), '12,ab', 'Match.unique reads the captures';
is-deeply $m.repeated.List, (), 'Match.repeated';
is $m.squish.elems, 2, 'Match.squish';
is $p.combinations.elems, 1, 'Match.combinations of no captures is (())';
is $m.combinations.elems, 4, 'Match.combinations of two captures';
is $m.permutations.elems, 2, 'Match.permutations';
is $p.chrs, '', 'Match.chrs reads the captures (Cool.chrs)';
is $m.Set.elems, 2, 'Match.Set';
is $m.Bag.total, 2, 'Match.Bag';
is $m.Mix.elems, 2, 'Match.Mix';
is-deeply $p.Set, set(), 'Match.Set of a grammar match';
is $m.sort.map(~*).join(','), '12,ab', 'Match.sort';
is $m.reverse.map(~*).join(','), 'ab,12', 'Match.reverse';
is ~$m.head, '12', 'Match.head';
is ~$m.tail, 'ab', 'Match.tail';
is $m.skip(1).map(~*).join(','), 'ab', 'Match.skip';
is ~$m.first(*.so), '12', 'Match.first';
is $m.grep(/\d/).elems, 1, 'Match.grep';
is $m.map(~*).join(','), '12,ab', 'Match.map';
is $m.flat.elems, 2, 'Match.flat';
is $m.Seq.elems, 2, 'Match.Seq';
is $m.cache.elems, 2, 'Match.cache';
is $m.eager.elems, 2, 'Match.eager';
is $m.Slip.elems, 2, 'Match.Slip';
isa-ok $m.Array, Array, 'Match.Array is an Array';
is $p.Hash.keys.sort.join(','), 'a,b', 'Match.Hash reads the named captures';
is-deeply $m.Hash, {}, '... which a plain match has none of';

# An unbound capture is a hole the list keeps, which `.List` must not read
# as `Nil`.
my $o = "ab" ~~ /(\w)(\d)?(\w)/;
is $o.List.raku, $o.list.raku, 'Match.List is Match.list, holes included';
is $o.List.elems, 3, '... and all three slots';

# A grammar that declares the method keeps its own.
grammar H { token TOP { <a> }; token a { \d+ }; method min { "own" } }
is H.parse("12").min, 'own', "a grammar's own method comes first";
is H.parse("12").max, -Inf, '... and the others still read the captures';
