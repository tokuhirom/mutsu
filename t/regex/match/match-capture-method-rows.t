use Test;

# ADR-11276 §9.95: a `Match` is a `Capture`, and answers `keys`, `values`,
# `kv`, `pairs`, `antipairs`, `hash` and `elems` from `Capture`'s rows. Those
# rows flatten a list or array value in `values` and `kv`, as Rakudo does for a
# plain `Capture` too. The answers below are Rakudo's.

plan 19;

my $m = "ab" ~~ /(\w)* $<n>=[b]?/;
is-deeply $m.keys, (0, 'n').Seq, 'Match.keys';
is $m.values.elems, 3, 'Match.values flattens a quantified capture';
is $m.kv.elems, 5, 'Match.kv flattens it after its key';
is $m.pairs.elems, 2, 'Match.pairs keeps it whole';
my @anti = $m.antipairs;
is @anti.elems, 2, 'Match.antipairs';
is @anti[1].value, 'n', '... a named capture keyed by its name';
isa-ok @anti[0].key, Array, '... a quantified capture is one key';
is $m.hash.keys, ('n',), 'Match.hash';
isa-ok $m.hash, Map, '... is a Map';
is $m.elems, 1, 'Match.elems counts the positional captures';
isa-ok $m.Hash, Hash, 'Match.Hash is a Hash';
isa-ok $m.serial, Match, 'Match.serial is the match (Any.serial)';

grammar G { token TOP { <a> <b> }; token a { \w }; token b { \d } }
my $p = G.parse("x1");
is $p.hash.keys.sort, <a b>, 'a grammar match answers hash';
is $p.elems, 0, '... and elems';
is $p.values.map(~*).sort, <1 x>, '... and values';

my $c = \([1, 2], 3, :a([4, 5]));
is-deeply $c.values, (1, 2, 3, 4, 5).Seq, 'Capture.values flattens an array';
is-deeply $c.kv, (0, 1, 2, 1, 3, 'a', 4, 5).Seq, 'Capture.kv too';
is-deeply \($[1, 2], 3).values, ($[1, 2], 3).Seq, 'an itemized array stays whole';
is-deeply \([1, 2]).pairs, (0 => [1, 2],).Seq, 'Capture.pairs keeps it whole';
