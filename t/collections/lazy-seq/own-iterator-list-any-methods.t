use Test;

# ADR-11276 §9.148: `Any`'s list methods read the receiver's own `iterator`
# (`head`, `map`, `sort`, `sum`, ...) or its own `list` (those, and `join`,
# `elems`, `kv`, `flat`, ...), as Rakudo's do: `Any.iterator` is
# `self.list.iterator`. Neither class composes `Iterable`.

plan 34;

my @src = 3, 1, 2, 2;
class WithIter { method iterator { @src.iterator } }
class WithList { method list { @src.List } }

my $i = WithIter.new;
is $i.head, 3, 'iterator: head';
is $i.tail, 2, 'iterator: tail';
is $i.first(* < 3), 1, 'iterator: first';
is $i.skip.List, (1, 2, 2), 'iterator: skip';
is $i.grep(* > 1).List, (3, 2, 2), 'iterator: grep';
is $i.map(* + 1).List, (4, 2, 3, 3), 'iterator: map';
is $i.sort.List, (1, 2, 2, 3), 'iterator: sort';
is $i.min, 1, 'iterator: min';
is $i.max, 3, 'iterator: max';
is $i.minmax, 1..3, 'iterator: minmax';
is $i.sum, 8, 'iterator: sum';
is $i.unique.List, (3, 1, 2), 'iterator: unique';
is $i.squish.List, (3, 1, 2), 'iterator: squish';
is $i.batch(2).List, ((3, 1), (2, 2)), 'iterator: batch';
is $i.Seq.List, (3, 1, 2, 2), 'iterator: Seq';
is $i.repeated.List, (2,), 'iterator: repeated';
# `self.list` is the object itself for a class with only an `iterator`.
is $i.elems, 1, 'iterator: elems is of the one-item list';
is $i.join(','), $i.Str, 'iterator: join is of the one-item list';

my $l = WithList.new;
is $l.head, 3, 'list: head';
is $l.sort.List, (1, 2, 2, 3), 'list: sort';
is $l.sum, 8, 'list: sum';
is $l.join(','), '3,1,2,2', 'list: join';
is $l.elems, 4, 'list: elems';
is $l.reverse.List, (2, 2, 1, 3), 'list: reverse';
is $l.kv.List, (0, 3, 1, 1, 2, 2, 3, 2), 'list: kv';
is $l.keys.List, (0, 1, 2, 3), 'list: keys';
is $l.flat.List, (3, 1, 2, 2), 'list: flat';
is $l.end, 3, 'list: end';
is $l.combinations(2).elems, 6, 'list: combinations';
is $l.classify(* %% 2){True}.List, (2, 2), 'list: classify';
is $l.antipairs.head.key, 3, 'list: antipairs';
is $l.cache.List, (3, 1, 2, 2), 'list: cache';

class OwnHead { method iterator { @src.iterator }; method head { 'own' } }
is OwnHead.new.head, 'own', 'a class\'s own method wins';
is OwnHead.new.tail, 2, 'the others still read its iterator';
