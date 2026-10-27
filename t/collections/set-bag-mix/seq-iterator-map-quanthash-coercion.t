use Test;

# `Seq.new($iterator).map(...)` is a finite lazy pipe. The QuantHash and Hash
# coercions must fold the elements it yields (Red's `Bla.^all.map(*.bla).Bag`,
# `ResultAssociative.Bag` took the whole pipe for one element: `(=>1).Bag`).

plan 14;

class Rows does Iterator {
    has @.rows = ("test1", 2), ("test2", 1);
    has $!i = 0;
    method pull-one { $!i < @!rows.elems ?? @!rows[$!i++] !! IterationEnd }
}
sub pairs-seq { Seq.new(Rows.new).map({ .[0] => .[1] }) }
sub keys-seq  { Seq.new(Rows.new).map({ .[0] }) }

is-deeply pairs-seq().Bag, (test1 => 2, test2 => 1).Bag, 'Bag of pairs';
is-deeply pairs-seq().Mix, (test1 => 2, test2 => 1).Mix, 'Mix of pairs';
is-deeply pairs-seq().Set, set(<test1 test2>), 'Set of pairs keeps the keys';
is-deeply keys-seq().Bag, bag(<test1 test2>), 'Bag of keys';
is-deeply keys-seq().Set, set(<test1 test2>), 'Set of keys';
is-deeply keys-seq().BagHash, bag(<test1 test2>).BagHash, 'BagHash';
is-deeply keys-seq().SetHash, set(<test1 test2>).SetHash, 'SetHash';
is-deeply pairs-seq().MixHash, (test1 => 2, test2 => 1).Mix.MixHash, 'MixHash';
is-deeply pairs-seq().hash, {test1 => 2, test2 => 1}, '.hash';
is-deeply pairs-seq().Hash, {test1 => 2, test2 => 1}, '.Hash';
is pairs-seq().Map.elems, 2, '.Map';
is-deeply pairs-seq().Slip.Seq.sort, (test1 => 2, test2 => 1).Seq, '.Slip';

# An unbounded pipe still refuses the coercion.
throws-like { (1..Inf).map(* + 0).Bag }, X::Cannot::Lazy, 'an infinite pipe stays lazy: Bag';
throws-like { (1..Inf).map(* + 0).Set }, X::Cannot::Lazy, 'an infinite pipe stays lazy: Set';

done-testing;
