use Test;

# `Any`'s iteration methods are rows found by the receiver's MRO
# (ADR-11276 §9.134): an instance, a type object, a user iterator and an
# IterationBuffer-free plain list all reach the same row. Answers checked
# against Rakudo.

plan 50;

class Plain {}
class PI { method iterator { (4, 5).iterator } }
class It does Iterable { method iterator { (1, 2, 3).iterator } }

is-deeply Plain.new.map({ 1 }).List, (1,), 'an instance maps as one item';
is-deeply PI.new.map(* + 1).List, (5, 6), 'a user iterator feeds map';
is-deeply PI.new.grep(* > 4).List, (5,), 'a user iterator feeds grep';
is PI.new.first, 4, 'a user iterator feeds first';
is-deeply It.new.map(* * 2).List, (2, 4, 6), 'an Iterable instance maps its elements';
is-deeply Int.map(*.^name).List, ('Int',), 'a type object maps as one item';
is Int.first, Int, 'first on a type object';
is-deeply Int.eager.List, (Int,), 'eager on a type object';
is Plain.new.squish.elems, 1, 'squish on an instance';
is-deeply Plain.new.classify({ 'k' }).keys.List, ('k',), 'classify on an instance';
is-deeply (1, 2, 3).rotor(2, :partial).List, ((1, 2), (3,)), 'rotor with :partial';
throws-like { (1, 2, 3).reduce() }, X::Multi::NoMatch, 'reduce without a callable';
throws-like { (1, 2, 3).squish(1) }, X::Multi::NoMatch, 'squish with a positional';
is (1, 2, 3).skip(1).List, (2, 3), 'skip';
is-deeply (1, 2).flatmap({ ($_, $_) }).List, (1, 1, 2, 2), 'flatmap';
is-deeply [1, [2, 3]].deepmap(* * 2), [2, [4, 6]], 'deepmap keeps the structure';
is-deeply (1, (2, 3)).duckmap(-> Int $x { $x + 1 }).List, (2, (3, 4)), 'duckmap';
throws-like { (1, 2).flatmap(* + 1, * + 2) }, X::Multi::NoMatch, 'flatmap takes one mapper';

# A pure row's interpreter twin answers what the pure row declines (§9.135).
class PIs { method iterator { (3, 1, 2).iterator } }
is-deeply PIs.new.sort.List, (1, 2, 3), 'sort through a user iterator';
is-deeply Int.sort.List, (Int,), 'sort on a type object';
is (3, 1, 2).max(-*), 1, 'max with a mapper';
is-deeply (3, 1, 2).sort(-*).List, (3, 2, 1), 'sort with a mapper';
is-deeply (1, 2, 3, 4).unique(:as(* % 2)).List, (1, 2), 'unique with :as';
is (3, 1, 2).minmax(-*), 3..1, 'minmax with a mapper';
is Plain.new.max.^name, 'Plain', 'max on an instance';
throws-like { (1, 2).minpairs(1) }, X::Multi::NoMatch, 'minpairs takes no argument';

# The junction constructors are rows of `Any`.
is (1, 2, 3).any.raku, 'any(1, 2, 3)', 'any';
is Int.one.raku, 'one(Int)', 'one on a type object';
my $held = (1, 2);
is $held.all.raku, 'all(1, 2)', 'a scalar-held list is the list';
is Plain.new.none.^name, 'Junction', 'none on an instance';

# The coercions, `slice` and `take` are late rows of `Any`/`Mu`.
is Plain.new.Set.elems, 1, 'Set on an instance';
is Int.Bag.raku, '(Int=>1).Bag', 'Bag on a type object';
is (1, 1, 2).BagHash.^name, 'BagHash', 'BagHash';
is Bag.new-from-pairs("a" => 2, "b" => 1)<a>, 2, 'new-from-pairs counts the values';
is-deeply (gather { 42.take }).List, (42,), 'take';
is-deeply (1, 2, 3).Seq.slice(0, 2).List, (1, 3), 'slice';
throws-like { Int.slice(0, 2) }, X::Multi::NoMatch, 'slice wants a defined invocant';

# `slice` flattens its indices, which must ascend; `toggle`, `tree(...)`,
# `rotate` and `join` are late rows too.
is-deeply (1, 2, 3, 4).slice((0, 2)).List, (1, 3), 'slice flattens a list of indices';
is-deeply (1, 2, 3).slice(0, 5).List, (1,), 'slice ends at the first index past the end';
throws-like { (1, 2, 3).slice(2, 0) }, Exception, message => 'Provided index 0, which is lower than 3', 'slice indices must ascend';
is-deeply (1, 2, 3, 2, 1).toggle(* < 3).List, (1, 2), 'toggle';
is-deeply (1, 2).Seq.rotate.List, (2, 1), 'rotate on a Seq';
throws-like { (1, 2).join('a', 'b') }, Exception, message => /'Too many positionals'/, 'join takes one separator';
is Supply.from-list(1, 2).tree.^name, 'Supply', 'tree on a Supply is the Supply';

# A Supply's list coercions, `elems` and the quant hashes' family coercions.
my $sup = Supply.from-list(1, 2);
is-deeply $sup.list, (1, 2), 'Supply.list';
is $sup.Array.raku, '[1, 2]', 'Supply.Array';
is Plain.new.elems, 1, 'elems on an instance';
is SetHash.Baggy.^name, 'BagHash', 'SetHash.Baggy keeps the hashiness';
is Set.Mixy.^name, 'Mix', 'Set.Mixy';
is bag(1).Setty.raku, 'Set.new(1)', 'Bag.Setty';
