use Test;

# `Any`'s iteration methods are rows found by the receiver's MRO
# (ADR-11276 §9.134): an instance, a type object, a user iterator and an
# IterationBuffer-free plain list all reach the same row. Answers checked
# against Rakudo.

plan 18;

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
