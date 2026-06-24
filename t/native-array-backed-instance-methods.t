use Test;

# An `is Array` subclass instance delegates non-mutating list methods to its
# backing `__mutsu_array_storage`. Those that return a fresh value
# (sort/reverse/map/first/unique/join/head/tail/elems + minmax via their native
# helpers) now dispatch through the same VM-native path a *plain* array uses,
# instead of bouncing to the tree-walk interpreter (ledger §D / §C Phase-3).
# This pin asserts the results are byte-identical to Rakudo (i.e. the native
# dispatch matches the previous interpreter fallback), and that mutating methods
# (push/pop) still work.

plan 28;

class S is Array {}
my $s = S.new;
$s.push(3, 1, 2, 4);

is $s.sort.join(","), "1,2,3,4", 'sort on is-Array storage';
is $s.reverse.join(","), "4,2,1,3", 'reverse on is-Array storage';
is $s.map({ $_ * 10 }).join(","), "30,10,20,40", 'map on is-Array storage';
is $s.first({ $_ > 2 }), 3, 'first on is-Array storage';
is $s.grep({ $_ > 2 }).join(","), "3,4", 'grep on is-Array storage';
is $s.elems, 4, 'elems on is-Array storage';
is $s.unique.join(","), "3,1,2,4", 'unique on is-Array storage';
is $s.join("-"), "3-1-2-4", 'join on is-Array storage';
is $s.head(2).join(","), "3,1", 'head on is-Array storage';
is $s.tail(2).join(","), "2,4", 'tail on is-Array storage';

# mutation still works through the native fast path / fallback
$s.push(9);
is $s.elems, 5, 'push mutates the instance';
$s.pop;
is $s.elems, 4, 'pop mutates the instance';

# the sort result must not have mutated the source order
is $s.join(","), "3,1,2,4", 'non-mutating methods leave the instance untouched';

# list-operation methods (previously missing from the is-Array delegation list,
# so they wrongly treated the instance as a single element) now operate on the
# backing storage, matching Rakudo.
is $s.min, 1, 'min over is-Array elements';
is $s.max, 4, 'max over is-Array elements';
is $s.minmax.gist, "1..4", 'minmax over is-Array elements';
is $s.sum, 10, 'sum over is-Array elements';
is $s.reduce(* + *), 10, 'reduce over is-Array elements';
is $s.keys.join(","), "0,1,2,3", 'keys (indices) of is-Array';
is $s.values.join(","), "3,1,2,4", 'values of is-Array';
is $s.kv.join(","), "0,3,1,1,2,2,3,4", 'kv of is-Array';
is $s.pairs.map(*.value).join(","), "3,1,2,4", 'pairs of is-Array';
is $s.antipairs.map(*.key).join(","), "3,1,2,4", 'antipairs of is-Array';
is $s.classify({ $_ %% 2 })<True>.join(","), "2,4", 'classify over is-Array';
is $s.rotor(2).map(*.join("")).join(","), "31,24", 'rotor over is-Array';
is $s.combinations(4).elems, 1, 'combinations over is-Array';
is $s.permutations.elems, 24, 'permutations over is-Array';
is $s.all.so, True, 'all junction over is-Array';
