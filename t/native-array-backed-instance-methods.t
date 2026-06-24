use Test;

# An `is Array` subclass instance delegates non-mutating list methods to its
# backing `__mutsu_array_storage`. Those that return a fresh value
# (sort/reverse/map/first/unique/join/head/tail/elems + minmax via their native
# helpers) now dispatch through the same VM-native path a *plain* array uses,
# instead of bouncing to the tree-walk interpreter (ledger §D / §C Phase-3).
# This pin asserts the results are byte-identical to Rakudo (i.e. the native
# dispatch matches the previous interpreter fallback), and that mutating methods
# (push/pop) still work.

plan 13;

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
