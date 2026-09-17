use Test;

# `Hash.new($assoc)` / `Map.new($assoc)` must copy the key/value contents of a
# blessed Associative object (a class that implements the AT-KEY/keys protocol,
# e.g. a Hash::Agnostic tied hash), not treat the object as a single scalar key.
# Regression: `Hash.new($obj)` produced `{"Obj()" => Any}` because the flattening
# used `value_to_list`, which cannot enumerate a blessed Associative instance.

plan 7;

class MyHash does Associative {
    has %.store handles <AT-KEY EXISTS-KEY DELETE-KEY keys values pairs>;
}

my $obj = MyHash.new(store => {a => 1, b => 2, c => 3});

my %h = Hash.new($obj);
is %h.elems, 3, 'Hash.new(Associative) copies all pairs';
is-deeply %h.sort.List, (:a(1), :b(2), :c(3)).sort.List, 'Hash.new(Associative) copies key/value contents';

my %m = Map.new($obj);
is %m.elems, 3, 'Map.new(Associative) copies all pairs';
is %m<b>, 2, 'Map.new(Associative) preserves values';

# A plain Hash argument is unaffected (already flattened to its pairs).
my %plain = (x => 10, y => 20);
is-deeply Hash.new(%plain).sort.List, (:x(10), :y(20)).sort.List, 'Hash.new(%plain-hash) still works';

# Hash::Agnostic's `.pairs` implementation returns the result of `keys.map`,
# which is a deferred Seq. Its `.Hash` method delegates to Hash.new, which must
# materialize that Associative result.
class LazyPairsHash does Associative does Iterable {
    has %.store;

    method AT-KEY($key) is raw { %!store.AT-KEY($key) }
    method keys()              { %!store.keys         }
    method pairs()             { self.keys.map({ $_ => self.AT-KEY($_) }) }
    method iterator()          { self.pairs.iterator }
    method Hash()              { Hash.new(self)        }
}

my $lazy = LazyPairsHash.new(store => {a => 1, b => 2});
is-deeply $lazy.Hash.sort.List, (:a(1), :b(2)).sort.List,
    'Hash.new materializes a lazy Associative .pairs result';

# Named args still become data (rakudo issue #3211 behavior kept).
is Hash.new(:42a, :666b).elems, 2, 'Hash.new(:named) keeps named args as data';
