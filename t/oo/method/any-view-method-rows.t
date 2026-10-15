use Test;

# ADR-11276 §9.109: `Any`'s key/value views, `pairup`, `invert`,
# `permutations`, `combinations`, `cache`, `hash` and `sum` are rows, and the
# receivers the deleted `methods_0arg/collection.rs` arms answered reach them:
# a closed shape with no row of its own (`Range`, `Pair`, `Uni`, `Blob`,
# `IO::Path`), a `Seq` with no settled shape, a shaped array, `Nil`, a type
# object and a stash.

plan 43;

# A `Range` lists as its elements.
is (1..3).keys.raku, (0, 1, 2).Seq.raku, 'Range.keys';
is (1..3).pairs.raku, (0 => 1, 1 => 2, 2 => 3).Seq.raku, 'Range.pairs';
is (1..3).permutations.elems, 6, 'Range.permutations';
is (1..3).combinations.elems, 8, 'Range.combinations';

# Other closed shapes.
is "ab".NFC.keys.raku, (0, 1).Seq.raku, 'Uni.keys';
is Blob.new(5, 6).kv.raku, (0, 5, 1, 6).Seq.raku, 'Blob.kv';
is "x".IO.pairs.elems, 1, 'IO::Path.pairs is one pair';
is (:a(1)).hash.raku, {a => 1}.raku, 'Pair.hash';
throws-like { "x".IO.hash }, X::Hash::Store::OddNumber, 'IO::Path.hash is an odd initializer';
is (1..2).hash.raku, {1 => 2}.raku, 'Range.hash';

# `Nil` has no elements.
is Nil.keys.elems, 0, 'Nil.keys';
is Nil.invert.elems, 0, 'Nil.invert';

# A shaped array reads its leaves.
my @s[2;2] = (1, 2), (3, 4);
is @s.keys.raku, ((0, 0), (0, 1), (1, 0), (1, 1)).Seq.raku, 'shaped keys';
is @s.kv.elems, 8, 'shaped kv';
is @s.antipairs.map(*.key).List, (1, 2, 3, 4), 'shaped antipairs';

# A `Seq` with no settled shape.
is (gather { take 1; take 2 }).kv.raku, (0, 1, 1, 2).Seq.raku, 'Seq.kv';
is (gather { take 3; take 4 }).antipairs.raku, (3 => 0, 4 => 1).Seq.raku, 'Seq.antipairs';
is (gather { take 3; take 4 }).values.List, (3, 4), 'Seq.values';
is (gather { take (a => 1) }).invert.raku, (1 => "a",).Seq.raku, 'Seq.invert';
is (gather { take 1; take 2 }).sum, 3, 'Seq.sum';
is (gather { take 1; take 2 }).cache.List, (1, 2), 'Seq.cache';

# `pairup`.
is (1, 2, 3, 4).pairup.raku, (1 => 2, 3 => 4).Seq.raku, 'List.pairup';
is [1, (a => 2), 3, 4].pairup.raku, (1 => (a => 2), 3 => 4).Seq.raku, 'Array.pairup';
is Range.pairup.elems, 0, 'a type object pairs up to nothing';
throws-like { (1, 2, 3).pairup.eager }, X::Pairup::OddNumber, 'an odd element';

# `invert` checks its elements.
throws-like { (1, 2).invert.eager }, X::TypeCheck, 'List.invert of a non-Pair';
throws-like { "a".invert.eager }, X::TypeCheck, 'Str.invert names the Str';
throws-like { (1..2).invert.eager }, X::TypeCheck, 'Range.invert';
is (a => 1, b => 2).invert.sort.raku, (1 => "a", 2 => "b").Seq.raku, 'List.invert';

# `cache`.
isa-ok [1, 2].cache, Array, 'Array.cache is the Array';
isa-ok {a => 1}.cache, List, 'Hash.cache is a List';
is {a => 1}.permutations.elems, 1, 'Hash.permutations';

# A type object's `hash`.
is Any.hash.raku, {}.raku, 'Any.hash';
is Int.hash.raku, {}.raku, 'Int.hash';
is Hash.hash.raku, 'Hash', 'Hash.hash is Hash';
throws-like { Mu.hash }, X::Method::NotFound, 'Mu is no Any';

# A type object lists as itself.
is Array.cache.raku, (Array,).raku, 'Array.cache';
is Int.permutations.elems, 1, 'Int.permutations';
is Int.combinations.elems, 2, 'Int.combinations';
is Int.keys.elems, 0, 'Int.keys';

# `kxxv` is `Baggy`'s alone.
throws-like { set(<a>).kxxv }, X::Method::NotFound, 'Set.kxxv';
throws-like { {a => 1}.kxxv }, X::Method::NotFound, 'Hash.kxxv';

# A stash reads as its symbols.
our $stash-probe = 7;
ok OUR::.kv.grep('$stash-probe').elems, 'a stash kv lists its symbols';
