use v6;
use Test;

# `@`/`%`-sigil named sub-parameters in a hash-destructuring `for` loop
# (`-> % [:$k, :@v]`) must extract the value of the *key* (sigil stripped),
# bind it as a fresh block-scoped lexical (shadowing any outer variable of the
# same name), and flatten a Positional value's elements into the array like a
# real signature parameter. Regression coverage for zef's
# `my Candidate @dists = gather for @x -> % [:$curi, :@dists] {...}`.

plan 8;

# 1-2. `:@dists` binds the array value of key `dists`, not `[Any]`.
for ({ :curi("C1"), :dists([1, 2, 3]) },) -> % [:$curi, :@dists] {
    is $curi, "C1", ":\$curi scalar destructure binds the key value";
    is-deeply @dists.List, (1, 2, 3), ":\@dists array destructure binds the key's elements";
}

# 3. A fresh block lexical shadows an outer same-named variable instead of
#    clobbering it (the outer typed array must not receive the inner elements).
my Int @dists = 10, 20;
for ({ :dists([1, 2, 3]) },) -> % [:@dists] {
    is @dists.elems, 3, "inner :\@dists shadows outer \@dists";
}
is-deeply @dists.List, (10, 20), "outer \@dists is untouched by the inner destructure";

# 5. A Positional (List) value flattens one level into the `@` param, matching
#    signature-binding (not plain assignment) semantics.
class D { has $.n; method meta { {:name($.n)} } }
for ({ :dists((D.new(:n<X>),).List) },) -> % [:@dists] {
    is @dists.elems, 1, "single-element List value flattens into the array";
    is @dists[0].^name, "D", "the flattened element is the object, not a List wrapper";
}

# 7. Nested arrays flatten only one (shallow) level.
for ({ :m([[1, 2], [3, 4]]) },) -> % [:@m] {
    is @m.elems, 2, "shallow flatten keeps nested arrays as elements";
}

# 8. `%`-sigil named sub-param binds the hash value of its key.
for ({ :h({ a => 1, b => 2 }) },) -> % [:%h] {
    is %h<a>, 1, ":\%h hash destructure binds the key's hash value";
}
