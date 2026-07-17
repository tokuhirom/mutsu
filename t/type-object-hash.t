use Test;

plan 11;

# A type object has no contents, so `.hash` (inherited from Any) is the empty
# hash. mutsu used to route it through the list path, which treated the type
# object as a one-element hash initializer and threw
# "Odd number of elements found where hash initializer expected".
# zef hit this in `Zef::Client`: `$dist.meta<files>.hash.keys` on a dist whose
# META has no `files` key.

is-deeply Any.hash, {}, 'Any.hash is the empty hash';
is-deeply Int.hash, {}, 'Int.hash is the empty hash';
is-deeply Str.hash, {}, 'Str.hash is the empty hash';
is-deeply Array.hash, {}, 'Array.hash is the empty hash';
is-deeply Set.hash, {}, 'Set.hash is the empty hash';

is Any.hash.^name, 'Hash', 'the result is a real Hash';
is Any.hash.elems, 0, 'the result is empty';
is-deeply Any.hash.keys.List, ().List, 'keys of a type-object hash is empty';

# An Associative's `.hash` is itself, and the Hash type object is no exception.
is Hash.hash, Hash, 'Hash.hash is the Hash type object, not an empty hash';

class Foo {}
is-deeply Foo.hash, {}, 'a user class type object hashes to the empty hash';

# Mu is the root type and does not inherit Any's `.hash` at all.
throws-like { Mu.hash }, X::Method::NotFound, 'Mu.hash has no such method';
