# A chained subscript store into an object hash keeps the object key

`@v[0]{$obj} = 1`, `%h<k>{$obj} = 1` and `.[0].{$obj} = 1` stored the entry under
the key's string form (`AST<20>`) when the hash they reached was an object hash;
the single-level `$h{$obj} = 1` already keyed it by the object. The chained store
carries its subscript keys as strings, so the hash write never saw the key object.
The last write now goes through one helper (`insert_hash_element_keyed`) that stores
under the key's `.WHICH` and records the object in `original_keys` when the hash
has a key type.

Red's `found-bool` does `for @values { .[0].{$ex.ast} = $ex.value }` on the
`Red::AST`-keyed branch table `what-does-it-do` builds; a `Str` key there made
`hash-to-cond` die once `CX::Red::Bool` became resumable (Red `t/66-join-model` now
passes 3/3).
