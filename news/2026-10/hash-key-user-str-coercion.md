# A user object as a Str hash key goes through its .Str

A plain `Str`-keyed hash now coerces a user object key with the key's own
`.Str`, as rakudo does: `%h{K.new}`, `%h.AT-KEY(K.new)`, `EXISTS-KEY`,
`ASSIGN-KEY`, `DELETE-KEY`, `:exists`, `:delete` and the subscript store all
agree on the key. Before, the key was read with `to_string_value()`, which never
runs user code, so a class with `method Str { "a" }` was keyed as `K<23>` and
every lookup missed (#13293). Object hashes (`%h{Any}`) still key by identity.
The helper is `Interpreter::hash_key_through_user_str`
(`src/vm/vm_hash_key_user_str.rs`).
