# `:delete` resolves its container through the variable's store place

The `:delete` handler read and wrote its container by name through env, so a
scalar-held container (`my $h = {...}; $h<a>:delete`) had to be seeded into env from
its local slot first, and the closing sync wrote env back over the slot. The op now
carries a `StorePlace` (the same type the element-assign handler uses): a variable
the running code declares is read and mutated in its own slot, with the env mirror
kept pointing at it, and only a `Named` target (a compunit's file-scope or an `our`
variable, an aliased or cross-thread name) is resolved by name and seeded. The
inner handler, the nested-slice delete, the hash fast path, and the hole bookkeeping
(`unmark_initialized_indices`, `trim_trailing_array_holes`) all take the place.
`t/collections/element-delete-place-shapes.t` pins `:delete` over every container
shape a local can hold. Part of #14701.
