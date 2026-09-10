# Fix named parameter sub-signature destructuring

Named aggregate parameters with sub-signatures now destructure their values
correctly, including inner positional and named slurpies. Destructured slurpies
are bound as `Array` values, and custom `trait_mod` handlers receive the same
correct bindings.
