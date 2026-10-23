# An imported routine's parameter default resolves names in its own package

A routine pulled in by `require M <&sub>` runs its parameter defaults when the caller
omits the argument, and those defaults were evaluated in the caller's package. A default
that names a type the importer never imported (`HashAlgorithm $a =
HashAlgorithm(crypto_pwhash_alg_default())`) therefore died with "Unknown function".
The defaults now run in the routine's own package, so `Crypt::SodiumPasswordHash`'s
`t/030-hash.t` matches rakudo.
