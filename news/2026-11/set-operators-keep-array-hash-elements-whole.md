# Set operators keep an itemized Hash whole

`@x ∪ @y` (and `∩ ∖ ⊖`) flattened an itemized Hash (a row of `[ %(...), ... ]`,
`$%h`) into its pairs, so two lists of three row-Hashes united to the keys of
the rows. An itemized Hash is one opaque element keyed by its `.WHICH`; a bare
Hash in a bare List still flattens (`(%h, %g).Set` is three pairs), as Rakudo
does (#17325, Qwiratry's `slang-direct-evaluation`).
