# Set operators keep the Hashes of an Array whole

`@x ∪ @y` (and `∩ ∖ ⊖`) flattened a Hash held in a slot of a real Array into its
pairs, so two Arrays of three row-Hashes united to the keys of the rows. The
slots of an Array are containers: a Hash in one is a single element keyed by its
`.WHICH`. A bare List still flattens its Hashes (`(%h, %g).Set` is three
pairs), as Rakudo does (#17325, Qwiratry's `slang-direct-evaluation`).
