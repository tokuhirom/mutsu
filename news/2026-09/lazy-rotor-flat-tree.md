# An Array's `.rotor`, `.flat` and `.tree` are lazy

These three methods built their whole result at the call, so `@a.rotor(2).head(3)` or
`@a.flat.head(3)` cost O(e). Each of them is now a lazy `Seq` over a `ListGen` (#9158), like
`.keys`, `.pairs` and `.batch` before them. The Array is read live, as Rakudo reads it.

- **`.rotor`.** The stepping loop moved out of `dispatch_rotor` into `RotorState::step`
  (`src/value/list_gen_rotor.rs`). The eager path and the lazy path share this one
  implementation. The eager path is still used for a non-Array invocant, and for any spec whose
  negative gap could step before the start of the list. That spec throws `X::OutOfRange`
  partway through, and a pure iterator cannot throw.
- **`.flat`.** Each element is flattened in turn by the same `flat_val` the eager path uses.
  The elements of a real Array stay single; the elements of a List flatten.
- **`.tree`.** Each level of `tree_to_depth` over an Array is now a lazy itemized Seq that trees
  an element as it is pulled.

A consuming `.head(n)` / `.first` on a Seq held in a Scalar cell also takes only its prefix
now. `.tree` returns exactly such a Seq, `$(...)`. This fixes `@a.tree.head(3)`, which
reified the whole tree before.

Measured on a release build, 100 × `.head(3)` over 200 000 and 400 000 elements takes a flat
~2 ms for `rotor(2)`, `flat` and `tree`, a ratio of about 1. With this, every method named in
#9158 pulls only what its consumer needs.
