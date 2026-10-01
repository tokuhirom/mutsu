# Drop bincode's unused `derive` feature

`bincode` is only used through its serde bridge (precomp and scan caches); nothing derives
`bincode::Encode`/`Decode`. Disabling default features removes the `bincode_derive` proc-macro and
`virtue` from the build graph and from `Cargo.lock`, with no change to the on-disk format.
