# `Baggy.hash` / `Mixy.hash` return a parameterized Hash

`bag(<a b b>).hash` now has type `Hash[UInt,Mu,Any]` (`Hash[Real,Mu,Any]` for a Mix), with
`.keyof` `(Mu)` and `.of` `(UInt)`/`(Real)`, matching Rakudo. `.hash` on a Set/Bag/Mix had its own
copy of the coercion in `builtins/methods_0arg/collection.rs`; it now calls the same
`map_hash_coerce::to_hash` as `.Hash`, so the two can no longer drift. `.keyof` on a Hash reads the
key type from a `Hash[V,K,..]` declared type when no object-hash key type is set.
