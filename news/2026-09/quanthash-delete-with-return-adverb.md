# `:delete:k` and friends now delete from a SetHash/BagHash/MixHash

`$mix{$k}:delete:k` returned `$k` but left the element in the MixHash. The same happened
for `:delete:v`, `:delete:p` and `:delete:kv`, on SetHash and BagHash too, and on slices
([#9335](https://github.com/tokuhirom/mutsu/issues/9335)). A plain `:delete` worked, and so
did the combined form on a `Hash`.

**List::UtilsBy** 0.0.8 hit this in `weighted_shuffle_by`:

```raku
@out.push( $mix{$mix.roll}:delete:k ) while $mix;
```

The MixHash never shrank, so the loop never ended and `t/weighted_shuffle_by.rakutest` timed
out.

A combined adverb goes through `builtin_subscript_adverb`
(`src/runtime/builtins_multidim_subscript.rs`). For a QuantHash, that function reads the
rows from a `.hash` projection so it can reuse the Hash key/value logic. The `:delete`
companion then removed the keys from that projection, which is a throwaway copy; a `TODO`
there already said so. It now remembers the original QuantHash and removes the keys through
`delete_from_container`, the same removal the plain `:delete` opcode performs through the
container's shared backing node. It also refuses an immutable `Set`/`Bag`/`Mix` with the
plain path's `X::Assignment::RO`.

Pinned by `t/collections/set-bag-mix/quanthash-delete-with-adverb.t`.
