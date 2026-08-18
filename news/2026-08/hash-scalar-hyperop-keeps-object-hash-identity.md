# A symbolic hash-scalar hyper op no longer demotes an object hash to a plain Hash

`%h{Any} >>op>> scalar` (and the other dwim-valid arrow combinations —
`<<op>>`/`3 <<op<<`/`3 <<op>>`) silently lost the hash's object-hash identity:
`(%h{Any} >>~>> 3).WHAT` reported `(Hash)` instead of `(Hash[Any,Any])`, and
`.raku` rendered the plain `{...}` form instead of the typed
`(my Any %{Any} = ...)` form.

Root cause: `Interpreter::hyper_op_pair`'s two hash-vs-scalar branches
(`vm/vm_hyper_ops.rs`) built a fresh `HashData` for the result, copying only
the key/value pairs and dropping `key_type`/`value_type`/`declared_type`/
`original_keys` — even though the key set does not change in this branch, so
there is no reason the metadata should be lost. The hash-vs-hash branch
immediately above it already carried this metadata over correctly; the
hash-vs-scalar branches were simply missed.

Fixed by copying the source hash's type metadata onto the result, mirroring
the hash-hash branch and the `tagged_hash` helper `vm_hyper_func.rs` already
uses for the same purpose on the `&op`/`&metaop` lexical-variable call path.

Pinned by `t/hyper-hash-scalar-object-hash-type.t` (8 assertions, covering
all four dwim-valid arrow combinations in both operand orders, plus a
non-`Str`-keyed object hash), verified against `raku`.

Found while investigating `roast/S03-metaops/infix.t`'s regression under the
real `Test.rakumod` (`todo/deep/vendor-real-test-module.md`); the file's own
171 failing subtests turned out to be a different, deeper bug in hash
container-identity write-through, tracked separately in
`todo/deep/hash-pointy-param-writeback-loses-object-hash-identity.md`.
