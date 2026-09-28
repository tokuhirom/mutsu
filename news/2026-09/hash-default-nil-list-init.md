# A hash's `is default(...)` was ignored by list-initialization's `Nil` pairs

`my %h is default(Nil) = a => Nil; say %h;` stored `{a => (Any)}` instead of
`{a => Nil}` — the scalar and array forms of the same declaration already
resolved to the declared default correctly.

`exec_apply_var_trait_op` (`src/vm/vm_var_trait_ops.rs`) applies `is
default(...)` in an opcode that runs AFTER the declaration's own store, so
that store's `Nil`-to-default substitution can't see the default yet: for a
`%`-sigil declaration, `coerce_hash_var_value`'s `substitute_nil_pair_values`
finds no `var_default` registered, and `build_hash_from_items` decays the
still-bare `Nil` pair value to `Any` (an untyped hash's own default) before
the trait ever runs. The `default` trait handler already swept an `@`-sigil
array's stored elements for `Nil`/`Any` holes and replaced them with the
declared default after the fact, but had no equivalent sweep for a `%`-sigil
hash's stored values — so the array form self-corrected and the hash form
did not.

Added the missing hash branch, mirroring the array one exactly (including its
same pre-existing inability to tell an explicitly-written `Any` pair value
apart from a decayed hole — a divergence from raku already accepted for
arrays and out of scope here). Test:
`t/collections/hash/hash-default-nil-list-init.t` (GH #9832).
