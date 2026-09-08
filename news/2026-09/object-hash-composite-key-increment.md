# `%h{$composite}++` on an object hash accumulates again

```raku
my %p{Any};
my $p = (3, 4);
%p{$p} = 1;
%p{$p}++;
say %p{$p};      # raku: 2   mutsu (before): 1
say %p.elems;    # raku: 1   mutsu (before): 2
```

An object hash keyed by a *composite* value — a `List`, an `Array`, a `Hash` —
stored the `++` result under a second key. `.elems` reported 2 for what the
program had written as one entry, and the read still found the original, so the
increment looked like it had silently done nothing. A scalar key was fine, and
so were `+=` and `~=`; only the `++`/`--` read-modify-write path diverged.

## Root cause

An itemized aggregate used as a hash subscript is **one key, not a slice**, and
the read / assign / `:exists` / `:delete` paths all normalize such an index to a
`Scalar` wrapper before keying — that wrapper is the one shape the slice
machinery does not read as a list, and it is what makes those four paths agree.
`exec_inc_dec_index_op` never applied that normalization. So `=` keyed the entry
by the wrapper's `.WHICH` (`List|3 4`) while `++` keyed it by the raw list node's
own per-node identity (`Array|1`), and the two landed in different buckets.

`+=`/`~=` were unaffected because a compound assignment routes through the
element-assign path, which does normalize.

## The fix

`exec_inc_dec_index_op` (`src/vm/vm_var_assign_post_incdec.rs`) now applies the
same itemized-aggregate → `Scalar` normalization as the element-assign path,
gated on a hash target so a positional subscript keeps its own rule (an itemized
list is a single *numeric* index there). The `original_keys` record it writes for
an object hash also goes through `Interpreter::object_hash_key_value`, so the key
handed back by `.keys`/`.kv`/`.raku` is de-itemized exactly as the assign path
records it.

Pinned by `t/object-hash-composite-key-increment.t`, whose 23 assertions pass
unchanged under rakudo: `++`, `--`, prefix `++`, `+=` and `~=` over `List`,
itemized `List`, `Array`, `Hash`, instance and scalar keys, plus the
autovivifying `++` on an absent composite key.

Closes #7538.
