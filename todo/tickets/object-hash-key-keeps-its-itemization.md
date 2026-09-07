# An object hash stores the key still itemized, so `.raku` shows `$(1, 2)`

Found 2026-09-07 while fixing
`news/2026-09/itemized-hash-subscript-is-one-key.md` (an itemized value used as
a hash subscript is one key, not a slice). That fix landed the key correctly —
the entry is one entry and `%j.keys[0].^name` is `List`, as raku says — but the
key retains its **itemization tag**, which raku drops when it stores it.

## Repro

```raku
my %j{List:D};
my $t = $(1, 2);
%j{$t} = "x";
say %j.keys[0].raku;
# raku:  (1, 2)
# mutsu: $(1, 2)
```

```raku
my %h{Any};
my $k = $(1, 2);
%h{$k} = 5;
say %h.raku;
# raku:  (my Any %{Any} = (1, 2) => 5)
# mutsu: (my Any %{Any} = $(1, 2) => 5)
```

Only the itemization survives; everything else about the key is right. It is
therefore purely a store-side normalization gap: raku's object-hash store
decontainerizes the key before keying by its `.WHICH`, mutsu keeps the
`Scalar`-wrapped value it was handed.

## Why it is not just the subscript fix

The subscript paths deliberately normalize an itemized hash index to a `Scalar`
wrapper — that is the one shape the slice machinery does not treat as a list,
and it is what makes read / assign / `:exists` / `:delete` agree on one key
(and on its `.WHICH`). The wrapper is the right *transport*; what is missing is
the object hash unwrapping it when it records the key value it will later hand
back from `.keys` / `.kv` / `.pairs` / `.raku`.

## Where to look

The four index paths are `src/vm/vm_var_index_ops.rs`,
`src/vm/vm_var_assign_index_named.rs`, `src/vm/vm_var_exists_ops.rs` and
`src/vm/vm_var_delete_ops.rs`. The unwrap belongs on the object hash's own
key-recording step, not in those — an itemized key reaching a *plain* hash
stringifies to `"1 2"` and never shows the wrapper, so only the object-hash
store is affected. `Value::deitemize_element` is the existing notion to apply.

## Check when fixing

`.keys`, `.kv`, `.pairs`, `.raku` and `.gist` of an object hash keyed by an
itemized `List`, `Array` and `Hash`; that `%h{$k}` still reads back after the
key is de-itemized (the `.WHICH` must not change under the unwrap, or the
round-trip breaks); and `t/itemized-hash-subscript.t`, which pins the entry
count and the key's `.^name` and must keep passing.
