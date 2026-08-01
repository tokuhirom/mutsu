# A typed array's hole reads back as the element type object under `:v` / `:k`

`:exists` and the value adverbs use two *different* hole predicates, and only one
of them knows about a typed array's element seed:

```raku
my Int @j;
@j[2] = 5;
say @j[0]:exists;    # raku: False   mutsu: False  (agrees)
say (@j[0]:v).raku;  # raku: ()      mutsu: Int
say (@j[0]:k).raku;  # raku: ()      mutsu: 0

my Int @i[3];
say (@i[0]:v).raku;  # raku: ()      mutsu: Int
```

`:exists` goes through `ArrayData::hole_at` (`src/value/value_collections.rs`),
which treats a `Package` cell as a gap marker when its name is `Any` **or the
array's `value_type`**, and then checks the embedded `initialized` set. The value
adverbs go through `resolve_positional_scalar`
(`src/runtime/builtins_multidim_subscript.rs`), which open-codes the same idea
but only recognises `Any`:

```rust
let exists = match bound_map {
    Some(set) => {
        set.contains(&ui)
            || !matches!(items[ui].view(), ValueView::Package(name) if name == "Any")
    }
    None => true,
};
```

So an `Int`-seeded cell is "not a gap marker" there, and the adverb reports the
seed as a real value. The comments on both sites already claim they mirror each
other, so the fix is to make that true: have `resolve_positional_scalar` call
`hole_at`. It takes `items: &[Value]` plus a `bound_map`, not the `ArrayData`, so
either the element type travels alongside (a third parameter) or the two helpers
change to take `&ArrayData` — `format_positional_slice_level` is also called from
`src/vm/vm_var_delete_ops.rs` with a plain snapshot vector and no bound map, so
that call site needs an answer either way.

This is independent of shape: it is wrong for an unshaped `my Int @j` too. Found
while fixing the shaped `:exists` carve-outs
([news](../../news/2026-08/shaped-array-exists-reports-holes.md)), which left the
two predicates visibly disagreeing on the same array.
