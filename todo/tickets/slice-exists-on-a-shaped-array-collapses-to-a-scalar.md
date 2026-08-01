# A slice `:exists` on a shaped array collapses to a scalar

A multi-index `:exists` reports one `Bool` per index — unless the target is
shaped, where it answers a single `Bool`:

```raku
my @a; @a[0] = 1; @a[2] = 3;
say (@a[0,1,2]:exists).raku;   # raku: (Bool::True, Bool::False, Bool::True)
                               # mutsu: the same (agrees)

my @z[3];
say (@z[0,1]:exists).raku;     # raku: (Bool::False, Bool::False)
                               # mutsu: Bool::False
```

`exec_exists_index_adv_op` (`src/vm/vm_var_exists_ops.rs`) routes *any*
Array-valued index on a shaped target into the multidimensional path:

```rust
ValueView::Array(items, ..) if crate::runtime::utils::is_shaped_array(&target) => {
    // Shaped array: multi-dimensional exists (e.g. @arr[0;0]:exists).
    let exists = Self::index_array_multidim(...);
    ...  // pushes ONE Bool and returns
}
```

so `@z[0,1]` is read as the multidimensional `@z[0;1]` rather than as the slice
it is. The two really are different subscripts — raku answers `False` (one Bool)
for `my @z[3]; @z[0;1]:exists` and a two-element list for `@z[0,1]:exists` — so
the disambiguation has to come from which separator was written, not from "the
target is shaped". `;` and `,` produce different index shapes at the parser,
which is where the distinction survives.

Worth checking the same question for the other adverbs (`:v`/`:k`/`:kv`/`:p`) and
for `:delete` on a shaped array while fixing this.

Found while fixing the shaped `:exists` carve-outs
([news](../../news/2026-08/shaped-array-exists-reports-holes.md)); this half is
about the index shape, not about holes, so it was left out of that change.
