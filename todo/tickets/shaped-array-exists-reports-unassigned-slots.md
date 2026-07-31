# `:exists` on a shaped array reports every in-range slot as existing

raku distinguishes an assigned slot of a shaped array from an unassigned one;
mutsu answers `True` for every in-range index:

```raku
my @t[3];
say @t[0]:exists;      # raku: False,  mutsu: True
say @t.EXISTS-POS(1);  # raku: False,  mutsu: True
@t[1] = 5;
say @t[1]:exists;      # raku: True,   mutsu: True (agrees)

my @s[2;2];
say @s.EXISTS-POS(0, 0);  # raku: False, mutsu: False (agrees — the multidim
                          # path already answers correctly)
```

Both single-index paths deliberately special-case shaped arrays:
`slot_present_at` in `exec_exists_index_adv_op` (`src/vm/vm_var_exists_ops.rs`)
returns `i >= 0 && (i as usize) < items.len()` for `ArrayKind::Shaped` before it
consults the hole predicate, with the comment "Shaped arrays are fixed-size: any
in-range index exists, regardless of whether the slot holds the (default) Nil
value". `native_method_1arg`'s `EXISTS-POS`
(`src/builtins/methods_narg/dispatch_1arg.rs`) and the value-level `EXISTS-POS`
in `methods_call_dispatch.rs` carry the same `shape.is_some()` guard, added to
keep them consistent with the opcode.

The predicate mutsu already has is the right one — `ArrayData::hole_at` reads the
embedded `initialized` set, which is exactly "was this slot explicitly
assigned". The blocker is that a shaped array is allocated pre-filled with its
element type object, so unless declaration seeds `initialized` as empty (rather
than leaving it `None`, which means "bulk-constructed, no gaps"), dropping the
guard would make every slot of a *literal-initialized* shaped array
(`my @m[2;2] = (1,2),(3,4)`) read as a hole instead. So the fix is in shaped
allocation, not in the three `:exists` sites, and it needs a roast pass over
S09-typed-arrays.

Found while routing `:delete` on a mixin through DELETE-KEY/DELETE-POS
([news](../../news/2026-07/delete-adverb-dispatches-through-a-mixin.md)), which
made the hole-blind `EXISTS-POS` visible; the unshaped half of that was fixed
there, this shaped half was not.
