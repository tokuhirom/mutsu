# `:delete` trims a shaped array down, discarding its shape

Deleting the last assigned slot of a shaped array collapses the whole array to
empty:

```raku
my @z[3];
@z[2] = 3;
@z[2]:delete;
say @z.raku;    # raku: Array.new(:shape(3,), [Any, Any, Any])   mutsu: []
say @z.elems;   # raku: 3                                        mutsu: 0

my @y[3];
@y[0] = 1;
@y[0, 1]:delete;
say @y.raku;    # raku: Array.new(:shape(3,), [Any, Any, Any])   mutsu: []
```

A shaped array is fixed-size: `:delete` empties a slot, it cannot shorten the
array. `trim_trailing_array_holes` (`src/vm/vm_var_delete_ops.rs`) pops every
trailing hole off the backing without asking whether the array is shaped, so
once the deletion leaves only holes above some point, those slots are gone and
`.elems` follows. Trimming *is* right for an unshaped array — `my @a; @a[0] = 1;
@a[0,1]:delete` really does leave `[]` in raku — so the fix is to skip the trim
for `ArrayKind::Shaped` (and to leave the shape metadata attached, which the
rebuilt backing also has to carry).

It is not specific to the slice form: the single-index `@z[2]:delete` above hits
it too. A delete that does *not* strand a trailing hole is already right
(`my @w[3]` fully assigned, `@w[1]:delete` → `[1, Any, 3]` with `.elems` 3),
which is why this survived — only the trailing case reaches the trim.

Two smaller things to settle in the same change:

- A slice `:delete` reports the deleted values as `(1, Any)` where raku says
  `(1, Nil)` — the never-assigned slot deletes to `Nil`, not to the array's
  hole type.
- `.raku` must keep printing `Array.new(:shape(3,), ...)` after the delete; it
  does today only because the trim path happens to leave the metadata alone
  when it does not fire.

Found while fixing the shaped-array slice `:exists` collapse
([news](../../news/2026-08/shaped-array-slice-exists.md)).
