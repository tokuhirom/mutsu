# `.ASSIGN-POS`/`.BIND-POS`, shaped-array stores and `is Array` stores are O(1)

Three element-store paths cost O(e) per store, so a loop filling an array
through any of them was quadratic (#9157):

- `.ASSIGN-POS`/`.BIND-POS`/`.DELETE-POS` called as methods. Single-index
  `BIND-POS` and every multi-index form rebuilt the array (and each level on the
  index path), then rewrote the env bindings that pointed at the old node. They
  now write in place through each level's shared node, the way `@a[$i] = $v`
  already did. The method block's preamble no longer scans the env for the
  array's type constraint either: the element type is read off the container
  (`ArrayData::value_type`), and the env is consulted only for the variable
  name a failed type check reports.
- Element write/delete on a shaped array (`my @a[N]`). `shaped_array_shape`
  re-validated the whole element structure on every call, twice per store. The
  shape is a fixed attribute of the container, so a cached shape is now checked
  along the first-child spine only (O(d)), and a 1-dim shape recovered from a
  flat array is cached rather than re-derived.
- A store into an `is Array` instance (`g()[$_] = 1`, `$obj[$i] = $v`, an
  accessor-lvalue store) copied the instance's storage array per store. It now
  goes through `Gc::make_mut`: in place when the storage node is singly owned,
  copied once when it is shared (so a `.clone` taken earlier still does not see
  the write).

`scripts/array-complexity-check.sh` (release, 4-core container, `SCALE=4`,
N = 40000); the issue measured 3.87 (ASSIGN-POS), 4.07 (shaped write) and ~3.9
(`is Array` store) before:

| case | ratio t(2N)/t(N) |
|---|---:|
| `@a.ASSIGN-POS($_, 1) for ^N` | 1.41 |
| `@a.BIND-POS($_, 1) for ^N` | 1.75 |
| `@a.ASSIGN-POS($_, 1, 1) for ^N` | 2.19 |
| `my @a[N]; @a[$_] = $_ for ^N` | 1.93 |
| `my @a[N]; @a[$_]:delete for ^N` | 2.15 (`SCALE=10`) |
| `g()[$_] = 1 for ^N` into an `is Array` instance | 1.88 |

The four new cases were added to the script. A side effect: a multi-index
`ASSIGN-POS` past the end now autovivifies only the indexed slot and leaves the
skipped slots as holes (`[Any, Any, [Any, 5]]`, as raku), where it used to fill
them with empty arrays.
