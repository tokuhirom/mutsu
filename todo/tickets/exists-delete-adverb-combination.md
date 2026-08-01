# `:exists:delete` on a slice dies with "No such method 'DELETE-KEY'"

```raku
my @a = 1, 2, 3;
say (@a[0,1]:exists:delete).raku;
# raku : (Bool::True, Bool::True)
# mutsu: No such method 'DELETE-KEY' for invocant of type 'List'
```

Combining the two adverbs is legal: `:exists:delete` deletes the elements and
reports whether each one had existed. mutsu evaluates `:exists` first, producing
the *result* list of Bools, and then applies `:delete` to that list instead of to
the original container — hence `DELETE-KEY` on a `List`.

The single-index form has the same shape of bug, one type earlier:

```raku
my @a = 1, 2, 3;
say (@a[0]:exists:delete).raku;
# raku : Bool::True
# mutsu: No such method 'DELETE-KEY' for invocant of type 'Bool'
```

## Where

`exec_exists_index_adv_op` (`src/vm/vm_var_exists_ops.rs`) owns the `:exists`
family and decodes the other adverbs out of `adverb_bits` — but only `:k`, `:v`,
`:kv` and `:p`, each of which changes the *shape of its own answer*. `:delete` is
not in that set; it is a separate opcode applied to whatever `:exists` left on the
stack. The two invalid combinations (`:exists:k`, `:exists:v`) are already
rejected with a runtime error in the same function, so the adverb decoding is the
right place to notice this one too.

The fix is to make the combination one operation: delete each index (through the
same path the standalone `:delete` uses) and answer the pre-delete existence, in
the shape the index form dictates — a bare Bool for a single index, one Bool per
index for a slice, matching the shape rule pinned by `t/range-index-exists.t`.

Found while fixing the Range-index `:exists` slice
([news](../../news/2026-08/range-index-exists-is-a-slice.md)); the error message
changed type there (`Bool` -> `List`) only because the Range now correctly
produces a list, so this is not a regression from that change — it reproduces on
`@a[0,1]` and on `@a[0]` alike.
