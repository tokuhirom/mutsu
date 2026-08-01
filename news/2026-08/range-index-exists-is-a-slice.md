# A Range index to `:exists` is a slice, and a one-element slice stays a list

Two related shape bugs in `exec_exists_index_adv_op`
(`src/vm/vm_var_exists_ops.rs`), the opcode behind the `:exists` adverb family.

## A Range index was read as a single key

Every other slice index form reached `:exists` as a list of indices; a Range did
not. Its `match idx.view()` handled `Int`, `Sub` (WhateverCode), `Array`,
`Whatever` and `+Inf`, and a Range fell through to the `_` tail — the
*single-key* path, which stringifies the index and looks it up as one key. So
`@a[0..1]:exists` looked up the key `"0..1"`, found nothing, and answered a
single `False`:

```raku
my @a; @a[0] = 1; @a[1] = 2;
say (@a[0,1]:exists).raku;    # (Bool::True, Bool::True)  -- correct
say (@a[0..1]:exists).raku;   # Bool::False               -- wrong
```

It was wrong for every target kind, not just arrays: `%h{'a'..'b'}`, `$set{...}`,
`$bag{...}`, `$mix{...}` all answered one `False`. The value adverbs were never
affected because they go through `nested_index_elements`, whose last arm is
`_ if idx.is_range()`.

Adding a Range arm per target kind would have meant touching the array path, the
hash/Pair/Stash path and the Set/Bag/Mix/Instance tail separately. Instead the
Range is expanded **once, above every target dispatch**, right after the index is
popped — after the itemized-`$(...)` normalisation, which is a genuinely
different thing (`@a[$(7,8,9)]:exists` is a single index, the element count).
Every existing slice path then handles it unchanged.

The one place that had to learn about it is the multi-dimensional hash walk:
`%h{'a';'b'}` is also an `Array`-valued index, and it traverses *into* a nested
hash. A Range must never be read that way — `my %h = a => { b => 1 }` answers
`(True, False)` for `%h{'a'..'b'}` but `True` for `%h{'a';'b'}` — so the expansion
records that it came from a Range and that arm is skipped.

## A one-element slice collapsed to a scalar

The same function decided the result shape with

```rust
let is_multi = indices.len() != 1 || is_zen;
```

which is a statement about how many indices the form *produced*, when the rule is
about the form itself. A one-element slice is still a slice:

```raku
my @a = 1, 2;
say (@a[0,]:exists).raku;     # raku: (Bool::True,)   mutsu was: Bool::True
say (@a[0..0]:exists).raku;   # raku: (Bool::True,)   mutsu was: Bool::True
say (@a[*]:exists).raku;      # raku: (Bool::True,)   mutsu was: Bool::True  (on a 1-element array)
say (@a[0]:exists).raku;      # raku: Bool::True      -- a bare index, correctly scalar
say (@a[(0)]:exists).raku;    # raku: Bool::True      -- parens do not make a list
```

This predates the Range work but the Range fix makes it reachable far more often
(`0..0`, `^1`, `0..^1`). The count test is replaced by an `index_is_slice` flag
set where the index form is classified — true for an `Array`, `Whatever`, `+Inf`
or zen index, false for `Int` and a resolved WhateverCode. `:kv`, `:p` and `:k`
already got this right, so they now agree with `:exists` by construction.

## Tests

`t/range-index-exists.t` — 25 cases across Array (including holes and a shaped
array), Hash, Set, Bag, Mix, the nested-hash/multidim distinction, negation, the
`:kv`/`:p`/`:k` adverbs, and the shape rule. All 25 produce identical output
under `raku`. `make test` and `make roast` pass.

`todo/tickets/range-index-exists-is-read-as-a-hash-key.md` is resolved by this.
One adjacent bug found and *not* fixed here — `:exists:delete` applies the
`:delete` to the `:exists` result instead of the container — is filed as
`todo/tickets/exists-delete-adverb-combination.md`; it reproduces on plain list
and single-index subscripts too, so it is not a consequence of this change.
