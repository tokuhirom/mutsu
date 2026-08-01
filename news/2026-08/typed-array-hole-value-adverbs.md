# A typed array's hole no longer reads back as its element type

`:exists` and the value adverbs (`:v` / `:k` / `:p` / `:kv`) answer the same
question — is there anything in this slot? — and they used two different
predicates to answer it. Only one of them knew that a typed array's gap marker
is the *element type* object, not `Any`:

```raku
my Int @j;
@j[2] = 5;
say @j[0]:exists;    # False       (right)
say (@j[0]:v).raku;  # was Int,  now ()
say (@j[0]:k).raku;  # was 0,    now ()
say (@j[0,1,2]:v);   # was (Int Int 5), now (5)
```

The seed leaked out as if it were a real value, so a slice over a
mostly-unassigned `my Int @a` reported every gap as an `Int` element, and `:k`
handed back index positions that `:exists` denied. It was wrong for a shaped
typed array too (`my Int @i[3]`), and for every element type — `my Str @s`
reported `Str`.

`ArrayData::hole_at` is the canonical predicate and was already correct: a
`Package` slot is a gap when its name is `Any` **or** the array's `value_type`,
and when the embedded `initialized` set says the index was never explicitly
assigned. `:exists` calls it. `resolve_positional_scalar` in
`src/runtime/builtins_multidim_subscript.rs` open-coded the same idea instead,
and its copy recognised only `Any`:

```rust
set.contains(&ui)
    || !matches!(items[ui].view(), ValueView::Package(name) if name == "Any")
```

Both helpers now take the `&ArrayData` rather than an element slice plus a
loose `bound_map`, and `resolve_positional_scalar` is a single
`!data.hole_at(ui)`. Passing the whole container is what makes the two
predicates agree by construction: the element type and the `initialized` set
are the two halves of the answer, and they only travel together on the
`ArrayData`. The comments at both sites had claimed they mirrored each other;
that is now true rather than aspirational.

The one caller that has no live array — the nested `:delete` path in
`src/vm/vm_var_delete_ops.rs`, which formats a pre-delete snapshot — clones the
`ArrayData` instead of just its elements. It passes `keep_missing = true`, so
no slot is dropped there whatever the predicate says, but the snapshot should
not look like an array of a different element type.

An untyped array is untouched, including the case that makes the predicate
subtle: `my @n = 1, Nil, 3` assigns `Nil` to a slot, which reads back as `Any`
and *does* exist — a written `Any` is a value, an autovivification gap is not.

Pinned in `t/typed-array-hole-adverbs.t`; all 28 assertions pass unmodified
under rakudo. Found while fixing the shaped `:exists` carve-outs
([news](shaped-array-exists-reports-holes.md)), which left the two predicates
visibly disagreeing about the same array.
