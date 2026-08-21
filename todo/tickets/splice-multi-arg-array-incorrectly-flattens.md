# `.splice()` incorrectly flattens an Array/List replacement arg even when it is not the sole replacement arg

Discovered while implementing ADR-0040 Slices 0-1 (element-store itemization) and
writing its acceptance oracle, `t/element-store-itemization.t`.

## Root cause

`do_splice` (`src/runtime/methods_mut_dispatch.rs`, inside the `"splice"` match arm) builds
the replacement-element list with:

```rust
for arg in args.iter().skip(2) {
    match arg.view() {
        ValueView::Array(arr, ..) => {
            new_items.extend(arr.iter().cloned());
        }
        _ => new_items.push(arg.clone()),
    }
}
```

This flattens **every** `Array`/`List`-kind argument unconditionally, regardless of how many
replacement arguments were passed. But raku's `splice` follows the same "one-arg rule" as
`push`/`append`/`unshift`/`prepend`: an `Array`/`List` argument flattens only when it is the
**sole** replacement argument; when there are multiple replacement arguments, each one
(including an `Array`/`List`) becomes exactly one element.

## Repro

```
my @a = 1,2,3;
@a.splice(1,1,"x",[7,8]);
say @a.raku;
```

- raku: `[1, "x", [7, 8], 3]` (4 elements: `[7,8]` is kept as ONE element)
- mutsu: `[1, "x", 7, 8, 3]` (5 elements: `[7,8]` flattens even though `"x"` is also present)

Also reproduces with two Array/List args and no leading scalar:

```
my @a = 1,2,3;
@a.splice(1,1,[7,8],[9,0]);
say @a.elems;
```

- raku: `4` (each array kept as one element)
- mutsu: `6` (both arrays flattened)

Additionally, a *single* replacement Array/List argument flattens in raku **unconditionally**,
even when it is already itemized (`$[7,8]`) — this differs from `push`/`append`'s one-arg rule,
where an itemized single argument does NOT flatten (`t/append-one-arg-rule.t`). So `splice`'s
correct rule is not simply "reuse `flatten_append_args`" — it needs its own one-arg-rule variant
that ignores itemization for the single-arg flatten decision, but still keeps non-flattened
elements (the multi-arg case) itemized per ADR-0040.

## Why this is a separate ticket

This is a pre-existing arity/flattening bug, orthogonal to ADR-0040's element-store
itemization work (`docs/adr/0040-array-hash-elements-are-itemized-at-the-store.md`). ADR-0040
Slice 1 applies itemization to whatever `do_splice` decides is a single kept-whole element (the
`_ => new_items.push(...)` branch, which already correctly handles non-Array discrete args like
a `Range` — see `t/element-store-itemization.t`'s M3 row); it deliberately does not touch the
Array-flattening decision logic above, to keep the itemization PR's blast radius scoped to
itemization only.

## Suggested fix

Replace the per-arg-independent flattening loop with a one-arg-rule check scoped to
`args[2..]` as a whole (mirroring `flatten_append_args`, `src/runtime/mod.rs:63-88`, but with
splice's own "flatten even if itemized" quirk for the single-arg case):

```rust
let post = &args[2..];
if post.len() == 1 {
    // flatten unconditionally (Array/List/Seq/Hash/Range), regardless of itemization
} else {
    // each arg becomes exactly one element (itemized per ADR-0040 where applicable)
}
```

Needs a `raku -e` sweep of the single-arg case across Array/List/itemized-Array/Seq/Hash/Range
to confirm which of those still flatten unconditionally for `splice` specifically (this ticket
only measured Array/List), then a regression test file (`t/splice-arg-flatten-rule.t` or
similar) covering both the arity and (once ADR-0040 lands) the itemization of kept-whole
elements.
