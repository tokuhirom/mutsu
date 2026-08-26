# `@a[i;j]:v` (and `:k`) on a multidim hole returns `Nil`, not `()`

Found while fixing `todo/tickets/multidim-exists-adverb-blind-to-initialized-and-typed-holes.md`
(see `news/2026-08/multidim-exists-adverb-canonical-hole-predicate.md`). Pre-existing, unrelated to
that fix's hole-predicate consolidation -- reproduces identically before and after it, on a plain
untyped shaped array with no typed/initialized subtlety at all:

```
my @a[2;2];
say @a[0;1]:v;
```

raku: `()` (an empty list -- `say` prints a blank line).
mutsu: `Nil` (`say` prints the literal word `Nil`).

Compare with the single-dimension (non-multidim) form, which is already correct and pinned by
`t/typed-array-hole-adverbs.t`: `is-deeply (@j[0]:v), (), ...`.

## Root cause

`builtin_multidim_subscript_adverb` (`src/runtime/builtins_multidim_ops.rs`, the non-Whatever/list
single-coordinate branch) and its siblings (`multidim_subscript_adverb_multi`,
`builtin_multidim_subscript_adverb_dyn`) return `Value::NIL` directly from the `"v"`/`"not-v"` (and
similarly-shaped `"k"`/`"not-k"`) arms when the queried element does not exist:

```rust
"v" => {
    if exists {
        Ok(array_to_list(value))
    } else {
        Ok(Value::NIL)
    }
}
```

but raku's `:v` on a missing multidim slot answers an *empty list*, not `Nil` -- the same shape
`Array.WHERE`'s existing single-dimension implementation already gets right by returning
`Value::array_with_kind(vec![], ArrayKind::List)` (or equivalent) instead of `Value::NIL`. Every
`else => Value::NIL` arm across the three handlers above needs the same swap; `"k"`/`"not-k"` need
checking too (raku's `:k` on a miss is also `()`, not `Nil`).

## Scope

Small, self-contained, no design needed -- swap `Value::NIL` for an empty `List`-kind array in the
relevant arms of `builtin_multidim_subscript_adverb`, `multidim_subscript_adverb_multi`, and
`builtin_multidim_subscript_adverb_dyn` (`src/runtime/builtins_multidim_ops.rs`), verify each adverb
(`:v`, `:k`, `:not-v`, `:not-k`, and check `:p`/`:kv`'s existing empty-array-vs-Nil choice too) against
`raku -e` on a plain untyped multidim hole, and add/extend a regression test (e.g. alongside
`t/typed-array-hole-adverbs.t` or a new `t/multidim-value-adverb-hole-shape.t`).
