# `array_slot_ref` grows the array at *bind* time; raku defers until the write

## Symptom

Binding an out-of-range array element grows the array immediately in mutsu,
where raku leaves it alone until something is actually written through the
binding:

```raku
my @a = 1, 2;
my $r := @a[5];
say @a.elems;   # raku 2            mutsu 6
say @a.raku;    # raku [1, 2]       mutsu [1, 2, Any, Any, Any, Any]
```

Raku only vivifies on the write, and then fills the gap:

```raku
my @a = 1, 2; my $p = 0 => @a[5]; $p.value = 9; say @a.raku;
# raku [1, 2, Any, Any, Any, 9]
```

The hash side is already correct: `hash_slot_ref` hands back a **deferred**
`HashEntryRef` path token for a missing key, so a read is non-vivifying and the
eventual write autovivifies the path. The array side has no such token —
`Value::array_slot_ref` (`src/value/value_methods_b.rs`) grows unconditionally
(`while data.len() <= idx { data.push(hole.clone()) }`), which its own doc
comment describes as intentional.

## Why it matters now

`array_slot_ref` is the shared primitive behind a growing number of
container-producing paths:

- `:=`-bound elements (`my $r := @a[i]`),
- the `:p`/`:kv` subscript adverbs (ADR-0036 slice 2),
- `.pairs`/`.values`/`.reverse`/`.sort` (ADR-0036 slice 3 / ADR-0045 slice 4),
- `for` loop parameter binding (ADR-0045 slices 1-3),
- `return-rw` subscript operands, which compile in the same container-producing
  mode (`scalar_bind_autovivify` + `bind_terminal`, see
  `compile_return_rw_arg` in `src/compiler/helpers_call_args.rs`).

Every one of those inherits the eager growth. It is invisible for the in-range
uses that dominate, which is why it has not surfaced — all the consumers above
promote elements that already exist.

**It is the reason ADR-0036 §1.3 row 10 was not landed with slice 3.** Row 10 is
`my $p = 0 => @a[0]; $p.value = "x"`, and the fix is to compile a FatArrow's
Index RHS in the same container-producing mode the `=:=` and `return-rw` arms
already use (`src/compiler/expr_binary.rs`, next to the existing
`scalar_container_alias_name` / `WrapVarRef` capture for a bare `Expr::Var`).
That is a three-line change and it makes row 10 pass — but `key => @a[i]` is
ordinary, common code, so routing it through the primitive would spread this
eager growth to every such pair, including the out-of-range ones. Landing the
row is not worth silently growing arrays at pair-construction time.

## The fix

Give the array side the deferred token the hash side has: `array_slot_ref`
should return a non-vivifying path token for an out-of-range index and promote
only on the write, mirroring `hash_slot_ref`'s `HashEntryRef` arm.
`src/value/entry_path.rs` already contemplates the asymmetry — its doc notes the
array side "vivifies eagerly (`array_slot_ref` grows past the end)" while the
hash side stays lazy.

Once that lands, row 10 is the small compiler change above, and
`t/subscript-pair-element-container.t`'s row-10 `todo` can go.

## Reproduce

The three snippets above, no fixtures.
