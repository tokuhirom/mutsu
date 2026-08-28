# Binding an out-of-range array element no longer grows the array

`my @a = 1, 2; my $r := @a[5]` used to report six elements. Raku reports two:
the binding vivifies nothing until something is actually written through it, and
only then fills the gap. mutsu grew the array at *bind* time, so `@a.elems` was
`6`, `@a.raku` was `[1, 2, Any, Any, Any, Any]`, and `@a[5]:exists` answered
`True` for a slot nobody had touched. The write direction was already right — the
divergence was entirely in the eagerness.

## Root cause

`Value::array_slot_ref` (`src/value/value_methods_b.rs`) is the shared primitive
behind every container-producing subscript path: `:=`-bound elements, the
`:p`/`:kv` subscript adverbs (ADR-0036 slice 2), the `.values`/`.reverse`/`.sort`
producers (ADR-0036 slice 3 / ADR-0045 slice 4), `for` loop parameter binding
(ADR-0045), and `return-rw` subscript operands. It grew the vec unconditionally
(`while data.len() <= idx { data.push(hole.clone()) }`) before promoting the
element to a shared `ContainerRef` cell, and its own doc comment described that
as intentional.

The hash twin never had the problem. `hash_slot_ref` hands back a *deferred*
`HashEntryRef` path token for a missing key and creates nothing; the first write
walk-creates the path. What the array side lacked was a **root**: `EntryRoot` had
only `Hash` and `Cell`. `EntryStep::Index`, `EntryTerminal::Array` and its
gap-filling `insert()` all existed already — the deferred-path machinery had been
array-capable for a while with no way to anchor a path on an array.

## What changed

- **`EntryRoot::Array(Gc<ArrayData>)`** (`src/value/entry_path.rs`), handled in
  `level` / `level_mut` (an array root presents a `Level::Array` to an `Index`
  first step and no level to a `Key` step, exactly as a hash root is fixed the
  other way), with the module doc and the `EntryRoot` doc rewritten — both had
  described the eager array side as the known asymmetry.
- **The GC visit arm** for the new root (`src/value/value_gc.rs`), with a unit
  test alongside the existing `Hash`/`Cell` ones. A missed root is a
  under-collection bug, not a cosmetic omission.
- **`array_slot_ref` stays lazy for a terminal index past the end**: it mints the
  token instead of growing. A *non-terminal* (intermediate descent) step still
  grows eagerly — the level has to exist before the next subscript can descend
  it, which is the same contract `hash_autovivify_cell` has on the hash side. The
  growth itself moved into a new `Value::array_grow_to`, so the one caller that
  still wants it says so explicitly.
- **`EntryTerminal::unwritten_read`**: an unconnected deferred bind reads as what
  an unwritten slot of that container holds. For a hash entry that is `Any`; for
  an array index past the end it is the array's *hole* value, so
  `my Int @i; my $r := @i[5]; say $r` prints `Int` and an `is default(42)` array
  prints `42` — both verified against rakudo. `hash_entry_read` routes through it,
  leaving the hash side byte-identical.
- **A COW re-anchor arm** in `vm_var_assign_index_named.rs`: an element assignment
  that detaches the array `Gc` re-points array-rooted tokens the same way it
  already re-pointed hash-rooted ones.
- **The three call sites that cannot carry a token yet grow explicitly.** The
  growth `array_slot_ref` used to do unconditionally now lives in
  `Value::array_grow_to`, and the bound-slice arm of
  `exec_index_autovivify_lazy_op` plus `multi_dim_scalar_autoviv_cell` and
  `collect_multi_dim_leaf_cells` call it before promoting. Each promotes into an
  element of *another* array, and neither `resolve_array_entry` nor the
  bound-slice write-through knows about tokens — letting one through there made
  `roast/S32-array/multislice-6e.t` read the raw token for `@array[0;0;3]` and
  silently swallow `@array[*;0;3] = 999`. Each site carries a `// TODO` and the
  gap is tracked in `todo/tickets/bound-array-slice-still-vivifies-eagerly.md`.

## What it unblocked

**ADR-0036 §1.3 row 10.** `my @a = <A B>; my $p = 0 => @a[0]; $p.value = "x"`
should alias the element; it was a silent no-op. The fix is the three-line
compiler change slice 3 had already identified — compile a FatArrow's
`Expr::Index` RHS in the container-producing mode (`scalar_bind_autovivify` +
`bind_terminal`) that the `=:=` and `return-rw` arms use, so the Pair's value *is*
the element's shared cell. It was held back precisely by this ticket:
`key => @a[i]` is ordinary, common code, and routing it through a primitive that
grew the array would have extended arrays at pair-construction time. With the
token in place it lands, out-of-range case included
(`my $p = 'k' => @a[5]` grows nothing until `.value` is written). The row-10
`todo` in `t/subscript-pair-element-container.t` is gone and the ADR records the
status.

## Pins

`t/array-slot-ref-deferred-vivification.t` (23 assertions: bind without write,
bind then write, typed / `is default(...)` hole values, nested and deep paths,
the "an independent write does not retro-bind" rule the hash side has had since
`t/phantom-entry-bind.t`, and `%h`-side non-regression) plus the two new row-10
assertions in `t/subscript-pair-element-container.t`. Both files pass under real
`raku` as well as mutsu.

## Knowingly left behind

A bound *slice* (`my @s := @a[1,5]`) still grows the source array at bind time.
Its promoted cells live in the slice array itself, and an out-of-range index
would put a token where the array read/display chokepoints and the bound-slice
write-through do not expect one. Recorded as
`todo/tickets/bound-array-slice-still-vivifies-eagerly.md`.
