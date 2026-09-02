# A `:=` bind of an immutable `List` element produces a writable container

A `List` is immutable: its elements are plain values, not `Scalar` containers.
Rakudo therefore refuses a write through a name bound to one, while the same
bind into an `Array` element writes through:

```raku
my @t := (5, 6);   my \a := @t[0]; a = 10;   # rakudo: Cannot modify an immutable Int (5)
my @t  = (5, 6);   my \a := @t[0]; a = 10;   # rakudo: [10 6]
my $x = 1; my @t := ($x, 6); my \a := @t[0]; a = 10;   # rakudo: sets $x to 10
```

mutsu gets the middle and last lines right but silently allows the first,
because `OpCode::IndexAutovivifyLazyTerminal` promotes ANY scalar leaf to a
fresh `ContainerRef` cell (`Value::array_slot_ref`), regardless of whether the
container it came from is a real `Array` or an immutable `List`. The write then
lands in a cell that only the binding can see. The same over-promotion makes
`my $x := (5, 6)[0]; $x = 10` and `my $y := (5, 6)[0]` wrongly writable.

## Why this is deep, not a one-line guard

The obvious fix — decline the promotion in
`exec_index_autovivify_lazy_op` whenever `ArrayKind::is_immutable_list()` —
was implemented and measured on 2026-09-02. It is correct in isolation and
passes its own tests, but **several consumers lean on the promotion**, and a
full local `make roast` catches them:

| consumer | symptom when the promotion is declined |
|---|---|
| a chunked multi-parameter loop over a flat list (`for @ok -> \value, @strings`) | the parameter stops refreshing per iteration — `roast/S32-str/val.t` fails 1201 subtests |
| `.kv` on a mutable QuantHash (`for $b.kv -> \k, \v { v = 5 }`) | the weight write-back is lost |
| `.kv` on a `Pair` inside a closure (`roast/S32-hash/kv.t` 27) | the `is rw` alias stops writing back |

Each is a separate latent gap that the promotion has been papering over:

* **QuantHash `.kv`** is not routed through
  `try_quanthash_weight_pair_producer` (only `.pairs`/`.values` are), so it
  hands out plain weights. Routing `.kv` there *and* teaching the
  slot-addressed store (`vm_var_assign_set_local.rs`, the `ContainerRef`
  write-through) to honour `quanthash_weight_ref` — the by-name path in
  `vm_misc_assign.rs` already does — fixes it. Both changes were prototyped
  and verified on 2026-09-02; they were reverted only because the guard they
  compensated for was itself backed out.
* **`Pair.kv` / `Pair.values`** do not hand out the pair's value container at
  all: `my $p = (a => my $ = 42); my @l := $p.kv.list; my \v := @l[1]; v = 99`
  leaves `$p.value` at 42 (rakudo: 99). Today's roast test passes only through
  the loop's own rw-writeback, which the closure case cannot use.
* **the chunked loop parameter** is the big one: whatever refreshes a
  sigilless loop parameter per iteration goes through the promoted cell, so it
  needs its own investigation before the guard can land.

## What shipped instead

`news/2026-09/list-destructuring-sigilless-bind.md`: the guard is applied only
where it is provably safe — `OpCode::IndexAutovivifyLazyTerminal` gained a
`sigilless` flag, set only for a `my \a := ...` bind (single or
list-destructuring), so `my (\a, \b) := (5, 6); a = 10` dies correctly while
every consumer above keeps the promotion it depends on.

Closing this ticket means removing that narrowing: fix the three consumers,
then make the rule unconditional and drop the flag.

## Minimal repro

```
mutsu -e 'my @t := (5, 6); my $x := @t[0]; $x = 10; say @t'   # (10 6); raku dies
```
