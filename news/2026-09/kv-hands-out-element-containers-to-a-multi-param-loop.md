# `.kv` hands out element containers, because a writable multi-parameter now binds raw

`for @a.kv -> $i, $v is rw` propagated a direct write but lost a deferred one:

```raku
my @b = 10, 20; my @c;
for @b.kv -> $i, $v is rw { @c.push(-> { $v = $v + 1 }) }
@c[0](); @c[1]();
say @b;              # raku [11 21]   mutsu [10 20]
```

The hash twin lost it the same way. These are ADR-0045 §1.3 row 16 and its
sibling — the last two rows of that table still marked `todo`, and the last
thing standing between ADR-0045 and its closing sweep.

## The blocker was the consumer, not the producer

ADR-0036 slice 3 built a container-aware producer layer so `.values` /
`.reverse` / `.sort` hand out the elements' own `Scalar` containers, and `.kv`
was left out of it. The reason had nothing to do with `.kv`: a `.kv` loop is a
**multi-parameter** loop, and a multi-parameter loop does not bind at the native
bind site the single-parameter forms use. It binds through the bind-prefix
`Stmt::Assign`s that `build_for_bind_stmts` emits, each reading its chunk slot
through the ordinary element chokepoint — which **decontainerizes**. So a cell
handed out by the producer arrived at `$v` as a bare value, while the writeback
that used to carry the mutation had already been retired for the iteration
precisely *because* the chunk carried a cell. Routing `.kv` without fixing the
bind would have turned a lost deferred write into a lost direct one.

So the bind is what changed. A **writable** scalar multi-parameter — `<->`,
`is rw`, or sigilless — that is positional and has no default now emits
`Stmt::SyntheticBlock([MarkBind, decl])`, the same shape an `@`/`%`-sigil
multi-parameter already used to avoid coercing its chunk element. `array_slot_ref`
is idempotent, which is what makes it work: binding `_[1]` over a chunk slot that
already holds a source cell hands back that cell, so the parameter aliases the
**source** element rather than the temporary chunk. `"kv"` then joins
`ELEMENT_PRODUCERS` in both arms — the array arm yielding a flat
`index, cell, index, cell, …` and the hash arm `key, cell, …`, since the loop
chunks the flat list by two.

**The value is not in `.kv`.** The same raw bind gave a chunked rw
multi-parameter over a plain array the alias it never had:

```raku
my @a = 1, 2, 3, 4; my $c;
for @a -> $x is rw, $y is rw { $c = -> { $x = $x + 1 } if $x == 1 }
@a[0] = 99; $c();
say @a;              # raku [100 2 3 4]; mutsu was [99 2 3 4]
```

And the element type constraint (ADR-0036 slice 4) reaches the `.kv` value slot
for free: `my Int @a; for @a.kv -> $i, $v is rw { $v = "s" }` now dies.

## Three counter-currents, all real

ADR-0045 §8 predicted that any *new* place a cell can reach would be found by a
sweep rather than by reading, and all three of these were — the third only by
the full roast run:

- A **mutable QuantHash**'s `.kv` writeback read its parameter straight out of
  `env` and got a `ContainerRef`, so `for $baghash.kv -> $k, $v is rw { $v = 6 }`
  set every weight to 1. A QuantHash weight is not a stored element container
  (ADR-0036 §2.4 keeps that arm on the writeback deliberately), so it
  decontainerizes now.
- An **object hash**'s `.raku` rendered the promoted cell, turning
  `1 => "a"` into `1 => a`. That one was pre-existing — a bare `%h.values` call
  did it on `main` — and `.kv` merely made `t/object-hash-which-keys.t` walk into
  it. `dispatch_constrained_hash_raku` decontainerizes now, which is ADR-0045
  row 40's "promotion is invisible" invariant applied to the one renderer that
  had not got the memo.
- An **immutable** Bag/Set/Mix stopped rejecting a write through the alias
  (`roast/S02-types/{bag,mix}.t`: "Make sure we cannot assign on a .kv alias").
  The check that raises `X::Assignment::RO` there compared the parameter's value
  in `env` against the chunk element — and a raw bind puts the parameter in a
  local slot as a cell, so `env` had nothing to compare and the write looked
  like no write at all. The loop snapshots what the chunk holds before running
  the body now, which is the only thing left that can see an assignment made
  *through* the promoted cell. Note what this is not: the loop must not refuse
  to promote here, because the same bind is what makes the mutable `BagHash`
  case work.

## Found along the way, not fixed

A deferred *read-only* closure over a multi-parameter still snapshots by value,
so it does not see a later write to the element — while a closure that writes
reads the fresh value correctly. Pre-existing and multi-parameter-wide, filed as
`todo/tickets/multi-param-read-only-closure-capture-snapshots-the-element.md`.

Separately, `t/for-loop-element-alias.t`'s new multi-parameter row names its
parameters `$p`/`$q` rather than `$x`/`$y`: naming them `$x` makes the file's
`Proxy` rows fail, and so does a plain `my $x = 1` anywhere in it, on `main` too
— mutsu stores a `Proxy` assigned into an Array without FETCHing it and
compensates inside the loop. Filed as
`todo/tickets/proxy-assigned-into-an-array-is-not-fetched.md`.

With this, **every row of ADR-0045 §1.3 is green** and
`t/for-loop-element-alias.t` carries no `todo` at all. Only the ADR's slice 6
sweep remains.
