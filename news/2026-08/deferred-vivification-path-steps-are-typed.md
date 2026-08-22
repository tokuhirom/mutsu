# The deferred vivification path knows a positional step from an associative one

A subscript chain that reaches a not-yet-existent hash key does not create
anything: it hands out a deferred `HashEntryRef` token — a container root plus
the path walked from it — and the first *write* walk-creates that path. That is
what lets a path-addressing routine be used as a getter without vivifying what
it looks up, and it is the mechanism [ADR-0059](../../docs/adr/0059-is-rw-routines-return-a-container.md)
connected to `is rw` lvalue returns.

The path was a `Vec<String>`, so it could only describe *associative* descent. A
positional step was stringified into a hash key, and the container the write
created was always a `Hash`:

```raku
my %h;
my $x := %h<g>;
$x[0] = 'x';
say %h.raku;   # raku: {:g($["x"])} — mutsu vivified a Hash keyed "0", or nothing at all
```

The path steps are now typed (`EntryStep::Key` / `EntryStep::Index`, carried by
a new `is_positional` field on `OpCode::IndexAutovivifyLazy` and its `Terminal`
twin), and the walk-create builds the container the **next** step asks for — an
`Array` for an index, a `Hash` for a key. All four mixed shapes now match raku
exactly: `%h<g>[0]`, `%h<g>[0]<k>`, `%h<g><a>[1]`, `%h<a><b>[2][0]`.

Rather than grow a parallel walker, the write and read halves converge on the
existing element chokepoints. `hash_entry_terminal` now returns an
`EntryTerminal` — a located `(hash, key)` or `(array, index)` slot — whose
`insert` goes through `Value::hash_insert_through` / `Value::assign_element_slot`
and fills an array gap with the element type object exactly as `array_slot_ref`
does. The five call sites that previously destructured the terminal tuple and
hand-rolled a map insert now just call `insert`, and `=:=`'s hand-rolled
read-only walk (`extract_hash_ref`) is replaced by the shared
`hash_entry_locate`.

Three further gaps in the same mechanism surfaced and are fixed with it:

- **An element assignment made *through* a bound token wrote nowhere.**
  `my $x := %h<g>; $x[0] = 'x'` resolved the token to `Any` and assigned into
  it, so `%h` stayed `{}` — the write was silently lost for the associative
  shape too. It now extends the token's path by the whole subscript chain,
  writes through it, and promotes the binding to the shared cell installed at
  the token's slot. Wired into all three index-assign opcodes
  (`IndexAssignExprNamed`, `IndexAssignExprNested`, `IndexAssignDeepNested`), so
  it covers `$x[0]<k>` and deeper.

- **The second write through a just-materialized binding detached from the
  hash.** A `:=` bind registers a sigilless alias
  (`__mutsu_sigilless_alias::$x` → `__mutsu_bind_index_ref_N`) that the
  env-centric element-assign handlers redirect through *before* they look at
  anything else, and `materialize_bound_slot_to_cell` never updated that alias
  target. So `$x = ['a']; $x[1] = 'b'` found no container behind the alias,
  autovivified a fresh one, and left `%h<g>` at `["a"]`. It now points the alias
  at the cell. (This one predates the positional work: it reproduced on `main`
  for a purely associative `$x<a> = 1; $x<b> = 2`.)

- **An already-materialized but still empty cell could not anchor a chain.**
  `array_slot_ref` grows past the end and promotes the fresh hole to a
  `ContainerRef` cell, so a chain that steps positionally into a real array and
  then associatively (`Crane::In`'s `in(container[@steps[0]], @steps[1..*])`)
  arrived at a cell holding `Any`, which `GetLocalDeferred` dereferenced away.
  The token root is now an `EntryRoot` — a hash *or* such a cell — and in
  container mode an *empty* cell is handed to the subscript whole, so
  `my @a; my $x := @a[0]; my $y := $x<k>; $y = 5` yields `[{:k(5)},]` like raku.
  A cell promoted from a real scalar leaf is deliberately excluded: rakudo
  refuses to assign through it, so it is left alone rather than clobbered.

Pinned by `t/deferred-bind-positional-step.t` (28 subtests, byte-identical
output under `raku`).

## Effect on the TOML battery

`Crane` — the sole dependency of `Config::TOML`, the selected TOML battery — has
a full set of `Positional` candidates built on `return-rw container[@steps[0]]`,
and this is what made them unusable. All of `Crane.set`'s mixed-path shapes now
match raku:

```raku
use Crane;
my %g; Crane.set(%g, :path["a", 0], :value(1));      say %g.raku;  # {:a($[1])}
my @b; Crane.set(@b, :path[0, "k"], :value(1));      say @b.raku;  # [{:k(1)},]
my %c; Crane.set(%c, :path["a", 0, "k"], :value(1)); say %c.raku;  # {:a($[{:k(1)},])}
```

Measured on a release build, running each suite from its own dist directory:
`Crane` 0.1.2 moves from **280 ok / 176 not-ok to 283 / 173**, with
`t/in.rakutest` going 9 → 12 passing. File granularity is unchanged at 3/15
(raku: 15/15), because the files that still fail do so on separate features:
`Crane.add`/`.copy`/`.move`/`.remove`/`.replace` mutate the caller's container
instead of the deep-cloned copy they return (~120 subtests, filed as
`todo/tickets/crane-add-mutates-the-original-container.md` — that is now the
largest single block), plus `X::Crane::PositionalIndexInvalid` and
`WhateverCode` (`*-0`) indices, both recorded in `docs/batteries/toml.md`'s
work list.

`Config::TOML` 0.1.3 is unmoved at 132 ok / 479 not-ok, 0/19 files: its own
remaining failures are grammar/regex-level, downstream of a `Crane` that is not
yet whole.
