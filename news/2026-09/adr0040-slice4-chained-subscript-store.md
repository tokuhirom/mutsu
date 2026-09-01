# ADR-0040 slice 4 — a chained subscript stores its element bare

Slice 4 of ADR-0040 was scoped as "delete the two compensators, now that slices
1-2 make them redundant". They were not redundant, and the reason turned out to
be a bug rather than a leftover.

## What the instrumentation found

The way to check "is this mechanism still doing anything?" is to instrument the
site and run the whole corpus, not to re-read the design's divergence table.
Doing that here turned up 22 live firings of the render-side compensator across
`t/`, every one of them a real defect that ADR-0040's own 25-row matrix could
not see — because the matrix only ever uses **one** subscript level.

Slice 1's implementation note had already flagged the shape of it: "deeper
(3+-level) chained assignment was not separately audited". It was not covered.
Neither was the **leaf** of a two-level chain, nor the intermediate container a
**deferred vivification token** walk-creates. Measured against raku:

| program | raku | mutsu (before) |
| --- | --- | --- |
| `my %h; %h<a><b> = [1,2]; %h<a><b>.raku` | `$[1, 2]` | `[1, 2]` |
| `takes(%h<a><b>)` | `1` | `2` |
| `my %d; %d<a><b>[2] = "z"; %d<a><b>.raku` | `$[Any, Any, "z"]` | `[Any, Any, "z"]` |
| `my @g; @g[0][1][2] = 7; takes(@g[0][1])` | `1` | `3` |
| `my %h; my $r := %h<a>[1]; $r = "x"; %h<a>.raku` | `$[Any, "x"]` | `[Any, "x"]` |

The render-side compensator made `%h.raku` look right while `%h<a><b>.raku`,
`.VAR` and list-context arity were all wrong — ADR-0040 §1.5's "one value,
three answers" shape, still alive one level down.

## A note on instrumenting the right thing

A first pass counted 57 firings and looked like proof the compensator was
load-bearing everywhere. It was not. `raku_hash_value` has two callers with
opposite roles: the three Hash/Map rendering sites (the actual compensator) and
the `ValueView::Scalar` arm, which is the *primary* `$(…)` renderer for every
itemized value. Probing only the compensator's own call sites dropped the count
to 22 — and made all 22 real.

## The fix

Four sites, all of one shape — a value entering a parent's element slot:

- the leaf of a two-level chained assign (`exec_index_assign_expr_nested_op`),
- the leaf of a 3+-level one, plus its four intermediate-vivification sites,
  collapsed onto a new `fresh_autoviv_container` helper
  (`exec_index_assign_deep_nested_op`),
- the container a deferred vivification token walk-creates (`fresh_level_for`,
  `src/value/entry_path.rs`),
- an itemized-`Hash` arm for `.VAR`, which the existing itemized-`Array` arm did
  not cover because a `Hash` carries its itemization as a bool on the repr
  rather than as an `ArrayKind`.

Itemizing an `Array` only flips its kind tag, so the `&mut` the walk takes into
the slot to keep descending is unaffected.

`:=` through a **two-level** chain now matches raku as well: the element renders
`$[1, 2]`, the source read directly stays bare, and a later push still shows
through. A **3+-level** bind installs a shared `ContainerRef` cell rather than a
value, and wrapping *that* in a `Scalar` (slice 1's `@a.push(@b)` shape) breaks
the write path, which does not yet see through a `Scalar`-wrapped cell. Left
bare deliberately; the write-through is pinned instead.

## The compensators: measured, not deleted

Both sites were instrumented and the entire corpus run — `t/` (3601 files) and
the full roast whitelist (1425 files):

| compensator | before | after | what still reaches it |
| --- | --- | --- | --- |
| render-side (`raku_hash_value`) | 22 in `t/` | **0 in `t/`**, 1 in roast | a self-referential hash, where the value rendered is the cycle sentinel |
| read-side (`itemize_hash_value`) | 3 in `t/` | 3 in `t/`, 17 in roast | **natively constructed hashes only** — Pod block `.config` (12 of 17), `gethost(…)<addrs>`, two exception/`Proc` hashes |

So the deletion is blocked on one nameable class, and it is the same class slice
2 already hit once: a `Hash` a native Rust builtin constructs directly bypasses
every store hook. Slice 2 fixed the JSON decoder by hand for exactly this
reason; Pod `.config`, `gethost` and their siblings are the rest of it. That is
the remaining slice-4 work, and it is a store-site enumeration rather than a
mechanism question.
