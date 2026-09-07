# ADR-0039 slice 2: `@`/`%` reads should compile to a slot — attempt 3 measured, seven defects found, one blocker left

**Design: [ADR-0039](../../docs/adr/0039-container-lexicals-resolve-lexically.md)
§4.2 first bullet; the measurements are §9, §10, §11 and now §12.**

A container read (`Expr::ArrayVar` / `Expr::HashVar`) still compiles to a by-name
`GetArrayVar` / `GetHashVar` where a scalar read compiles to `GetLocal(slot)`.
Container lexical scoping is therefore *dynamic*, not lexical — and the by-name
read is what **hides store-lane bugs**: two paths that end up naming different
containers under one name look fine as long as every read re-resolves the name.

Attempt 3 (2026-09-07) implemented the flip, fixed seven store-side defects it
exposed, reached a fully green `prove t/` (3783 files) **and a fully green local
`make roast`** (1436 files, 218962 tests) — and was then withdrawn by the
**bundled-library battery gate**, which is a required CI check that neither `t/`
nor roast covers. Everything below is measured on `fad0ac3ad`; treat it as the
starting state for attempt 4, not as an estimate.

## Why it is worth doing

Slice 1 (`unit_lexicals`) made the declaration side lexical; until the read side
follows, a whole class of "the slot and `env` disagree" defects stays invisible
rather than fixed, and every write site keeps paying the by-name lookup.
Attempt 3 is direct evidence: turning the read into a slot read surfaced
**26 `t/` files + 3 roast files + 2 battery files** of genuine, pre-existing
divergence from `raku`, every one of which the by-name read had been papering
over. Six of the seven repairs are real bug fixes that simply had no observable
symptom before.

## What attempt 3 measured (supersedes §10's numbers)

| what §10/§11 recorded | re-measured 2026-09-07 |
| --- | --- |
| §11: the write/capture blocker (nested-frame container mutation reaches its owner by NAME only) | **gone**, as §11 predicted; `t/nested-frame-container-mutation-reaches-owner.t` passes under the flip |
| §10.2 defect 1 — expression-position container declaration allocates no slot | still reproduces (`t/block-local-my-scope` 21/23, `t/feed-operators` 17/18, `t/push-inline-array-decl` 3/5) |
| §10.2 defect 2 — `try_fast_hash_element_assign` resolves its target by name | still reproduces (`t/var-decl-constraint-clear` 7) |
| §10.2 defect 3 — whole-container rebuilds replace the node | **gone** — shipped on its own as `store_container_preserving_identity` (§10.4) |
| §10.2 defect 4 — a QuantHash re-tag rebuilds its data node | still reproduces, **different root cause** (see fix 7) |
| §10.3 roast row `S32-list/classify.t` 25-27 | still reproduces; the recorded diagnosis ("a bug in fix 3's own probe") is **wrong** — see fix 5 |
| §10.3 roast row `S03-metaops/hyper.t` 18-19, 92-93 | still reproduces; §10.5 point 3's remedy (a baked `HyperMethodCall` target slot) is **not** needed — see fix 6 |
| — | NEW: `@_`/`%_` and twigil'd names — 11 `t/` files |
| — | NEW: the cross-thread lane — 9 `t/` files |
| — | NEW: `roast/S32-io/IO-Socket-Async.t` 37 |
| — | NEW (the blocker): `Cro::HTTP/router-auth.rakutest`, `zef/distribution-depends-parsing.rakutest` |

All 26 `t/` files were verified green on the same build with the flip reverted,
so every one was genuine flip fallout rather than pre-existing noise.

## The seven repairs (six still unlanded; re-derive them, do not re-measure)

1. **Gate the flip on `is_plain_user_lexical(sigiled)`.** The by-name read's tail
   is load-bearing for non-plain names: its `None` arm yields an empty container,
   which is how `%_` reads as `{}` on the fast method-dispatch path that
   deliberately leaves it unbound (`t/method-implicit-named-slurpy.t`), and its
   cascade is how `%!attr`/`%.attr` reach `self`'s attribute cell and how
   `@*dyn`, `%?RESOURCES` and `::`-qualified names resolve at all. Reading `%_`
   through a slot raised `Variable '%_' is not declared`. This one restriction
   closed 11 of the 26 files, and any future widening has to supply those
   behaviours first.
2. **`GetLocal` must not prefer the name-keyed store over a live cell.** Its
   `@`/`%` arms consult `__mutsu_atomic_arr::`/`__mutsu_atomic_hash::` and the
   bare-name shared store *before* reading the slot. The condition that worked
   is **"the slot's cell is still what this frame's `env` names"**, and both
   neighbours of it were measured wrong: "the slot holds any cell" breaks
   `roast/S32-io/IO-Socket-Async.t` 37 (a `supply`/tap body's shared block
   lexical is cell-boxed too, but the lane does *not* write through that cell,
   so a tap callback's `@got.append` lands in the lane and in `env` while the
   cell stays empty), and "the slot holds the *unit-lexical* cell" excludes the
   `my %h; await (^3).map: -> $i { start { %h{$i} = $i } }` case the gate exists
   for. Closed all 9 cross-thread files.
3. **An expression-position container declaration must take the slot the read
   resolves to** (`compile_expr_stmt`'s `Stmt::VarDecl` arm,
   `compiler/expr_block.rs`). `local_map` is monotonic in the default build, so a
   popped sibling block's `@a` slot stays reachable and
   `{ my @a = 5,7,9 } (my @a).push: $_ for ^3` stored into `env` alone.
   `declare_local` resolves get-or-create by name in the default build, so it
   reuses the very slot the read resolves to.
4. **`try_fast_hash_element_assign` must use its compiler-baked `target_slot`**
   (thread it in from `exec_index_assign_expr_named_op`, resolve with
   `resolve_local_slot`). `find_local_slot` is a `position` search, so with a
   same-named shadow (`code.locals == ["%h", "%h"]`) it nil'd and re-seeded the
   OUTER binding.
5. **`store_container_preserving_identity` needs Set/Bag/Mix arms.**
   `classify(..., :into(my %b := BagHash.new))` rebuilds the bag, which fell
   through to a plain `env.insert` of the fresh node. **This one is
   independently observable and has landed** (see
   `t/container-rebuild-preserves-identity.t`'s QuantHash rows).
6. **`write_back_hyper_target_var` must write THROUGH the container node.** Its
   fallback did `set_env_with_main_alias` + `locals_set_by_name`, and the latter
   is a `position` search, so under a same-named shadow (`my @r;` at file scope
   plus a block's own `my @r = (1,2,3)`) it wrote the OUTER slot: `@r»++`
   answered `1 2 3`. Mutating the existing node reaches every holder and needs no
   slot search — which is why §10.5 point 3's baked `HyperMethodCall` slot is
   unnecessary.
7. **The `is BagHash`/`SetHash`/`MixHash` trait handler must re-sync its slot
   after registering the name-keyed constraint.** That registration re-tags
   `env`'s value via `register_var_container_type_metadata` →
   `tag_container_metadata` → `Gc::make_mut`, which COPIES a shared node — and
   the node was just stored into this frame's slot. Read `env` back and re-store
   it into the slot afterwards. Two neighbouring repairs were measured **wrong**:
   making `register_var_container_type_metadata` itself store
   identity-preservingly breaks `t/typed-bind-recursion.t` (a call frame's
   flattened env carries the caller's same-named entry, so a recursive
   `my @ret := Array[T].new` retags the CALLER's container — restricting it to
   the frame's own overlay does not help), and moving the registration to run
   *before* the trait's own stores breaks `t/quanthash-hyper-funcop.t`,
   `t/pairs-element-container.t` and `t/quanthash-declared-init-identity.t`.

## The blocker: a `gather` body's container append reaches the lane, not the owner's cell

With all seven applied, `prove t/` and a full local `make roast` are green, and
the **battery gate** fails on two files:

- `Cro::HTTP/router-auth.rakutest` (24 assertions)
- `zef/distribution-depends-parsing.rakutest` (1 assertion, test 21)

The zef one was bisected to a **single container name**, `@prereq-candidates` in
`Zef::Client!find-prereq-candidates`, using a temporary compiler harness worth
rebuilding: an env var that restricts the flip to a comma-separated name list
(`MUTSU_SLOT_READ_FILTER`) plus one that dumps every name the flip would apply to
(`MUTSU_SLOT_READ_DUMP`), then a delta-debugging script over the ~92 names. That
turns "a 3000-line vendored library misbehaves" into "one name" in a few minutes;
**do this first next time**, before trying to reduce the source by hand.

What the traces then showed, in the frame that fails:

- the owner's slot and its `env` entry hold the **same** `ContainerRef` cell,
  and that cell's array is **empty**;
- `get_env_with_main_alias(name)` nevertheless answers a **1-element** array,
  because `get_env_with_main_alias_inner`'s `__mutsu_atomic_arr::` probe wins
  over `env`;
- so the append (`@prereq-candidates.append(@candidates)`, performed from inside
  a nested `gather for ... { ... }` block) landed in the **atomic lane** and
  never reached the cell the owner's slot holds.

The `%` twin does not have this problem: `shared_hash_elem_set` writes the merged
hash through `unit_lexical_container_cell` / the env cell **unconditionally**,
while `shared_array_mutate` does so only on its non-`is_thread_clone` branch. A
first attempt at the obvious repair — capture the binding cell before
`shared_array_mutate`'s thread-clone `env.remove` and write through it
unconditionally — did **not** fix it, so the appending frame's `env` apparently
does not hold the cell either; the next step is to trace which env tier the
`gather` body runs against.

Note the shape: this is §8.4 point 2's territory ("once `Expr::ArrayVar` emits
`GetLocal(slot)`, the store-writeback path can no longer reach the reader"), but
narrower than that point predicted — the lane still works, it is the *cell* that
goes stale, and only for `@` (not `%`) and only when the mutation happens in a
`gather`-driven frame.

**Beware the Heisenbug.** Adding a `note "..." ~ @prereq-candidates.elems` *inside*
the appending closure makes the whole file pass: the extra read changes the
closure's free-variable set and therefore its capture strategy. Instrument only
outside closures, or instrument the VM instead.

## What NOT to do

- Do not reach for decl-site cell-boxing of every `@`/`%`.
  `docs/captured-outer-cell-sharing.md` §7.1d records that it regressed ~12 files
  through decont leaks, which is why `register_container_ref_capture_if_free` is
  restricted to plain `$` names.
- Do not lift the `is <Type>`-trait exclusion from
  `CompiledCode::needs_cell_unvouched_containers`. It was measured as a working
  fix for the `%h is MixHash` capture, but repair 7 subsumes it at a fraction of
  the blast radius.
- Do not trust `t/` + roast alone for this change. Both were fully green with the
  flip on; the battery gate (`scripts/battery-testsuite.sh`, `MUTSU_BIN=…`) is the
  only local gate that caught the blocker. Run it — it takes ~10 minutes.

## Acceptance

- `prove t/` green with the flip on (attempt 3 reached this).
- A full local `make roast` green (attempt 3 reached this).
- **`MUTSU_BIN=target/release/mutsu scripts/battery-testsuite.sh` green** — the
  gate attempt 3 did not reach.
- `t/nested-frame-container-mutation-reaches-owner.t` and
  `t/container-lexical-declarator-matrix.t` keep passing.
