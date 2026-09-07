# A named sub declared in a bare block resolves its free variables lexically

ADR-0024 gave a *mainline* named sub a lexical answer for the free variables it
captures: a shared cell per name, parked in `unit_lexicals[MAINLINE_UNIT_KEY]`
and consulted before the flat, name-keyed `env` whose key belongs to whoever is
calling. A sub declared inside a **bare block** had no such store, so every one
of its free variables fell back to the caller's env — and a caller that
`my`-declares the same name owns that key.

```raku
{
    my $bv = 1;
    sub gb() { $bv = 3 }
    sub fb() { my $bv = 5; gb(); $bv }
    say fb();   # raku: 5   mutsu (before): 3
    say $bv;    # raku: 3   mutsu (before): 1
}
```

Two independent variables, fused: `gb`'s write clobbered `fb`'s shadow and never
reached the binding it actually names.

## Every recorded premise was re-measured, and four were wrong

The finding was filed as the residue of
`news/2026-09/free-var-bind-aliased-caller-lexical.md`, which had fixed the
file-scope half. Its own root-cause section was re-measured against `raku
v2026.07` and a fresh build before anything was designed. What it said, and what
the measurement showed:

1. **"Free-variable reads and writes were never the problem; the divergence is
   confined to a `:=` bind."** False in a bare block. A `$` *write* diverges (row
   B). An `@`/`%` *read* diverges (row J). Even a `$` read diverges once the
   shadowing caller is a **block** rather than a routine (row M1) — the scalar
   read happened to survive a *routine* shadow, which is the one shape the
   earlier investigation probed.
2. **"Only the last-statement (`SetGlobal`) bind spelling is known to diverge in
   the block surface."** False: the `bind-then-statement` (`SetLocal`) spelling
   diverges identically on the caller's own read (row D1). The file's evidence
   for the claim was that the *alias* still tracked a later write — a different
   assertion, which was indeed already right.
3. **"`exec_register_sub_op` gates the ADR-0024 capture on
   `block_scope_depth() == 0`, so a bare-block sub is excluded."** Right outcome,
   wrong gate. A bare `{ ... }` at file scope leaves `block_scope_depth` at **0**
   and pushes a *block routine frame* instead; the condition that actually
   excluded the sub was the mainline arm's `routine_stack().is_empty()`. Designing
   against the depth counter produced a no-op patch (boxes = 0 under
   `MUTSU_VM_STATS`) before a `rust-gdb` breakpoint on the registration site read
   the real value.
4. **"Closing this IS the env-model change ADR-0055 §7.5 names — a routine's env
   parent should be its lexical scope, not its caller — and needs its own ADR."**
   Not needed. The file's own "smaller step first" paragraph — extend the
   ADR-0024 capture to block-scope named subs with a per-block bucket — turned
   out to cover the entire measured surface, including the `@`/`%` sigils and
   both bind spellings, with no change to how envs are parented.

The `news/2026-09/deferred-map-callback-frame.md` analogy (the same class of bug
in `eval_map_over_items` was a *merge priority*, not a missing capture) did not
apply either: none of the three closure-env merges ADR-0055 §7.6 counts is on
this path. There was no capture to prioritise — there was no store.

## The fix

`mainline_lexical_subs` becomes a map from a sub's name to the `unit_lexicals`
bucket holding *its* cells. Mainline subs all map to `MAINLINE_UNIT_KEY`, because
mainline is one scope; a block-declared sub gets its own
`UNIT<block ...>` bucket, because sibling blocks are distinct scopes that may
each declare the same name and a shared bucket would fuse them (row P). One new
accessor, `active_unit_lexical_bucket()`, generalises
`mainline_lexical_frame_active()` — same last-frame-only discipline, it just
answers *which* bucket — and the read resolver (`unit_lexical_slot`), the write
resolver (`unit_lexical_slot_mut`, hence `unit_scope_lexical_write` and
`unit_scope_lexical_bind`), the writeback suppressor
(`is_mainline_lexical_write`), the closure-capture injection
(`inject_mainline_lexical_captures`) and the light-call exclusion all read it.

Registration gains a second arm: the same gates as mainline (not in EVAL,
package `GLOBAL`, no module load, not a thread clone, exactly one live candidate
slot for the name) but with `routine_stack().iter().all(|f| f.is_block)` in place
of `routine_stack().is_empty()`, so a sub registered while another *routine* runs
is still excluded.

One extra site needed the same generalisation, and it is the one the finding did
not name. `mainline_lexical_cell` — the frame-unconditional lookup both `:=`
handlers use to *reuse* the source's existing cell rather than mint a
disconnected one — probed `MAINLINE_UNIT_KEY` only. With the store populated but
that lookup blind to it, the bind stopped hijacking the caller (row C1 correct)
but bound the alias to a fresh cell, so a later write to the block lexical no
longer reached it (row C2 regressed from a coincidentally-right `9` to `1`). It
now tries the **running routine's own** bucket first — frame-gated, which is
required precisely because sibling block buckets must not be scanned blind — and
falls back to the unconditional mainline probe the cross-thread and `is rw`
callers depend on.

## Which slot the sub captured turned out to be the hard part

Getting the *store* right was the easy half. Two soundness holes in **which
`code.locals` slot** the capture boxes only surfaced under a targeted roast sweep
over the closure/scoping synopses — `make test` covers neither.

**The slot search was a heuristic that only holds at mainline.** ADR-0024 picked
the free variable's slot by LIVENESS: "at the moment this `RegisterSub` runs, a
shadowing block has either not run yet (still `Nil`) or is unrelated, so only the
slot that is genuinely initialized right now can be the binding visible at this
declaration point". True at mainline. False inside a bare block, where earlier
SIBLING blocks have already run and left their own same-named slots live — it
silently captured a foreign block's `$a`.

Replaced by the bake closures already get. The compiler resolves each free
variable against `local_map` at the sub declaration's own textual emit point and
stores it in the plan as `CompiledSubDeclPlan::free_var_decl_slots` — the
named-sub counterpart of `CompiledCode::free_var_parent_slots`, which
`add_closure_code_baked` bakes for a closure and which is never populated for a
plan-derived named sub. The hoisted plan gets the same bake through the handover
that already passes it `compiled_routine_keys`. The liveness search survives only
as the mainline arm's fallback for a plan with no compiled bodies.

**Sibling block scopes deliberately SHARE a slot.** `declare_local` mints a fresh
slot only for a genuine ANCESTOR shadow; a name left in the monotonic `local_map`
by an already-popped sibling reuses that sibling's slot on purpose, "because
nothing observes both at once through the slot". ADR-0024's cell does: boxing
such a slot fuses two independent bindings, and the cell outlives the first
block. Those slots are now recorded at declaration time in
`CompiledCode::multi_scope_slots` and the block arm declines them — the name
keeps legacy dynamic resolution rather than being fused. A genuine inner shadow
gets its own slot and is unaffected, which is why the nested-block rows still
work.

An `our sub` is excluded from the block arm outright: it is installed in the
package registry and outlives its block, its captured block lexicals already have
a store (`escaped_our_lexical_cells`), and its hoisted registration runs before
any of the compunit's blocks — so a second store mirrored its cell into the
shared `env` key ahead of everything else.

The shape that found all three is
`roast/S02-names-vars/variables-and-packages.t`'s "initilization from BEGIN
block": three sibling blocks each declaring `my $a` and a sub over it, each sub
called *before* its declaration statement runs, the third initialized from a
`BEGIN` block, followed by an `our sub` block. It is pinned as rows U1-U6.

## Control table

`raku v2026.07` vs `mutsu`, before and after. Rows E/K/L are file-scope controls
that must not move; row I is the dynamic-variable control that must KEEP caller
priority.

| row | shape | before | after | raku |
| --- | --- | --- | --- | --- |
| A | block `$` read, routine shadower | 1 | 1 | 1 |
| B1/B2 | block `$` **write**, routine shadower | **3 / 1** | 5 / 3 | 5 / 3 |
| C1/C2/C3 | block `$` `:=`, last-statement spelling | **1** / 9 / 9 | 5 / 9 / 9 | 5 / 9 / 9 |
| D1/D2/D3 | block `$` `:=`, bind-then-statement spelling | **1** / 7 / 7 | 5 / 7 / 7 | 5 / 7 / 7 |
| E1/E2/E3 | FILE-scope `$` `:=` (control) | 5 / 9 / 9 | 5 / 9 / 9 | 5 / 9 / 9 |
| F1/F2/F3 | block `$` `:=`, **block** shadower | 5 / **5** / 9 | 5 / 9 / 9 | 5 / 9 / 9 |
| G1/G2/G3 | block `@` `:=` | 2 / **2** / 3 | 2 / 3 / 3 | 2 / 3 / 3 |
| H1/H2/H3 | block `%` `:=` | 2 / **2** / 1 | 2 / 1 / 1 | 2 / 1 / 1 |
| I1/I2 | `$*dyn` — must keep caller priority | caller / outer | caller / outer | caller / outer |
| J1/J2 | block `@` / `%` **read** | **2 / 1** | 3 / 3 | 3 / 3 |
| K1/K2 | FILE-scope `@` / `%` read (control) | 3 / 3 | 3 / 3 | 3 / 3 |
| L1/L2 | FILE-scope `$` write (control) | 5 / 3 | 5 / 3 | 5 / 3 |
| M1/M2/M3 | block `$` read+write, **block** shadower | **5 / 3 / 1** | 1 / 5 / 3 | 1 / 5 / 3 |
| N1/N2 | block `$` liveness (capture is a cell) | 1 / 2 | 1 / 2 | 1 / 2 |
| O1 | closure made in the shadow block wins | inner | inner | inner |
| P1/P2 | two sibling blocks, same name | one / two | one / two | one / two |
| Q1/Q2 | nested blocks, depth 2 | 3 / 21 | 3 / 21 | 3 / 21 |
| R1 | sub declared inside a **routine** | 1 | 1 | 1 |
| S1/S2 | closure made INSIDE the block sub | inner / outer | inner / outer | inner / outer |
| T1 | `for` body declaring a sub per iteration | 10,20 | 10,20 | 10,20 |
| U1-U6 | three sibling blocks, same name, sub called before the declaration | 0,1 / 0,1 / 3,4 | same | same |

12 of the 49 assertions failed before the change; all 49 pass now,
byte-identical under `mutsu` and `raku`. Pin:
`t/free-var-in-bare-block-lexical-scope.t`.

## What is left

ADR-0024's remaining declaration-scope limitation is a sub declared inside
another **routine** (`sub outer { sub inner { $x } }`): the registration arm
still refuses it, because the enclosing frame's locals are not the frame
`RegisterSub` runs in the way a block's are. Row R measures it as agreeing with
raku today, so it is a latent gap rather than an open divergence.

The block arm also declines, by design, any name whose slot two sibling scopes
share (`multi_scope_slots`) and any `our sub`'s captures. Both keep legacy
dynamic behaviour rather than guessing; no partial state is written either way.
Making the sibling-shared case work too would need the slot allocator to stop
reusing a popped sibling's slot, which is a §1.4/§1.5 question, not this one.
