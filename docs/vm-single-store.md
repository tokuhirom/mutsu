# Single-authority store redesign (`locals` as truth, `env` as a derived view)

> **Status:** DESIGN (2026-06-17). No code yet. This document reopens the
> dual-store question from the design level, as a *single authoritative store*
> redesign, per the user directive ("単一権威ストア再設計として設計から問い直す").
> It supersedes the **footprint-reduction-only** framing of
> [docs/vm-dual-store.md](vm-dual-store.md) — read that file first for the full
> mechanism map, the 151 `env_dirty` setters, and the slice history. This file is
> the *forward plan*; vm-dual-store.md is the *archive of what was tried*.

---

## 0. TL;DR — the thesis that reopens "deletion is impossible"

`docs/vm-dual-store.md` (CP-2 "READ FIRST") concluded that the dual-store
machinery (`env_dirty` + `sync_locals_from_env` + `ensure_locals_synced` +
`saved_env_dirty`) **cannot be deleted** while slot-indexed `locals` and
arbitrary-by-name carriers (above all `EVAL`) coexist. That conclusion is
**correct for the goal it was aiming at** — "delete the reverse-sync primitive
outright". This redesign changes the goal, not the constraint:

> **We do not delete the reverse sync. We *relocate* it from a coarse,
> per-`GetLocal`, whole-`locals` scan (gated by a global `env_dirty` bit) to a
> *precise, per-carrier-boundary writeback of exactly the names that carrier
> wrote*.**

After that relocation:

- The **hot path** (`GetLocal`/`SetLocal`) is a pure slot read/write with **zero
  sync logic** — no `ensure_locals_synced`, no `env_dirty` check.
- `env` becomes a **derived name-view**, kept coherent by *write-through in both
  directions*: forward (locals→env) already exists (`flush_local_to_env`, Slice
  6.2); reverse (env→locals) is added as targeted write-through at the few
  by-name write sites, plus a logged-name writeback at the carrier-return
  boundary for the one writer that cannot know slots in advance (`EVAL`).
- `locals` is the **single authority** for ordinary lexicals. `env` holds only
  what a name-keyed consumer needs, and is never the truth for a slotted name.

The global `env_dirty` flag, the `sync_locals_from_env` O(locals) scan, the
`ensure_locals_synced` call at every `GetLocal`, and the `saved_env_dirty` frame
field are then **deletable** — not because the reverse sync vanished, but
because it became a handful of O(written-names) writebacks at known sites.

This is the same shape that already works for two narrower carriers:
- **regex `{}` blocks** log caller-local writes in
  `Interpreter::pending_local_updates`; the smartmatch op drains them per match
  (vm-dual-store.md Slice 6.3, `t/smartmatch-env-dirty.t`).
- **compiled function/method calls** report a *precise* changed-flag from their
  return merge (`merge_method_env`, `call_compiled_function_*`), so the blanket
  post-dispatch `env_dirty = true` was already replaced by a precise signal for
  the dispatch tiers (Slice 6.1 / Stage 1).

The redesign generalizes that pattern to **every** remaining by-name writer, and
then removes the now-unreachable coarse machinery.

---

## 1. The stores today (recap — see vm-dual-store.md §"The two stores today")

| store | shape | role |
|---|---|---|
| `Interpreter::locals` | `Vec<Value>`, slot-indexed | hot path, `GetLocal`/`SetLocal` |
| `Interpreter::env` | `Env` = `Arc<HashMap<…>>` (COW, scoped overlay) | everything name-keyed |

Sync primitives (`src/vm/vm_env_helpers.rs`):

- `flush_local_to_env(code, idx)` — **forward write-through**, immediate, gated
  on `code.needs_env_sync[idx]` (a name-based reader can name the slot) and
  `simple_locals[idx] || is_bare_param`. Already replaces the lazy forward flush.
- `ensure_locals_synced(code)` — **reverse pull**, gated on `env_dirty`; calls
  `sync_locals_from_env`. Invoked before every local-aliasing read (≈ every
  `GetLocal`).
- `sync_locals_from_env(code)` — O(`code.locals`) scan, env→locals, the pull body.
- `env_dirty` / `saved_env_dirty` — the global "a by-name writer touched env
  since the last pull" bit, snapshotted per call frame.

The forward direction is already write-through. **This redesign is about making
the reverse direction write-through too**, at which point the flag + scan die.

---

## 2. The reverse-sync writers, and how each becomes precise

Every site that sets `env_dirty = true` (151 today, grouped in vm-dual-store.md
§"Setter sites") is a *by-name env writer whose write the slot store did not
see*. Each must be converted to one of three precise forms. The grouping below
is the migration unit list.

### Class R1 — by-name writers with a knowable slot → **reverse write-through**

These write `env` by a name that, *if it is a current-frame lexical*, has a slot
in `code.locals`. The fix mirrors `flush_local_to_env` in reverse: at the write
site, if `find_local_slot(code, name)` hits, also set `self.locals[slot]`.

Members (from vm-dual-store.md step-2 survey):
- our-vars / package vars (`vm_register_ops`, `SetGlobal` of `our $x`),
- dynamic vars `$*x` write-through (`vm_var_assign_ops`),
- `&sub` / type-name registration (`vm_register_ops`, 17),
- symbolic deref / by-name var ops (`vm_var_get_ops`, `vm_data_ops`),
- loop/control round-trips (`vm_control_ops`, `vm_misc_ops` let/temp).

The natural home is **`set_env_with_main_alias`** itself (the single by-name env
writer, vm_env_helpers.rs:245): give it access to the current `code` + a
"reverse write-through" toggle so any by-name write to a slotted name updates the
slot in the same call. Most R1 sites then stop setting `env_dirty` for free.

> **Subtlety:** `set_env_with_main_alias` has no `code` today (it predates the
> slot store coupling). Threading `code` to it, or routing R1 sites through a new
> `set_env_through_slot(code, name, val)` wrapper, is the mechanical core of this
> class. The wrapper writes env (existing behavior) *and* `locals_set_by_name`.

### Class R2 — carrier writers that cannot know slots in advance → **logged-name writeback at the return boundary**

The carriers run *foreign* compiled/interpreted code that may assign **any**
caller lexical by name:
- **`EVAL`** — compiles+runs a sub-program; spec-mandated arbitrary caller-lexical
  writer (vm-dual-store.md's decisive blocker). `vm_call_exec_ops` (~9 marks).
- **regex `{}` embedded blocks** — already precise via `pending_local_updates`
  (the prototype for this whole class). Keep.
- **subset `where {…}` / signature constraint blocks** — run interpreter code
  that can touch caller lexicals.
- **interpreter method/function fallbacks** — the remaining `call_function` /
  `call_method_with_values` bridges that read/write env by name.

The fix generalizes `pending_local_updates`: each carrier **logs the set of
caller-lexical names it wrote** (the carrier already goes through
`set_env_with_main_alias`, so the log is a side-channel `HashSet<Symbol>` filled
there while a "carrier active" flag is set). At the carrier's **return
boundary**, write just those logged names back into the caller's slots
(`locals_set_by_name`) and clear the log. No global flag, no whole-`locals` scan,
no per-`GetLocal` cost.

> This is the crux that answers CP-2: `EVAL` keeps its by-name power, but its
> effect on the caller is reconciled **once, at EVAL return, for only the names it
> touched** — not via a global bit that forces a scan at the caller's next read.

### Class R3 — conservative marks that are already redundant → **delete**

The ~92 post-dispatch blanket `env_dirty = true` after compiled calls
(`vm_call_method_ops` 17/32, `vm_call_func_ops` 11/13, `vm_call_method_mut_ops`)
are *already* shadowed by the precise changed-flag from `merge_method_env` /
`call_compiled_function_*` (Slice 6.1 / Stage 1). Most are pure leftovers; delete
each once its dispatch tier is confirmed to report precisely (the work is mostly
done — finish the audit and remove the blankets).

### Special names — keep authoritative cells (no per-call sync)

`:=` bind aliases, `atomicint`/atomic arrays, cross-thread `shared_vars`, and
attributes (`!attr`) intentionally treat a **shared cell** (or `shared_vars`) as
truth, *not* `env` and *not* `locals` (vm-dual-store.md §"env consumers" 5–8;
`sync_locals_from_env` already skips `HashSlotRef` and `!attr` slots). These need
no change: a cell read is already authoritative. The single-store invariant
explicitly exempts them — they are "neither store is truth; the cell is".

---

## 3. Closure upvalues (prerequisite #1) — why it must come with R2

Closures capture **`self.env.clone()` (the whole outer env)** at
`MakeBlockClosure`, and `compute_needs_env_sync` (opcode.rs:1385) conservatively
forces whole-frame env writeback whenever any closure exists in a body. This is
the structural reason `env` cannot become a pure derived view: a closure is a
by-name reader of the parent env that outlives the frame.

**Target:** a closure captures **indexed upvalue cells** for exactly its free
variables (`CompiledCode::free_var_syms` already computes the set — Slice 3), not
a parent-env clone. A free variable shared between parent and closure becomes a
`ContainerRef`-style cell both sides hold by index. Then:
- the closure reads/writes upvalues by slot, never the parent env;
- `compute_needs_env_sync`'s whole-frame writeback is removed;
- the per-call O(captured-env) loops (vm-dual-store.md Slice 3) vanish entirely
  rather than being narrowed.

This is independently valuable (the `bench-array`/`array-ops`
`env_deep_copies` are per-iteration closure overlay copies — vm-dual-store.md
resume-map "NOT lever B"). It is sequenced **after** R1/R2 prove the
write-through model on non-closure carriers, because upvalue cells are the
highest-blast-radius change and benefit from the invariant guard (Slice A) being
in place first.

---

## 4. Migration slices (ordered, each independently shippable)

Each slice keeps `make test` + the relevant roast subset green locally and relies
on CI's release roast as the comprehensive net. Counters are opt-level
independent — iterate on the **debug** build (`MUTSU_VM_STATS=1`), reserve release
for wall-clock.

- **Slice A — measurement (no behavior change). DONE (2026-06-17).**
  `sync_locals_from_env` now records, under `MUTSU_VM_STATS`, how many of the
  slots a pull overwrites were *genuinely stale* (env differed from local, via
  the same `cheaply_unchanged` test `merge_method_env` uses) vs already coherent,
  plus a per-name histogram of the stale lexicals. New summary line:
  `reverse-sync: locals_pulls=N effective=E spurious=S stale_slots=T` and
  `stale-slot by name (top …)`. The comparison short-circuits when stats are off
  (zero release cost; the `.cloned()` was already unconditional).

  **Measured (debug, `MUTSU_VM_STATS=1`, 2026-06-17 — refreshes the stale
  2026-06-07 survey):**

  | bench        | locals_pulls | effective | spurious | stale slots (names) |
  |--------------|-------------:|----------:|---------:|---------------------|
  | method-call  | 1 | 0 | 1 | — |
  | bench-class  | 2 | 1 | 1 | `desc` |
  | fib          | 0 | 0 | 0 | — |
  | bench-string | 3 | 0 | 3 | — |
  | bench-array  | 4 | 1 | 3 | `@arr` |
  | array-ops    | 101 | 100 | 1 | `@data`×100 |

  A carrier probe (`tmp/slicea_probe.raku`: EVAL writing `$x`, dynamic `$*dyn`,
  `our $counter`, a closure pushing `@acc`) shows
  `locals_pulls=104 effective=53 stale-slot: adder=51 @acc=50 *dyn=1 x=1`.

  **Findings that steer the slices:**
  1. On the hot benchmarks the reverse sync is *already* mostly **spurious** — the
     pull runs but no slotted lexical was stale. Those vanish for free when
     `env_dirty` is removed (Slice F); they need no per-writer work.
  2. The **effective** pulls cluster exactly on the design's named classes:
     closures mutating an outer aggregate (`@acc`/`@data`/`@arr` — R2 / upvalue,
     and the `env_deep_copies` source), method attribute/local writeback (`desc`),
     and carriers (`x` = EVAL, `*dyn` = dynamic). R1 (dynamic/our) is low volume.
  3. **Caveat — Sub/closure values over-count.** `cheaply_unchanged` has no arm
     for `Value::Sub`/closures (`_ => false`), so a pull that re-reads a closure
     variable (`adder=51`) counts it stale every time even when it is the same
     closure. Treat Sub-valued stale-slot names as noise; the aggregate/scalar
     names are the real signal. (A future refinement could add a Sub arm keyed on
     callable id, but it is not needed to steer the slices.)

- **Slice B — `EVAL`/carrier precise *scalar* writeback (R2 flagship). DONE (2026-06-17).**
  `Interpreter::carrier_writes: Option<HashSet<String>>`; while `Some` (set around
  the 3 `exec_call_values`/`exec_call_pairs_values` carrier sites in
  `vm_call_exec_ops` via `begin_carrier`/`end_carrier`),
  `set_env_with_main_alias` logs every by-name env write. On carrier return,
  `writeback_carrier_writes(code, written)` copies the *current* env value of each
  logged **plain-scalar** name that has a caller slot back into that slot. The
  carrier site **keeps `env_dirty = true`** as a safety net. Nested carriers
  save/restore+merge the log; an erroring carrier restores the log state but skips
  the writeback (frame unwinds).

  **Why scalar-only + keep `env_dirty` (the bug the first cut hit).** The first
  revision *removed* `env_dirty` and wrote back *all* logged names. That regressed
  `S03-binding/nested.t` / `S05-modifier/my.t`: a slot holding a container with a
  live `:=` cell (`$struct` with a cyclic element bind) was overwritten from a
  COW-detached env copy, destroying the cell. Worse, the diagnosis showed the
  reconcile a carrier needs is sometimes for a name the carrier did **not** write
  (a prior `:=` bind whose propagation the *next* carrier's blanket `env_dirty`
  incidentally triggered) — an implicit dependency the precise log cannot see.
  So Slice B (a) reconciles only plain scalars precisely (`is_writeback_safe_scalar`
  — Int/Num/Str/Bool/Rat/…; never a container/instance/cell), and (b) retains
  `env_dirty` so the barrier pull still covers containers and implicit
  dependencies exactly as before. Removing `env_dirty` is **Slice F**, after R1/R3
  make those dependencies explicit.

  **Measured (MUTSU_VM_STATS, Slice A counters), `EVAL '$x=$x+1'` interleaved
  with an unrelated hot local read, 1000×:**

  | | locals_pulls | effective | stale_slots |
  |---|--:|--:|--:|
  | before (main) | 1000 | **1000** | 1000 |
  | after (Slice B) | 1000 | **0** | 0 |

  The reconciliation *work* (effective pulls / stale slots) drops to zero: the
  writeback reconciles the carrier's scalar writes immediately, so the
  `env_dirty`-triggered barrier pull finds every slot already coherent. The pulls
  themselves remain (env_dirty is retained) but now do no work — Slice F deletes
  them. This proves the precise carrier-reconcile mechanism that Slice F builds on.

  Pinned by `t/eval-env-dirty.t` (12). Gate met: `S03-binding/nested.t`,
  `S05-modifier/my.t`, `S29-context/eval.t`, `evalfile.t`, `S01.../eval_lex.t`,
  `S02-names/caller.t`, `S02-magicals/*` green; all env-dirty pins green.

  > **Slice F prerequisites surfaced here:** removing `env_dirty` requires (1)
  > precise reconcile for container/cell slots too (cell-aware, not a blind env
  > overwrite), and (2) making the implicit `:=`-bind→later-pull dependency
  > explicit (the bind reverse-write-throughs or sets its own precise signal),
  > plus the open-question-#2 audit that no carrier write bypasses the log.

- **Slice C — R1 reverse write-through. SUPERSEDED by measurement (2026-06-17).**
  A `MUTSU_VM_STATS` sweep of the canonical R1 patterns in a hot loop (our-var
  `$c=`, dynamic `$*dyn=`, by-name container `@a.push`, bareword→call) found each
  already does **only ~1–2 reverse-sync pulls total, effective=0** — the pure
  scalar by-name writers do **not** set `env_dirty` on the hot path, so there is
  no standalone pull traffic for Slice C to remove. The only large spurious pull
  source measured was the **EVAL/carrier** site (≈1001 pulls / 1000 EVALs,
  effective=0), so the real lever moved to the Slice F prereq below. The cold R1
  declaration/registration sites (`is Trait` handlers, class/enum/module reg)
  still set `env_dirty`, but they are metric-neutral and several are not simple
  R1 (module load imports an unknown name-set; enum reg writes many variant
  names) — they convert as part of the Slice F audit, not as a standalone slice.

- **Slice F prereq — EVAL carrier precise reconcile + drop its blanket. DONE (2026-06-17).**
  The per-EVAL blanket `env_dirty` is not set at the carrier site itself but by
  `with_nested_registers` (`src/vm.rs`), which unconditionally flags dirty after
  *any* nested run so the restored outer locals re-pull. For EVAL this is
  spurious: EVAL writes caller lexicals exclusively through
  `SetGlobal → set_env_with_main_alias`, which the Slice B carrier log already
  captures, so `writeback_carrier_writes` reconciles every scalar it wrote on
  return. `writeback_carrier_writes` is now **cell-aware** and returns a
  `fully_reconciled` bool: it copies plain scalars from env, **never overwrites a
  container/instance slot** (env may hold a COW-detached copy with live interior
  `:=` cells — the S03-binding/nested.t hazard), treating a container as coherent
  only when env and the slot share the same `Arc` (`cheaply_unchanged`), else
  leaving it to the barrier (`fully=false`). The EVAL carrier site
  (`vm_call_exec_ops.rs`) then restores the *pre-carrier* `env_dirty` (preserving
  a dirty the caller already owed) instead of the blanket when `fully && name ==
  "EVAL"`. Result: the EVAL-in-a-hot-loop benchmark drops **1001 → 2** pulls,
  output unchanged. Pinned by `t/eval-carrier-precise-writeback.t` (14:
  same-frame / ancestor-bare-GetLocal / two-deep / per-iteration / whole-array &
  whole-hash reassignment / in-place push / regex-`:let`-inside-EVAL /
  interleaved-hot-local). Gate: `S29-context/eval.t`, `S05-modifier/my.t`,
  `S03-binding/nested.t`, `t/eval-env-dirty.t` green.

  > **Why EVAL-only (the frame-scoping + open-q#2 lesson).** `env_dirty` is
  > **frame-scoped** (`push_call_frame` saves+resets it, `pop_call_frame`
  > restores), so a carrier's write reaches the caller only because the caller's
  > frame re-flags dirty after the call returns. The writeback is **local to the
  > current frame's `code.locals`**, so it can only reconcile names the carrier
  > wrote that the *current* frame slots — ancestor lexicals are read from env
  > (no stale slot) and self-contained. Dropping the blanket for **all** carriers
  > regressed `S05-modifier/my.t` / `S03-binding/nested.t` (8+1 subtests) because
  > regex `:let $a = …` writes a caller lexical **without** going through
  > `set_env_with_main_alias` — it bypasses the carrier log (open-question #2).
  > So the drop is restricted to EVAL, whose write path is fully logged; the
  > remaining carriers keep the net until open-q#2 (complete write logging) is
  > closed, which generalizes this drop to them.

- **Slice C′ — close the regex path of open-question #2; generalize the drop to
  the bareword carrier only. PARTIAL (2026-06-17).**
  Two parts. (a) *Completeness (regex path):* an embedded regex
  `{ }`/`:my`/`:let` block writes a caller lexical *directly* into `env` (via the
  interpreter's block-eval, not `set_env_with_main_alias`), so it bypassed the
  carrier log. `regex_eval.rs` now logs each such changed name into
  `carrier_writes` when a carrier is active (alongside the existing
  `pending_local_updates` push), making the log complete for the regex-scalar
  path open-question #2 flagged. (b) *Generalization (bareword carrier only):*
  the `fully`-reconciled blanket-`env_dirty` drop in `exec_exec_call_op`
  (the `exec_call_values` bareword carrier) is no longer scoped to `name ==
  "EVAL"`; any fully-reconciled bareword carrier now restores the pre-carrier
  dirty. The cell-aware `fully` flag (#3227) leaves diverged containers to the
  barrier and ancestor lexicals have no current-frame slot (read straight from
  env), so the frame-scoping argument carries over. *Validated:* the full roast
  whitelist (1281 files) is byte-identical with the bareword-carrier blanket
  dropped vs. the EVAL-only baseline (only delta = a stale-temp-file artifact in
  `S32-io/spurt.t`); EVAL-in-a-hot-loop stays at `locals_pulls=1`. Pinned by
  `t/single-store-slice-c-prime.t` (11) + `t/eval-carrier-precise-writeback.t`
  (14).

  > **What is NOT done — the `pairs`/`slip` carriers keep their blanket.** The
  > `exec_call_pairs`/`exec_call_slip` carriers dispatch interpreter builtins
  > (Test `is`/`ok`, topic-mutating `s///`, …) whose caller-lexical writes do
  > **not** all flow through `set_env_with_main_alias` or the carrier log. Their
  > write set can be empty while the carrier nonetheless wrote a caller slot, and
  > `writeback_carrier_writes` returns `true` (fully) for an empty set — so
  > dropping the net there silently loses the write (it regressed
  > `t/regex-m-s.t` `s/// updates $_`, `t/element-bind-cell.t`,
  > `t/regex-declarative-modifiers.t`; these are `t/`-only, so the whitelist did
  > not catch them — a reminder that the whitelist is necessary but not
  > sufficient for this invariant). Generalizing the drop to `pairs`/`slip`
  > requires the **full** open-question-#2 audit: logging *every* interpreter
  > env-write path (general assignment inside interpreter-executed routines, not
  > just regex), which is the substantial remaining Slice C′ work. Until then the
  > blanket stays on those two carriers, and `env_dirty` (with the cold R1
  > declaration sites and implicit `:=`-bind reconcile dependencies it still
  > carries) cannot be deleted (Slice F).

- **Slice C′ follow-up — close the `$x ~~ s///` writeback hole left by the
  bareword-carrier drop. DONE (2026-06-18).**
  The bareword-carrier generalization above (#3231) left a *latent* hole: a
  `$x ~~ s///` writeback inserts the mutated value into `env` directly
  (`vm_comparison_ops.rs`, bypassing `set_env_with_main_alias`) and only updates
  the *current* frame's slot via `update_local_if_exists`. When the substitution
  runs inside an EVAL, the current frame is the EVAL'd block (which has no `$x`
  slot), so `$x` is written to env but never logged into the carrier set — and
  the dropped net loses it (`EVAL '$x ~~ s/ab/xy/'` left `$x` unchanged). The
  general helper `note_caller_env_write` (carrier log + `env_dirty`) is now
  called at that writeback, so the carrier-return reconcile sees `$x`. Pinned by
  `t/eval-subst-writeback.t` (6). (Found during the `pairs`/`slip` investigation
  below: most EVAL'd mutations — `.=`, `.push`, `++`, plain `=` — already log via
  `set_env_with_main_alias` or leave `fully` false; only the direct-`env`-insert
  `~~ s///` writeback escaped.)

  > **Deeper finding on `pairs`/`slip` (2026-06-18): it is the CP-2 wall, not
  > merely incomplete write-logging.** Even after routing the direct-write paths
  > (`s///` topic, regex `:let`, regex embedded code) through the carrier log,
  > dropping the `pairs`/`slip` blanket still corrupts *deep `:=` bind-cell*
  > coherence (`t/element-bind-cell.t` tests 9/28/41/44/46/47): writing through a
  > bound scalar (`$abbrev := $struct[..]<..>[..]; $abbrev = 44`) mutates a shared
  > cell whose aliased container (`$struct`) is not name-trackable, and the
  > interaction of the carrier's reconcile/pull with the env↔locals
  > representation divergence for cell-linked containers actively corrupts the
  > container (it reads back `Nil`). This is the same divergence CP-2 hit. So
  > `pairs`/`slip` generalization needs not just complete write-logging but a
  > solution to deep-cell env↔locals coherence — deferred.

- **Slice D — finish R3 blanket-mark removal. AUDITED COMPLETE (2026-06-18).**
  A full audit of every `self.env_dirty = true` site (≈140 across `src/vm/`)
  against `MUTSU_VM_STATS` reverse-sync counters found **no significant redundant
  blanket left to remove**: the clear post-precise-dispatch redundancies were
  already deleted in #2709 (compiled-function `changed`-merge signal) and the
  Slice 6.3 `method_dispatch_pure` gate. The remaining marks fall into three
  categories, **none of which is redundant**:
  1. *The precise gate itself* — `let mark_dirty = !self.method_dispatch_pure; if
     mark_dirty { self.env_dirty = true }` (the CallMethod/CallMethodMut tails,
     `vm_call_method_ops.rs` / `vm_call_method_mut_ops.rs`) and the compiled-call
     `env_dirty = saved || changed` in `call_compiled_function_named`. These *are*
     the precision; deleting them loses correctness, not redundancy.
  2. *Legitimate mutations* — `@a.push` and friends (`vm_data_ops.rs`), mutating
     methods (`vm_call_method_mut_ops.rs`), declaration registration
     (`vm_register_ops.rs`). These genuinely write a caller variable in `env`, so
     the mark is required until the writer self-reports per-name (Slice F).
  3. *Carrier / block nets* — bareword-resolved-to-call (`vm_var_get_ops.rs`),
     interpreter fallbacks (`exec_call_*`), and control-flow block bodies
     (`vm_control_ops.rs`). Making these precise is the **carrier-drop** work
     (Slice B/C′), not a blanket *removal* — and it is gated by the same
     completeness + deep-`:=`-cell-coherence wall that defers `pairs`/`slip`.
  *Measurement:* `method-call` reverse-sync `locals_pulls=1`, `bench-class` 2,
  `bench-array` 4, `bench-hash`/`bench-string` 3 — i.e. single-digit pulls per
  *entire* benchmark, ~all one-time boundary marks, not per-iteration. The
  spurious traffic Slice D set out to remove is already gone. **Conclusion: Slice
  D needs no code change; advancing further means Slice E (upvalues) or the
  deferred carrier-drop completeness work, not more blanket deletion.** Pins
  (`t/method-env-dirty.t` / `t/named-call-env-dirty.t` / `t/zeroarg-env-dirty.t`)
  remain the regression guard for the precise gates.

- **Slice E — closure upvalues (prereq #1).**
  Capture free vars as indexed upvalue cells; remove `compute_needs_env_sync`
  whole-frame writeback and the per-call captured-env loops. Highest risk; lands
  only after A–D. Gate: 193 closure/block/routine/sort/gather/sigilless roast
  files (vm-dual-store.md Slice 3 set), `t/closure-captured-state.t`,
  `bench-array`/`array-ops` `env_deep_copies` → target 0.

- **Slice F — delete the coarse machinery. THE CONVERGENCE POINT (2026-06-18).**
  With every setter precise (A–E), remove `env_dirty`, `saved_env_dirty`,
  `ensure_locals_synced`, `sync_locals_from_env`, and the `ensure_locals_synced`
  call at `GetLocal`. `env` is now materialized only on carrier/closure/dynamic
  demand. Gate: full roast; metric `MUTSU_VM_STATS` `locals_pulls` = 0 on all
  `benchmarks/*.raku` with output unchanged; wall-clock no-regression on
  `method-call` / `bench-class` / `fib` (release).

  > **This is where the single-store campaign and the first-class-container
  > campaign converge (the 2026-06-18 finding).** Deleting `env_dirty` makes
  > `locals` the *single authority*, which is only safe if `env` and `locals`
  > never hold a *divergent representation of the same container* — exactly the
  > guarantee `docs/container-identity.md` Phase 2/3 is building. The ContainerRef
  > cell mechanism (Phase 2, largely landed for `:=` binds) solves COW-survival
  > *within one store*; the **dual-store env↔locals divergence** is the remaining
  > layer (it is why dropping the `pairs`/`slip` carrier blanket corrupts
  > `t/element-bind-cell.t` even with the cell mechanism present). So Slice F's
  > real prerequisite is **Slice E (upvalues) + container-identity Phase 2/3
  > (env↔locals container-Arc sharing)**, not more blanket/carrier surgery. The
  > `pairs`/`slip` carrier-drop becomes safe at the *same* moment Slice F does.

- **Slice G (follow-on, optional) — env materialization on demand.**
  Replace the per-call `clone_env()` snapshot with a frame-local overlay built
  lazily the first time a carrier/closure/dynamic read needs a name view (the
  multi-tier overlay from #2684 is the substrate). Targets the residual
  per-method-call `env_deep_copies` (vm-dual-store.md Slice 2 "remaining ~1").

---

## 5. Invariants (the contract the slices must preserve)

1. **Authority:** for any name with a slot in the current `code.locals`,
   `self.locals[slot]` is the truth. `env`'s copy is a cache, updated by forward
   write-through (`flush_local_to_env`) and made coherent on the reverse side by
   R1 write-through or R2 boundary writeback.
2. **No stale slot read:** after any carrier returns, every caller slot the
   carrier wrote by name is reconciled before the caller's next `GetLocal`. (The
   carrier-boundary writeback discharges this; Slice A's guard asserts it.)
3. **Special-name exemption:** `:=` cells, atomics, `shared_vars`, and `!attr`
   are authoritative in their cell/shared store — neither `locals` nor `env`
   overrides them. `sync_locals_from_env`'s existing skips encode this; the
   write-through paths must preserve the skips.
4. **No new by-name writer escapes the model:** every `set_env_with_main_alias`
   call is either (a) a forward mirror of a slot already written, (b) an R1
   slotted write that also updates the slot, or (c) inside an active carrier that
   logs the name. A `set_env_with_main_alias` that is none of these is a bug the
   Slice A guard must surface.

---

## 6. Risks & rollback

- **Highest-risk slice is E (upvalues).** It is deliberately last and guarded by
  Slice A's invariant assertions and the 193-file closure roast set. If a
  regression escapes, revert E alone — A–D stand on their own (they only make the
  existing reverse sync precise; they do not require upvalues).
- **EVAL completeness (Slice B).** The log must capture *every* path through
  which EVAL writes a caller lexical, including nested EVAL and EVAL-inside-regex.
  The Slice A guard (assert no stale slot after carrier return) is the safety net;
  ship B only when the guard is silent across the dynamic-scope roast.
- **Performance non-regression is a hard gate, not a hope.** vm-dual-store.md's
  #2746 lesson: a perf regression is invisible to `make test` and only shows as a
  CI release-roast timeout. Every slice records `MUTSU_VM_STATS` deltas here and a
  release wall-clock on `method-call`/`bench-class`/`fib` before merge.
- **Reverse write-through cost.** R1 adds a `find_local_slot` (linear in
  `code.locals`) per by-name write. By-name writes are cold (our/dynamic/register
  are not hot-loop ops); if any proves hot, precompute a name→slot map on
  `CompiledCode` (the same structure `free_var_syms` lives on).

---

## 7. Why this is the right reframing (closing the CP-2 loop)

CP-2 asked "can we delete `sync_locals_from_env`?" and correctly answered "no —
EVAL needs by-name reverse sync forever." This redesign asks a different
question: "can the reverse sync be **precise and local** instead of **coarse and
global**?" — and the answer is yes, because:

- the **forward** half is already write-through (`flush_local_to_env`),
- **two** carrier classes already do precise boundary reconciliation
  (regex blocks, compiled-call merge),
- the remaining writers are a **finite, enumerable** set (§2), each reducible to
  R1 write-through or R2 boundary writeback.

What dies is not the reverse sync but its **coarse global form** — the
`env_dirty` bit and the per-`GetLocal` whole-`locals` scan. That is exactly the
hot-path cost the campaign set out to remove, and it leaves `locals` as the
single authority with `env` a derived view. The "permanent primitive" CP-2
identified survives only as O(written-names) writebacks at a handful of cold
carrier boundaries — which is the correct, minimal shape for it.

---

## 8. Open questions (resolve during Slice A)

1. Does any **hot-loop** op write env by name (would make R1 write-through hot)?
   Slice A's per-writer counter answers this; if yes, precompute name→slot.
2. Can `EVAL` write a caller lexical *without* going through
   `set_env_with_main_alias` (e.g. via a direct `env_mut().insert`)? Audit all
   `env_mut().insert` sites reachable from the EVAL/carrier path; any that bypass
   the logged writer must be rerouted or separately logged.
3. Do `$CALLER::` / `$OUTER::` explicit-scope writes form a fourth class, or are
   they R1 (slotted in some ancestor frame)? They write a *non-current* frame's
   lexical — likely a small R2-like boundary case at the dynamic-scope walk.
4. Is `compute_needs_env_sync` removable in stages (per closure-feature) or only
   atomically with Slice E? Determine whether read-only closures can drop it
   before upvalues land.
