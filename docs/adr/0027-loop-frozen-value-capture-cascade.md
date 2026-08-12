# ADR-0027: Loop-frozen value captures cascade through nested closure creation — frame-owned vouching gated on the live value kind

- Status: Accepted (Slice 1 implemented; Slices 2-3 planned)
- Date: 2026-08-12
- Related: ADR-0018 (slot-addressed lexical capture), ADR-0023 (binding
  provenance for spawn capture), ADR-0025 (value-kind-blind cell boxing),
  PR #2668 (lever C, `owned_captures`), PR #4627 (`frame_authoritative`
  runtime vouching)
- Addresses: `todo/deep/for-loop-var-shared-across-nested-closure-captures.md`
  (resolved, moved to `news/2026-08/for-loop-var-shared-across-nested-closure-captures.md`)

## Outcome (Slice 1, 2026-08-12)

Implemented as designed: `Interpreter::frame_owned` added (save/seed/consult
mirroring `frame_authoritative`), seeded at all four closure-entry sites
(`vm_closure_dispatch.rs`, `resolution_call_sub.rs`,
`resolution_map_grep.rs`, `resolution_map_grep_rw.rs`), consulted in
`compute_owned_captures` gated on the live value kind exactly as designed.

One additional prerequisite gap surfaced during implementation, not
anticipated by the design: a `for`-loop's own pointy parameter never
populated `loop_local_vars` in the first place (it binds via a direct
env/slot store, not the generic declaration path that populates
`loop_local_vars` for an ordinary loop-body `my`), so the OUTER closure in
the repro was never marked as an owned capture at all — nothing for the
Slice 1 cascade to propagate. Fixed by registering the for-loop's own
pointy-parameter name(s) into `loop_local_vars` at loop entry, reusing the
exact name set already computed for ADR-0023's `active_loop_param_names`.
See the news entry for the full account.

Pin: `t/loop-var-nested-closure-freeze.t`. `roast/S17-lowlevel/lock.t`
verified green across repeated runs (the live-cell gate's canary).
`t/http-router.rakutest` test 437 could not be directly re-measured — the
vendored Cro::HTTP suite currently hits an unrelated pre-existing parse-time
failure before reaching it — but the isolated repro this ADR was written
against now matches `raku` exactly.

Slices 2 (parity audit of secondary execution paths) and 3 (retirement-path
documentation) remain open, tracked as follow-up work rather than blocking
this slice.

## Context

### The bug

A `for`-loop pointy-block variable must be a fresh per-iteration binding: a
closure that captures it sees the value of *its* iteration forever. mutsu
gets this right for a closure created directly in the loop body, but loses
it the instant the closure is created one closure-*call* deep from the loop
body — the IIFE factory shape:

```raku
my $callback = -> { "base" };
for (10, 20) -> $v {
    $callback = -> $fn { -> { "$v:{$fn()}" } }($callback);
}
say $callback();   # raku: 20:10:base — mutsu (buggy): 20:20:base
```

The first iteration's inner closure (built when `$v == 10`) reports
`$v == 20` when invoked later from inside the second iteration's closure.
This exact shape is `Cro::HTTP::Router`'s `around` middleware chaining
(`RouteSet.transformer`), observed as `t/http-router.rakutest` test 437
("The around blocks are called in top-to-bottom order", expected `12345`,
got `11355`).

### Root cause (confirmed by reading; full trace in the deep ticket)

The per-iteration freeze mechanism is `SubData::owned_captures`
(`compute_owned_captures`, `src/vm/vm_register_ops.rs:433`): at closure
creation, free vars that are also in `self.loop_local_vars` are recorded;
at call time (`src/vm/vm_closure_dispatch.rs:415-419`) those names are
force-overwritten from the closure's own captured `data.env`, beating the
default don't-overwrite merge (`entry_or_insert_sym`, line 358) that
ordinary lexical shadowing needs.

But `self.loop_local_vars` is `std::mem::take`n at the top of every call
path (`vm_call_light.rs:54`, `vm_call_fast.rs:80`,
`vm_call_light_typed.rs:64`, `vm_run_loop.rs:268`, restored on return) —
correct for the callee's *own* loops, but it means: when the IIFE (itself
created in the loop body, correctly marked `owned_captures = {v}`) runs and
*its* body creates the inner closure, `loop_local_vars` is empty, so the
inner closure's `owned_captures` comes out empty. Its `data.env` still
captures the correct frozen value (`capture_closure_env` is unconditional),
but nothing forces that value to win when the inner closure is later called
from a frame whose env chain holds a later iteration's `$v` — the
don't-overwrite merge silently keeps the wrong value.

(`$fn` in the same repro survives because it is an IIFE *parameter*, never
written after binding, so the compile-time `authoritative_free_vars` /
runtime `authoritative_captures` lane force-installs it. `$v` is refused by
that lane by design — see next section.)

In short: `owned_captures`-ness must mean "this free var was loop-frozen at
*some ancestor closure-creation point* in the chain", but the current
implementation only asks the immediate creating frame.

### Why the obvious fix is documented as unsafe

The codebase already cascades a per-frame vouched name-set through nested
closure creation: `Interpreter::frame_authoritative`
(`src/runtime/mod.rs:2009`) is seeded on every closure entry from the cc's
`authoritative_free_vars` plus inherited `SubData::authoritative_captures`
(`frame_authoritative_set`, `src/runtime/resolution_map_grep.rs:16`), and
consulted by `compute_authoritative_captures` when a deeper closure is
created — arbitrary-depth vouching (PR #4627). The doc comment on
`frame_authoritative_set` explicitly excludes loop captures from this
mechanism:

> unlike loop `owned_captures`, which may be concurrently-mutated shared
> cells and are deliberately NOT included (a reader thread would freeze a
> stale snapshot — `roast/S17-lowlevel/lock.t`'s condition-variable
> busy-wait).

The hazard is concrete. In `lock.t`'s busy-wait shapes, a loop-body local
(`my $flag = False` inside `for ^$times`) is captured by two `Thread.start`
closures; one *writes* it, so `box_captured_lexicals`
(`vm_register_ops.rs:833`, trigger A) boxes it into a shared `ContainerRef`
cell before the env snapshot — the live cell is what lets the reader thread
observe the writer. The reader thread's *nested* predicate block
(`$c.wait({ $flag })`, created inside the Thread closure's frame) must keep
resolving `$flag` through that live cell. A naive cascade — "this name is
loop-vouched, force-freeze it in every descendant closure" — would let
`freeze_readonly_owned_captures` (`vm_register_ops.rs:386`) fire in the
nested frame: that pass deep-derefs a `ContainerRef`-valued owned capture
to a plain value whenever the *creating frame's* `code.captured_mutated_locals`
does not list the name — and the nested frame's code cannot see the sibling
thread's write (it lives under the *loop* frame's code). The live cell
would be snapshotted to `False` and the busy-wait would never terminate.
That is the exact stale-snapshot deadlock class the exclusion protects.

### The insight this ADR is built on

The call-time `owned_captures` override carries two flavors of entry, and
only one of them is load-bearing:

- A **cell-valued** owned capture (`ContainerRef` — a captured-and-mutated
  loop local boxed by trigger A) is *already* force-installed by the
  unconditional ContainerRef branch of the captured-env merge
  (`vm_closure_dispatch.rs:310-327`: a captured cell always overwrites the
  caller's stale entry, dynamics excepted). For these names the
  `owned_captures` override at line 415 installs the same cell again — a
  no-op.
- A **plain-valued** owned capture is the per-iteration frozen snapshot.
  The override is the *only* thing that makes it win over a same-named
  caller-chain entry. This is the entire semantic content of
  `owned_captures` at call time.

So the property that must cascade is precisely **"frozen plain value"**,
and the property that must NOT cascade is precisely **"live shared cell"**
— and the two are distinguishable at runtime by looking at the captured
value itself. No static analysis, no stored flag, no guess: if the value
being captured is a `ContainerRef`, some mechanism (loop-mutation boxing,
escape boxing, dup-shadow boxing, ADR-0025 decl-site boxing) proved a write
channel exists and installed a cell — liveness is required and the general
cell-overwrite merge already provides both identity and liveness. If the
value is plain, every write-after-capture channel mutsu has routes through
a cell that would have been installed *before* the env snapshot
(`box_captured_lexicals` runs first at every closure-creation op), so a
plain value under a loop vouch is a genuine frozen snapshot with no
legitimate writer — force-installing it can never mask a concurrent write.

This dynamic gate is the flake-proof form demanded by the repo's gain/risk
definitions: it re-checks the live value at every closure creation, so a
name that becomes a cell later (a sibling closure triggering boxing, a
future ADR-0025 slice widening decl-site cells) is excluded from that
moment on, automatically. A stored creation-time flag or a compile-time
annotation could go stale in exactly the way that turns into a
load-order-dependent flaky failure.

## Decision

Cascade loop-frozen ownership through nested closure creation with a new
per-frame register, `Interpreter::frame_owned`, mirroring
`frame_authoritative`'s save/seed/consult lifecycle but **gated on the live
value kind at the consuming closure's creation**:

1. **Seed on closure entry**: entering a closure frame sets
   `frame_owned = data.owned_captures` (the names that were just
   force-installed from this closure's frozen env — within this frame they
   *are* per-iteration-frozen bindings). Every other call-frame push
   empties it (like `loop_local_vars` / `frame_authoritative`), so a named
   sub called from a loop body cannot leak the vouch onto unrelated
   same-named lexicals.
2. **Consult at closure creation**: `compute_owned_captures` includes a
   free var in the new closure's `owned_captures` when it is loop-local in
   the current frame (unchanged), **or** when it is in `frame_owned` *and*
   the value currently bound in the creating env is **not a
   `ContainerRef`** (the live-cell gate).
3. **No new SubData field, no new call-time semantics.** Inherited entries
   land in the existing `owned_captures` Vec and flow through the existing
   call-time override; because they are plain by the gate,
   `freeze_readonly_owned_captures` (which only touches
   `ContainerRef`-valued entries) can never deref anything through them.
   Transitivity is free: entering the inner closure re-seeds `frame_owned`
   from its (now extended) `owned_captures`, so the vouch reaches arbitrary
   depth with one register.

The invariant `owned_captures ⊆ cc.free_var_syms` is preserved (both
membership tests filter `free_var_syms`), so consumers that rely on it
(`sequence.rs:64`'s genuine-name set) need no change.

### Why this preserves the `frame_authoritative` exclusion's guarantee

The lock.t scenario under this design: `$flag` is captured-and-mutated →
boxed into a cell before the Thread closures' env snapshots → the Thread
closure's `owned_captures` contains `flag` but its captured value is the
cell. Entering the Thread closure seeds `frame_owned ∋ flag`; when the
nested `{ $flag }` predicate is created, the gate sees the env value is a
`ContainerRef` and **excludes** `flag` from the predicate's
`owned_captures`. The predicate's captured cell is installed by the
general cell-overwrite merge — identity protected, liveness intact,
busy-wait terminates. The stale-snapshot class is excluded by construction,
not by hoping the shape doesn't occur.

Conversely the bug's scenario: `$v` is never written in the loop body → no
cell → the IIFE's captured `$v` is the plain frozen `10` → entering the
IIFE seeds `frame_owned ∋ v` → the inner closure's creation sees a plain
value → `v` joins its `owned_captures` → the later invocation from
iteration 2's chain force-installs the frozen `10`. Output `20:10:base`.

One residual asymmetry is inherited, not introduced: a write channel the
compile-time mutation analysis cannot see (a separately-registered method
writing the name, a `cas`-style rw-arg sink) leaves the value un-boxed and
therefore freezable — but that is precisely the exposure the *existing*
depth-1 `owned_captures` already has (the `type_constrained_unboxable`
skip and the atomicint special case in `opcode.rs:3375` exist because of
it). The cascade reuses the same judgment at more sites; it does not
create a new class of unsoundness, and every known boxing trigger fires
before the env snapshot the gate inspects.

## Mechanism (implementation plan)

### Slice 1 — the register, the cascade, and the pins (one PR)

**Step 1: plumbing (behavior-neutral).**

- Add to `Interpreter` (next to `frame_authoritative`,
  `src/runtime/mod.rs:2009`):

  ```rust
  /// Names the currently-running closure frame vouches for as
  /// loop-frozen (its own `owned_captures`, installed frozen at entry).
  /// A closure created in this frame inherits owned (force-overwrite)
  /// capture for any of its free vars listed here WHOSE CAPTURED VALUE
  /// IS PLAIN — a `ContainerRef`-valued name is a live shared cell and
  /// is excluded (the lock.t stale-snapshot hazard; ADR-0027). Seeded on
  /// closure entry, emptied on every other frame push, saved/restored
  /// like `frame_authoritative`.
  pub(crate) frame_owned: Vec<crate::symbol::Symbol>,
  ```

- Initialize empty in `runtime_init.rs` (near line 2325) and in the
  thread-clone construction (`runtime_thread.rs`, near line 718 where
  `frame_authoritative: Vec::new()` is set).
- Save/restore across call frames: add `saved_frame_owned` to `CallFrame`
  (`src/vm.rs:319` block, next to `saved_frame_authoritative`);
  `std::mem::take` in both `push_call_frame` variants and restore in
  `pop_call_frame` (`vm/vm_env_helpers.rs:62/87/110`).
- Reset in `with_nested_registers` (`vm/vm_run_loop.rs:248` save/take/
  restore list, next to `saved_loop_local_vars` — same isolation rationale
  as ADR-0023 Step 3: a routine called from the loop body must start
  unvouched).

**Step 2: seed at every closure-entry site that seeds
`frame_authoritative`.**

- `vm_closure_dispatch.rs:676` (VM closure entry): alongside the
  `frame_authoritative_set` call, set
  `self.frame_owned = data.owned_captures.clone()`.
- `resolution_call_sub.rs:661` (interpreter-path twin for map/grep blocks):
  same seed, same save/restore discipline as `saved_frame_auth`.
- The inline map/grep/first fast paths that set `vm.frame_authoritative`
  per iteration (`resolution_map_grep.rs:430`,
  `resolution_map_grep_rw.rs:216` and `:492`): seed
  `vm.frame_owned = data.owned_captures.clone()` at the same points (the
  block may itself carry owned captures when the map runs inside a loop
  body one call deep).

**Step 3: consult in `compute_owned_captures`
(`vm_register_ops.rs:433`).**

```rust
pub(super) fn compute_owned_captures(
    &self,
    compiled_code: &Option<std::sync::Arc<CompiledCode>>,
) -> Vec<Symbol> {
    if self.loop_local_vars.is_empty() && self.frame_owned.is_empty() {
        return Vec::new();
    }
    let Some(cc) = compiled_code else {
        return Vec::new();
    };
    cc.free_var_syms
        .iter()
        .filter(|sym| {
            self.loop_local_vars.iter().any(|set| set.contains(*sym))
                // ADR-0027 cascade: inherited loop-frozen vouch, gated on
                // the live value kind — a ContainerRef is a live shared
                // cell (handled by the cell-overwrite merge) and must NOT
                // be re-frozen (lock.t busy-wait).
                || (self.frame_owned.contains(*sym)
                    && !matches!(
                        self.env.get_sym(**sym).map(Value::view),
                        Some(ValueView::ContainerRef(_))
                    ))
        })
        .copied()
        .collect()
}
```

All three closure-creation ops that call `compute_owned_captures`
(`vm_register_ops.rs:210/297`, `vm_register_sub_ops.rs:85/161`) pick the
cascade up through this single chokepoint. Reviewer invariant to check:
`freeze_readonly_owned_captures` must remain a no-op for inherited entries
— it only acts on `ContainerRef`-valued names, which the gate excluded, so
no code change is needed there, but a debug assertion during development is
cheap.

**Step 4: pins.**

New `t/loop-var-nested-closure-freeze.t`:

1. The ticket repro (IIFE factory chain) → `20:10:base`.
2. The three already-working shapes from the ticket as regression pins
   (direct loop closure; IIFE stored-and-called-independently; the same
   chain built by `sub` calls without a loop — the last one pins that the
   vouch does NOT fire outside loops).
3. A depth-3 variant (factory returning a factory returning the closure)
   to pin transitivity.
4. A mutated-loop-local variant (`my $x = $v; ... { $x } ... $x++`) to pin
   that cell-valued names stay live through the cascade.
5. A thread busy-wait shape modeled on lock.t (loop-body `my $flag`,
   writer thread, nested reader predicate) to pin the gate — verify the
   expected behavior against `raku` first, and keep it deterministic
   (signal + join, no timing assertions).

Acceptance for the slice: the pins, `t/http-router.rakutest` test 437
(`12345`), `make test` locally, full roast delegated to CI.
`roast/S17-lowlevel/lock.t` is the named canary — run it locally a few
times (release build) before pushing; a deterministic failure there is a
gate bug, not flake.

### Slice 2 — parity audit of secondary execution paths

Sites that carry `authoritative_captures` inheritance but not (yet) owned
inheritance, each to be probed against `raku` with a small repro before
changing (do not widen blind):

- `pending_whenever_inherited_owned` (`resolution_call_sub.rs:741`,
  consumed in `resolution_eval.rs:203`): despite its name it currently
  carries only `authoritative_captures`. Probe: a `whenever` block inside
  a loop-created supply closure creating a nested closure over the loop
  var.
- `eval_block_value` re-entrant carrier (`runtime/mod.rs:1289` block):
  confirm the closure-entry seed in Step 2 covers embedded-block
  evaluation, or add the twin seed.
- The writeback filter at `vm_closure_dispatch.rs:1252` (consults
  `authoritative_captures`): check whether inherited owned entries need
  the same exemption to avoid writing a frozen value back over a caller's
  live one on return.

Each confirmed gap gets its own pin test; each non-gap gets a line in the
PR description so the audit is recorded.

### Slice 3 — retirement path (documentation only, no code)

ADR-0025 slice 2's end state ("every captured plain user scalar is either
authoritative or a shared cell") plus per-iteration cell severing (the
ADR-0025 multi-param precedent, `vm_for_loop_body.rs`) points at the
architecturally purest form: a fresh cell per iteration per binding, with
closures capturing the cell — which would make `owned_captures`, this
cascade, and `freeze_readonly_owned_captures` all removable (see
Alternatives, option 4). When that campaign starts, this ADR's mechanism
is the *compatibility bridge* to delete, not extend: note it in the
campaign's ADR and supersede this one.

## Alternatives considered

1. **Fold owned names into `frame_authoritative` / mirror it verbatim
   without a value gate.** Rejected: reintroduces the exact
   `S17-lowlevel/lock.t` stale-snapshot deadlock the exclusion comment was
   written to prevent (walked through in Context — the nested busy-wait
   predicate would have its live `$flag` cell deref-frozen to `False`).
   The exclusion is load-bearing; the fix must be a separate lane.
2. **Tag `owned_captures` entries with a creation-time "snapshot vs cell"
   flag and cascade only snapshot-flagged entries.** Functionally close to
   the chosen design, rejected because the stored flag duplicates
   information the value itself carries and can go stale: a binding boxed
   *after* the parent closure's creation (a later sibling closure
   triggering escape boxing, or ADR-0025 decl-site cells landing) would
   leave a stale "snapshot" verdict that force-freezes over a now-live
   cell — a load-order-dependent, potentially flaky wrong answer. The
   live-value check at each creation is the sound dynamic form of the same
   idea and costs one `matches!` per candidate name.
3. **Compile-time propagation** (annotate cc's lexically nested in loop
   bodies with loop-owned free vars, mirroring
   `propagate_authoritative_down`). Rejected: (a) the inline map/grep fast
   paths re-compile block bodies, so the annotation is lost on exactly the
   copies that run — the re-compile gap is why `frame_authoritative`
   needed a runtime form in the first place (PR #4627); (b) compile time
   cannot see the value kind, so the runtime cell gate would still be
   needed — the annotation adds a second mechanism without removing the
   first.
4. **Per-iteration fresh cells for all loop captures** (box every
   loop-local capture, sever the cell at each iteration's rebind, retire
   `owned_captures` entirely). The cleanest end state — cell identity per
   binding instance makes nesting cascade for free through the env, and it
   is the direction ADR-0018/0023/0025 are already converging on. Rejected
   as *this* fix because it is blocked on the remaining value-kind deref
   gaps (`box_captured_lexicals` still skips Package/Proxy and `$`-held
   aggregates — ADR-0025 slice 1 deliberately kept those skips), carries
   the #2749 perf-canary risk for the very common read-only loop capture,
   and belongs to ADR-0025's slice 2/3 sequencing. This ADR is designed to
   be subsumed by it (Slice 3 above).
5. **Merge-order tweaks** (make plain captured values overwrite in the
   captured-env merge). Rejected per ADR-0025's established dichotomy:
   overwrite-install of an unvouched plain snapshot fixes hijack at the
   cost of liveness (`my $s = 0; @cb.push({ $s }); $s = 42` must see 42) —
   the two directions cannot be reconciled without knowing *which* names
   are frozen, which is what this ADR's vouch provides.

## Acceptance criteria

1. The deep ticket's repro prints `20:10:base`; pinned with the other
   shapes in `t/loop-var-nested-closure-freeze.t` (Slice 1 Step 4 list,
   including the cell-liveness and no-loop negative pins).
2. `t/http-router.rakutest` test 437 passes ("The around blocks are called
   in top-to-bottom order", `12345`).
3. `roast/S17-lowlevel/lock.t` remains green (release build, a few local
   runs; it is the canary for the gate — treat a deterministic failure as
   a design bug, per the flaky-triage protocol).
4. No `make test` regressions locally; full `make roast` delegated to CI
   (this touches closure dispatch — a local subset is not sufficient).
5. On completion, `git mv` the deep ticket to
   `news/2026-08/for-loop-var-shared-across-nested-closure-captures.md`
   and rewrite it as an accomplishment, per `todo/README.md`.
