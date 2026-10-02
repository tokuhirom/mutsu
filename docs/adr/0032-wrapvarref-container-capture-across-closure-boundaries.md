# ADR-0032: `WrapVarRef` container capture is a property of the capture edge, not of the named-sub declaration form

- Status: Partially implemented — Slice 1 (D1+D2+D3b) AND Slice 2 (retire the
  peephole) shipped together 2026-08-19, with one deliberate deviation from
  the design (the rw-arg/`:=` call-arg helper is EXCLUDED from D1, not
  included as designed) and one probe (`X`, `.VAR.WHICH` identity) found to
  be a separate, pre-existing bug outside this mechanism's reach. Slice 3
  (probe `O`, `is raw`/`is rw` parameter identity) is not started — still
  explicitly out of scope per §2.1. See "Outcome" below.
- Date: 2026-08-19
- Related: ADR-0018 (slot-addressed lexical capture), ADR-0023 (binding
  provenance), ADR-0024 (mainline lexicals for named subs), ADR-0025 (captured
  scalar cells, value-kind-blind), ADR-0027 (loop-frozen value capture cascade),
  ADR-0021 (argument named-ness), `docs/captured-outer-cell-sharing.md`
- Addresses: `todo/deep/captured-outer-pair-container-alias.md`

## 1. Context

### 1.1 The Raku semantics at stake

A handful of syntactic sites capture a scalar variable's *container*, not a
snapshot of its value:

```raku
my $v = 1;
my $p = (key => $v);        # Pair.value IS $v's container
my $q = Pair.new("k", $v);  # same (2-positional form binds the value raw)
my $c = \($v);              # Capture positional aliases $v
my $l = ($a, $b);           # List elements alias their source containers
f($v);                      # `is rw` / `is raw` parameters
my $x := $v;                # binding
```

`$v = 2` afterwards is visible through every one of them, and `$p.value = 9`
writes back into `$v`.

### 1.2 How mutsu implements it today

The compiler tags such a site with `OpCode::WrapVarRef { name_idx, slot }`
(`Compiler::emit_wrap_var_ref`, `src/compiler/expr_call.rs:30`), where `slot` is
the *emitting frame's* local slot for that name, or `u32::MAX` when the compiler
knows the name is not a local of this frame. Each consumer
(`MakePair` / `MakeNamedArg` in `src/vm/vm_mixin_does_ops.rs:519`, `MakeCapture`
and `MakeArray` in `src/vm/vm_data_ops.rs`, `Pair.new` in
`src/vm/vm_call_method_mut_ops.rs:612`, the rw-arg and `:=` paths) funnels the
tagged value through `capture_var_cell_inner`
(`src/vm/vm_data_ops.rs:234`), which promotes the *local slot* to a shared
`ContainerRef` cell and hands the cell to the consumer.

`capture_var_cell_inner` can only do that for a local of the **currently
executing frame**. Its own comment states the hole:

```rust
let Some(idx) = idx else {
    // The named scalar is not a local of this frame (a captured/outer
    // variable read through the closure env), so there is no slot to box
    // into a shared cell.
    ...
    return inner;   // <- snapshot
};
```

### 1.3 The one escape hatch that exists — and why it is shaped wrong

There is already a working cross-frame mechanism, but it is keyed on the
*declaration form* of the reader rather than on the capture edge.
`compile_named_sub_body` (`src/compiler/helpers_sub_body.rs:563-593`)
peephole-scans a **directly nested named sub's** compiled ops for the adjacency
`GetGlobal(i)` immediately followed by `WrapVarRef { name_idx: i }` and records
two facts:

- **Half A — the container must exist.** The parent's local slot goes into
  `self.code.needs_cell_named_sub_ref_slots`. At runtime `exec_set_local_op`
  (`src/vm/vm_var_assign_set_local.rs:243-294`) calls `box_decl_local_cell`
  (`src/vm/vm_var_assign_local_get.rs:327`) at the *declaration*, so a real
  shared cell exists in both the owner's slot and its env entry.
- **Half B — the read must not destroy the container.** The captured name goes
  into the child's `container_ref_capture_syms`. At runtime
  `exec_wrap_var_ref_op` (`src/vm/vm_misc_assign.rs:633-648`) then re-reads the
  *raw* cell out of env instead of using the already-dereferenced value on the
  stack. Its comment calls this "the no-local-slot half of `capture_var_cell`".

Both halves are correct and shipped. They simply never run for any other reader:
a pointy block, an anonymous `sub {...}`, a bare block, a class/role method, a
`start`/`supply` body. That is the entire bug.

The peephole is also brittle in its own right: `src/compiler/expr_method.rs:175`
has to *pop an already-emitted `ContainerizePair` op back off* purely to keep the
`GetGlobal`-immediately-followed-by-`WrapVarRef` adjacency intact for
`Pair.new`, and the emitter's own comment (`expr_method.rs:170`) warns that any
further `WrapVarRef` between the two ops breaks detection.

The mechanism landed in commit `3b9ead7c2` with a single pin,
`t/captured-outer-pair-container-alias.t` (6 tests, all named-sub shapes), and is
documented **only** in code comments — `docs/captured-outer-cell-sharing.md`,
the design doc for the surrounding decl-site-boxing campaign, does not mention
`WrapVarRef` / `container_ref_capture_syms` / `*_ref_slots` at all. That
documentation gap is part of why the mechanism was never generalized.

### 1.4 Measured surface (origin/main `fb54d5ce9`, debug build)

Repros were written under `tmp/` and A/B'd against `raku`. `1` means "the value
was snapshotted at construction time"; the raku column is the reference.

| Probe | Shape | raku | mutsu |
|---|---|---|---|
| 1 / 2 | named sub in a block, `key => $v` / `Pair.new` | 2 | 2 |
| 3 / 4 / 6 | same frame, read and write-through | 2 / 2 / 9 | 2 / 2 / 9 |
| A / D / E | plain closure/method read liveness (no container capture) | 2 | 2 |
| V / W | `is rw` arg and `:=` bind performed *inside* a closure | 2 / 5 | 2 / 5 |
| M / P | named sub at file scope / inside a sub | 2 | 2 |
| **C / B / H** | **pointy block: read liveness / write-through / `Pair.new`** | 2 / 9 / 2 | **1** |
| **N / Q / 5b** | **file-scope pointy / pointy inside a sub / anon `sub {}`** | 2 | **1** |
| **F** | **class method `key => $v`, write-through** | 9 | **1** |
| **K** | **closure stored in an array (escaping) builds the Pair** | 2 | **1** |
| **J / Z2** | **a cell already exists (escaping captured-mutated)** | 2 | **1** |
| **L / Z4** | **named sub nested one level down, owner two frames up** | 2 | **1** |
| **T** | **`\($v)` Capture built inside a closure** (`U`, same frame, is fine) | 7 | **1** |
| **X** | **`$v.VAR.WHICH` compared across a closure boundary** | True | **False** |
| **Y** | closure-built Pair vs same-frame Pair share one container | 3 3 | 1 1 |
| O | `-> $v is raw { key => $v }` — a *parameter*, not a capture | 2 | **1** |

Three conclusions follow, and they are what makes this an architectural decision
rather than a Pair patch:

1. **It is not a Pair bug.** `T` (`Capture`) and `X` (`.VAR` identity) fail
   identically. Any fix confined to `pop_pair_operands_capturing` would leave
   them broken — exactly what the deep ticket's design constraint forbids.
2. **Half A and Half B are independently missing**, each provably.
   - `Z4` / `L`: the reader IS a named sub (so Half B fires) but the owning
     frame is two levels up, so `self.local_map` has no slot and Half A never
     runs. Result: snapshot.
   - `Z2`: an escaping, captured-mutated capture, so ADR-0025's
     `box_captured_lexicals` DOES create the cell — verified under
     `rust-gdb` (breakpoint `src/vm/vm_register_ops.rs:1016` fires with
     `s = "value"`) — and the Pair *still* snapshots. A second breakpoint at
     `src/vm/vm_misc_assign.rs:643` shows why: `slot = 4294967295` (`u32::MAX`)
     and `code.container_ref_capture_syms` is `Vec(size=0)`, so the cell sitting
     in env is never recovered. Half B alone is the blocker here.
3. **The residue is confined to reads.** `V` / `W` (an `is rw` argument or a
   `:=` bind performed inside the closure) already work, because a write marks
   the name in `free_var_writes` and the write paths have their own machinery.

`compute_upvalues` (`src/opcode.rs:6059`) additionally rewrites a *read-only*
free scalar's `GetGlobal` to `GetUpvalue`, and `exec_get_upvalue_op`
(`src/vm/vm_var_assign_local_get.rs:68`) pushes `val.into_deref()`. So for the
common read-only shape the container is destroyed one op earlier still. `Z1`/`Z3`
(same closure also writes the name, so no promotion happens) fail identically,
which confirms upvalue promotion is an *additional* stripping point, not the
root cause.

## 2. Decision

Make the container-capture edge a **compiler-tracked fact about the capture
itself**, and generalize both halves from "directly nested named sub" to "any
nested compiled code".

### D1 — Record the capture edge where it is emitted, not by peephole

Populate `CompiledCode::container_ref_capture_syms` inside `emit_wrap_var_ref`
(`src/compiler/expr_call.rs:30`) whenever the name does **not** resolve to a
local of the emitting frame (`self.local_map.get(name)` is `None`, i.e. the
`slot == u32::MAX` case the op already encodes). This single rule covers every
consumer — `MakePair`, `MakeNamedArg`, `MakeCapture`, `MakeArray`, `Pair.new`,
the rw-arg and `:=` paths — because they all go through this one emitter.

Consequences:

- The `GetGlobal`+`WrapVarRef` adjacency scan in
  `src/compiler/helpers_sub_body.rs:563-593` becomes redundant and is deleted
  (see slice 2), and with it the `ContainerizePair` pop-back hack at
  `src/compiler/expr_method.rs:175-178`.
- The set is per-`CompiledCode` and already exists, already consulted by
  `exec_wrap_var_ref_op`, and already GC/clone-neutral. No new runtime state.

**Why the widening is shadow-safe.** `container_ref_capture_syms` is documented
as deliberately narrow: "runtime reference wrapping may read a captured env cell
only for this explicit set; ordinary same-named env cells must not override a
shadow value" (`src/opcode.rs:3798`). D1 preserves that invariant exactly,
because its condition is *"the name is not a local of the emitting frame"* — the
same condition that makes the op carry `slot == u32::MAX`. A shadowed local **is**
a local of the frame, so it never enters the set and continues to take the
slot-addressed path. `t/list-alias-shadowed-name.t` test 8
(`my $v = 10; { my $v = 99; } my $pair = (k => $v)` must read `10`) is the pin
for this and must stay green.

### D2 — Bubble the edge to the owning frame as a decl-site boxing request (Half A)

Generalize `needs_cell_named_sub_ref_slots` (suggested new name
`needs_cell_ref_capture_slots`; keep the field, widen its contributors) so that
**every** nested compiled code contributes, not just a named sub body:

- At the point a child `CompiledCode` is attached to its parent
  (`CompiledCode::add_closure_code`, `src/opcode.rs:6127`, plus the named-sub,
  method and `start`/`supply` body attachment sites), for each sym in the child's
  `container_ref_capture_syms`:
  - if the parent's `local_map` owns the name, push its slot into the parent's
    `needs_cell_ref_capture_slots`;
  - otherwise re-publish the sym in the **parent's own**
    `container_ref_capture_syms`, so the request keeps bubbling to whichever
    ancestor actually declares the variable. This is the transitive shape the
    existing `named_sub_captures` / `needs_cell_named_sub_free` bubbling already
    uses, and it is exactly what probe `L`/`Z4` needs.

The runtime side is unchanged: `exec_set_local_op` already boxes a slot listed in
that set via `box_decl_local_cell`, and that function's skip list
(`@`/`%` route to `box_decl_local_container_cell`; `Package`/`Array`/`Hash`/
`Sub`/`Proxy` values and type/`where`-constrained scalars are skipped) is kept
verbatim for this slice.

Two constraints inherited from the surrounding campaign apply and must be
honoured rather than rediscovered:

- **Slot-addressed, never name-addressed.** Same-named `my` declarations share
  one slot, so a name-keyed request would box an unrelated sibling block's
  declaration — the failure that regressed `roast/.../let.t` 4/9/12 when
  decl-site boxing was first attempted (`docs/captured-outer-cell-sharing.md`,
  session 41). The existing field is already slot-addressed for this reason;
  keep it that way, and only publish a *sym* when bubbling past a frame that
  does not own the name.
- **A fresh cell per loop iteration.** Per ADR-0023's fresh-binding provenance,
  a redeclaration must not reuse the previous iteration's cell. Pin:
  `t/for-loop-param-start-sibling-isolation.t`.

### D3 — Stop the read path from stripping the container (Half B)

Two sub-decisions:

- **D3a.** `exec_wrap_var_ref_op`'s env-cell recovery now fires for closure
  bodies as well, purely as a consequence of D1 populating the set. No code
  change in the VM.
- **D3b.** A sym in `container_ref_capture_syms` is **excluded from upvalue
  promotion** in `compute_upvalues` (`src/opcode.rs:6073-6086`, add the filter
  alongside the existing `written` / `runtime_bound` filters), so its read stays
  a `GetGlobal` and D3a's by-name recovery applies verbatim. Ordering
  constraint: the capture set must be complete before `compute_upvalues` runs
  for that code — D1 populates it during emission, which precedes the
  post-compile analysis passes, so the existing order already satisfies this.
  A regression test must pin the ordering, because a future pass that appends to
  the set after `compute_upvalues` would silently reintroduce the bug.

### D4 — Keep `capture_var_cell_inner`'s no-slot branch as an unchanged backstop

With D1-D3 the value that reaches a consumer is already a `ContainerRef`, so
`capture_var_cell_inner`'s first line (`if inner.is_container_ref() { return
inner; }`) handles it and the `slot == u32::MAX` snapshot branch is never taken
for a tracked edge. That branch stays exactly as it is for genuinely unboxable
sources (a `Package`-valued or constrained scalar the boxing skip refused, a
dynamic, a hand-built `VarRef` with `slot: None`). Do **not** add a by-name
cross-frame slot search there — that is the anti-pattern `slot: u32::MAX` was
introduced to kill (`t/list-alias-shadowed-name.t`).

### 2.1 Explicitly out of scope for this decision

- **Probe `O`** (`-> $v is raw { key => $v }`): here `$v` *is* a local of the
  running frame, so `capture_var_cell_inner` takes its found-a-slot branch and
  boxes the param slot into a **fresh** cell that does not alias the caller's
  container. That is a parameter-binding identity defect in a different branch of
  the same function, not a capture-edge problem. It deserves its own ticket, and
  should be re-measured once this ADR lands (a `is raw` param slot that already
  holds the caller's cell would then take the `is_container_ref` early return).
- **ADR-0025 slice 2** ("the escape verdict must stop being a correctness
  gate", `todo/deep/adr0025-slice2-implementation-plan.md`). This ADR adds one
  *specific, syntactically-triggered* boxing reason; slice 2 widens the
  *general* captured-and-mutated trigger. They are orthogonal and compose by
  construction: that plan's `cell_captured_ref_slots` is specified as
  "mirroring `needs_cell_named_sub_ref_slots`" and is consumed through the very
  same `box_decl` gate in `src/vm/vm_var_assign_set_local.rs:245`, so whichever
  lands first, the other folds into it with no semantic change. Note the two
  triggers are genuinely different sets: slice 2 keys on *mutation*, D2 keys on
  *container capture* — probe `C`'s `$value` is never mutated by any closure, so
  slice 2 alone would not fix it.
- **`@`/`%`/`&` sigils.** Already reference-shared; rebinding staleness is the
  narrower hole ADR-0024/0025 deferred, and this ADR does not touch it.
- **Making `ContainerRef` deref universal** (Track B / ADR-0001). Deliberately
  not started here; see alternative 4.

## 3. Alternatives considered and rejected

1. **Special-case Pair construction** — snapshot-free handling inside
   `pop_pair_operands_capturing`, or a Pair-specific "remember the source name"
   field. Rejected: banned by the deep ticket's design constraint, and provably
   insufficient — probes `T` (`Capture`) and `X` (`.VAR` identity) are not Pairs
   and fail identically.
2. **Runtime name search of the creating frame** from
   `capture_var_cell_inner` when there is no local slot. Rejected: cross-frame
   by-name guessing picks a same-named shadow slot and boxes its stale value —
   the precise failure `slot: u32::MAX` was introduced to prevent
   (`t/list-alias-shadowed-name.t`, the CSV::Table state-sync bug). The compiler
   already knows the answer; do not re-derive it by guessing at runtime.
3. **Box every captured scalar at its declaration.** Rejected by ADR-0025 on
   measured grounds: the #2749 broad-boxing form took `roast/S32-num/int.t` from
   ~1s to 150s+. D2's trigger is syntactic and rare by construction.
4. **Make `exec_get_upvalue_op` (and `GetGlobal`) stop dereferencing.** Rejected:
   every ordinary read of a boxed capture would then yield a cell and each
   consumer would need its own deref — that is the universal-`ContainerRef`-deref
   programme (Track B), which ADR-0001 §7 / ADR-0013 §7 leave as its own campaign
   and which must not be started as a side effect of a Pair fix.
5. **Add a non-dereferencing `GetUpvalueRaw` op** instead of D3b. Rejected for
   this slice: it needs a second opcode, a second rewrite rule in
   `compute_upvalues`, and a decision at every consumer about which variant it
   wants — for an answer identical to D3b's. D3b costs one env `HashMap` lookup
   on a syntactically rare shape, and the upvalue array's own contract already
   treats a non-frozen entry as "read it live from env by name", so keeping these
   names on the env path is consistent with the existing design rather than an
   exception to it. Revisit only if the perf gate in §4 says otherwise.
6. **Snapshot the container into the upvalue array at closure creation.** This is
   already what `capture_upvalues` (`src/vm/vm_register_ops.rs:780`) does — it
   freezes a `ContainerRef` and only a `ContainerRef`. It does not help: probe
   `Z2` proves the freeze happens and the container is still stripped, because the
   deref is on the *read*, not on the freeze, and Half A is separately missing.

## 4. Risks and gates

- **Widened cell population.** D2 makes more locals cells, which is exactly the
  ADR-0025 slice-1 fallout class: cells entering paths that never saw one. The
  named canaries from that campaign apply verbatim —
  `t/lock-protect-shared-scalar.t` (inline-exec upvalue array),
  `roast/integration/advent2013-day14.t` (for-loop multi-param writing through a
  captured cell), `t/closure-capture-instance-cell.t`,
  `t/list-alias-shadowed-name.t`,
  `t/for-loop-param-start-sibling-isolation.t`, `t/named-sub-lexical-scope.t`.
  Treat any failure there as a real regression, not noise.
- **Lost upvalue promotion (D3b).** Bounded to names that appear in a container
  capture inside a closure. Gate in order: (1) a `MUTSU_VM_STATS` counter
  (`ref_capture_decl_boxes`) read off a **debug** build, asserted ~0 across
  `benchmarks/`; (2) `roast/S32-num/int.t` wall-clock on a **release** build —
  the ADR-0025 canary for the #2749 blowup — checked before pushing; (3) bench CI
  history as the final verdict.
- **Env reachability of the cell from a closure frame.** D3a assumes the callee's
  env chain resolves the creator's cell under the plain name. `exec_get_upvalue_op`'s
  documented env fallback and probe `A`'s working read liveness both indicate it
  does, and the named-sub path (probe `P`) already relies on exactly this lookup —
  but a closure's captured-env merge is `entry_or_insert_sym` (don't-overwrite,
  ADR-0025 §1), so a same-named entry in the caller's chain could shadow it. **This
  is the first thing to verify in implementation** (break on
  `src/vm/vm_misc_assign.rs:644` for probe `Z2` after D1). If it does not hold,
  prefer resolving through the frozen upvalue entry (`self.upvalues[index]`, which
  holds the cell verbatim) over widening the merge rule.
- **Over-boxing a constrained scalar.** `box_decl_local_cell` already refuses a
  type/`where`-constrained scalar and reference-valued slots; a refused edge
  simply keeps today's snapshot behaviour. That is a known, documented residue
  (ADR-0025 slice 3), not a new one.
- **A new cell can make an inner expression-declared `my` clobber the owner.**
  `news/2026-08/expr-decl-writes-through-captured-cell.md` documents the
  instance that was fixed: an expression-position `my $g` (inside an `if`
  condition) compiles to env-only `MarkVarDeclContext; SetGlobal` with no
  local slot, finds the captured cell under the bare env key, and used to
  write *through* it into the caller's variable — including for a
  class/role/submethod/instance/multi/private method body, whose
  `CompiledCode` is registered separately from the enclosing frame and so
  never appeared in that frame's `closure_compiled_codes`. Both `SetGlobal`
  write-through sites (`vm_exec_dispatch.rs`, `vm_env_helpers.rs`) now consult
  `expr_declared_syms` at runtime and skip the write-through for any
  genuinely fresh binding, regardless of which mechanism boxed the cell — so
  this is now a general protection, not something D2 needs to re-derive.
  Pins: `t/expr-decl-lexical-no-leak.t`, `roast/S02-types/whatever.t` #45,
  `roast/S02-types/pair.t` #181.
- **Saved-frame propagation.** `docs/captured-outer-cell-sharing.md` §3 records
  that a newly formed cell must reach every `call_frames.saved_env` /
  `saved_locals` as well, or a method return rolls the cell back to the
  pre-boxing plain value. `box_decl_local_cell` runs at a declaration, so the
  window is narrow, but verify it for a capture created inside a method body
  (probe `F`).
- **Not a risk:** temporary red CI on the branch. Per CLAUDE.md, roast detecting
  the fallout deterministically is the safety net working.

## 5. Implementation slices

- **Slice 1 (the decision above).** D1 + D2 + D3b together — they are only
  correct as a set, so do not split them. Restricted to `$`-sigil names
  (`docs/captured-outer-cell-sharing.md` §7.1d records that broad `@`/`%` boxing
  regressed ~12 files through decont leaks; precise detection only). New pin
  `t/closure-container-capture-alias.t` — a *sibling* of the existing
  `t/captured-outer-pair-container-alias.t`, which already pins the named-sub
  shapes and must not be rewritten — covering probes `B`, `C`, `F`, `H`, `J`,
  `K`, `L`, `N`, `Q`, `T`, `X`, `Y`, plus the already-passing controls (`1`, `3`,
  `6`, `A`, `P`, `U`, `V`, `W`) so a future change cannot regress them, plus an
  ordering pin for D3b.
- **Slice 2.** Retire the peephole: delete the adjacency scan in
  `src/compiler/helpers_sub_body.rs:563-593` and the `ContainerizePair`
  pop-back at `src/compiler/expr_method.rs:175-178`, confirming D1 subsumes both.
  Kept separate so slice 1's blast radius is measurable on its own.
- **Slice 3.** Probe `O` (`is raw`/`is rw` parameter slot identity) — re-measure
  first; file or close its ticket accordingly.

### 5.1 Existing pins the implementation must keep green

Mechanism pins: `t/captured-outer-pair-container-alias.t` (the named-sub Pair
capture, the mechanism this ADR generalizes), `t/pair-new-container-alias.t`
(the eight `Pair.new` container semantics, including "the named form does NOT
alias" and "the key does not alias"), `t/varref-binding.t` (the `VarRef`
representation), `t/list-alias-shadowed-name.t` (the shadow-safety negative
half).

Decl-site-boxing canaries: `t/captured-outer-cell-sharing.t`,
`t/captured-outer-container-cell-sharing.t`,
`t/closure-capture-instance-cell.t`, `t/for-loop-param-start-sibling-isolation.t`,
`t/named-sub-lexical-scope.t`, `t/lock-protect-shared-scalar.t`,
`t/expr-decl-lexical-no-leak.t`, `roast/integration/advent2013-day14.t`.

## 6. Acceptance criteria

- Every probe in the §1.4 table matches `raku`.
- Every pin in §5.1 stays green; `make test` green; full `make roast` delegated
  to CI and green.
- The §4 perf gates pass in the stated order.
- `todo/deep/captured-outer-pair-container-alias.md` moves to
  `news/YYYY-MM/` once slices 1-2 have landed.

## 7. Appendix — the probes, verbatim

The `tmp/` scratch files are not committed; these are the canonical shapes the
§1.4 table measured, so slice 1 can start from a runnable set. Each comment is
the `raku` answer.

```raku
# C — pointy block, read liveness                     mutsu: 1
{ my $v = 1; my $mk = -> { key => $v }; my $p = $mk(); $v = 2; say $p.value }      # 2

# B — pointy block, write-through                     mutsu: 1
{ my $v = 1; my $mk = -> { key => $v }; my $p = $mk(); $p.value = 9; say $v }      # 9

# H — pointy block, Pair.new                          mutsu: 1
{ my $v = 1; my $mk = -> { Pair.new("k", $v) }; my $p = $mk(); $v = 2; say $p.value }  # 2

# F — class method                                    mutsu: 1
{ my $v = 1; class C { method mk() { key => $v } }; my $p = C.mk; $p.value = 9; say $v }  # 9

# K — the closure itself escapes (array element)      mutsu: 1
{ my $v = 1; my @cb = (-> { key => $v },); my $p = @cb[0](); $v = 2; say $p.value }  # 2

# Z2 — a cell provably exists, and it still snapshots  mutsu: 1
{ my $v = 1; my @cb = (-> { my $p = (key => $v); $v = $v; $p },);
  my $p = @cb[0](); $v = 2; say $p.value }                                        # 2

# Z4 / L — named-sub reader, owner two frames up      mutsu: 1
{ my $v = 1; my $step = -> { sub inner() { key => $v }; inner() };
  my $p = $step(); $v = 2; say $p.value }                                         # 2

# T — Capture built inside a closure (U is the same-frame control, and passes)
{ my $v = 1; my $mk = -> { \($v) }; my $c = $mk(); $v = 7; say $c[0] }             # 7

# X — container identity across a closure boundary    mutsu: False
{ my $v = 1; my $mk = -> { $v.VAR.WHICH }; say $mk() eq $v.VAR.WHICH }            # True

# Y — a closure-built Pair and a same-frame Pair must share one container
{ my $v = 1; my $mk = -> { (k => $v) }; my $p1 = $mk(); my $p2 = (k => $v);
  $p1.value = 3; say "$v {$p2.value}" }                                           # 3 3

# O — OUT OF SCOPE (a parameter, not a capture)       mutsu: 1
{ my $v = 1; my $mk = -> $x is raw { key => $x }; my $p = $mk($v); $v = 2; say $p.value }  # 2
```

Controls that already pass and must keep passing: the ticket's own named-sub
repro, the same-frame `(key => $v)` / `Pair.new` / write-through trio, a plain
closure read (`-> { $v }` after `$v = 2` reads `2`), an `is rw` argument passed
from inside a closure, and a `:=` bind performed inside a closure.

## 8. Note for a future reader

The mechanism this ADR generalizes had no design document — it lived entirely in
comments on `src/opcode.rs:3794`/`:3798`, `src/compiler/helpers_sub_body.rs`
and `src/vm/vm_misc_assign.rs`. When slice 1 lands, add a section on the
container-capture edge to `docs/captured-outer-cell-sharing.md` so the next
person looking for "how does a captured outer keep its container" finds it
where the rest of the cell-sharing campaign is written down.

## Outcome

### Slice 1 + Slice 2, shipped 2026-08-19

D1, D2, D3b were implemented as one change, and slice 2 (retiring the
peephole) was folded into the same PR rather than landed separately, because
keeping both mechanisms running in parallel added complexity for no
measurable benefit — D2's generalized bubbling subsumes the peephole's
named-sub case exactly, confirmed by `t/captured-outer-pair-container-alias.t`
staying green.

- **D1** landed in `Compiler::emit_wrap_var_ref` exactly as designed: it
  populates `container_ref_capture_syms` whenever `local_map` does not own
  the name, gated through the existing `is_plain_lexical_name` filter (which
  already excludes `@`/`%`/`&`-sigiled names, satisfying the "`$`-sigil
  only" restriction with no extra code).
- **D2** landed as a shared `Compiler::bubble_container_ref_capture_syms`
  helper, called from all three nested-code attachment sites:
  `add_closure_code_baked` (pointy blocks, anon subs, bare blocks,
  `start`/`supply` bodies — they all route through this one function),
  `compile_named_sub_body` (replacing the old peephole scan there), and
  `compile_method_body` (a new call, since a class/role method's
  `method_compiler` is a fresh `Compiler::new()` with no access to the
  declaring frame's `local_map` — the bubbling call is the only place Half A
  can be requested for a method reader). `needs_cell_named_sub_ref_slots`
  was renamed to `needs_cell_ref_capture_slots` to match its generalized
  contributors.
- **D3a** needed no code change, as designed.
- **D3b** landed in `CompiledCode::compute_upvalues` as an added filter
  alongside `written`/`runtime_bound`.
- The `ContainerizePair` pop-back hack in `expr_method.rs` was deleted, as D1
  predicted: `emit_wrap_var_ref` no longer depends on op adjacency.

**Every probe in §1.4 now matches raku except `O` (explicitly out of scope)
and `X`**, which turned out to be unrelated to this mechanism (see below).

### Two false-positive sources found during implementation, not anticipated by the design

The design's core claim — "the name is not a local of the emitting frame" is
a sound proxy for "this is a captured outer variable" — is false for two
categories of `WrapVarRef` site the design did not examine, both found via
`roast/S02-types/pair.t` regressing after the initial implementation
(bisected with a `git worktree` baseline build, since the failure required
specific surrounding file context to reproduce):

1. **A bareword call argument is not a variable read.** The general-purpose
   rw-arg/`:=`-bind-source tagging in `helpers_call_args.rs` calls
   `emit_wrap_var_ref` for EVERY call argument's shape tag, including
   `Expr::BareWord` (a type/package/constant name passed positionally, e.g.
   the `Pair` in `isa-ok($pair, Pair)`) and `Expr::AssignExpr` (an
   anonymous-scalar-assignment temp). Neither is a local of the emitting
   frame, so D1's original unconditional rule treated `"Pair"` as though it
   were a captured outer lexical, bubbling a bogus decl-site boxing request
   for any LATER, unrelated `my $pair` sharing that spelling.
2. **A for-loop parameter is this frame's own binding but has no local
   slot, by design** (see `for_loop_param_syms`'s existing doc comment,
   which already documents an identical false-free-variable hazard for
   `compute_free_vars`). `local_map.get("pair")` returns `None` for a read
   of `for %h.pairs -> $pair { ... }`'s OWN loop variable, which D1
   misread as "captured from an ancestor frame."

**The fix, and where it landed:**

- (1) is fixed by NOT registering D1 at all for the rw-arg/`:=` call-arg
  helper — not by narrowing it to `Expr::Var` only. A narrower attempt
  (register for `Expr::Var`/`Expr::DoStmt` VarDecl, skip bareword/
  AssignExpr) still broke `t/hash-attr-map-default-element-assign.t`: that
  call site fires for every plain positional argument (not only an actual
  `is rw` one), so a genuine free-variable argument passed to an ORDINARY
  function inside a closure (`lives-ok { $c.h{3} = Str }` passes `$c` to an
  internal hash-element-assign helper) was ALSO wrongly boxed. Probes `V`/`W`
  (`is rw` argument / `:=` bind performed inside a closure) do not need D1 to
  pass — per §1.4 they already matched raku on unmodified `main`, through the
  pre-existing `free_var_writes` write-tracking machinery, which is
  independent of `container_ref_capture_syms`. **This is a deliberate
  deviation from the design's claim that D1 "covers... the rw-arg and `:=`
  paths": it does not, and should not.** A new `emit_wrap_var_ref_arg_tag`
  emits the bare op with no D1 registration; the rw-arg helper always calls
  it now, for every argument shape.
- (2) is fixed post-compile in `CompiledCode::compute_free_vars`
  (`opcode.rs`), retaining `container_ref_capture_syms` against
  `for_loop_param_syms` and `my_declared_enum_sym` — mirroring, verbatim,
  the two existing `free`-set exclusions immediately above it in the same
  function. Applied post-compile (not at `emit_wrap_var_ref` call time)
  because `compute_free_vars` already runs, for every nested-code kind,
  before that code's `container_ref_capture_syms` is read by the bubbling
  call at its attachment site — so the ordering the design's D3b note
  worried about ("the capture set must be complete before `compute_upvalues`
  runs") holds here for free.

Regression coverage: `t/closure-container-capture-alias.t` probe "shadow
safety" pins D1's shadow-safety invariant; the bareword/for-loop-param false
positives are pinned by `roast/S02-types/pair.t` and
`t/hash-attr-map-default-element-assign.t` staying green (no new dedicated
unit test was added for these two, since the roast/local-suite regression
they caused IS the regression test — reintroducing either bug fails an
existing file deterministically).

### Probe `X` (`.VAR.WHICH` cross-closure identity) is NOT fixed by this ADR

Root-caused to be unrelated to `WrapVarRef`: `.VAR` on a scalar variable
compiles via `compile_expr_method_on_var` to `CallMethodMut`, whose target
value is read via a plain `GetGlobal`/`GetLocal` (already dereferenced even
when the variable is boxed) — `.VAR`'s reflection-object identity comes
entirely from a separate, name-keyed `var_meta_value` env cache
(`src/runtime/runtime_var_meta.rs`) that has no cross-frame writeback
mechanism of its own. Confirmed independent of this ADR's mechanism by
control: the OLDER named-sub capture mechanism this ADR generalizes (which
predates it and already worked for `key => $v`) fails the identical
`.VAR.WHICH` shape (`{ my $v = 1; sub f() { $v.VAR.WHICH }; say f() eq
$v.VAR.WHICH }` — mutsu: `False`, raku: `True`, on unmodified `main`).
Filed as `todo/tickets/var-which-identity-across-closure-boundary.md`; pinned
(as an expected `todo`-marked failure, not a silent skip) in
`t/closure-container-capture-alias.t` probe `X`.

### Validation

`make test`-equivalent (full `t/` sweep, `cargo test --lib`, targeted roast:
`S02-types/pair.t`+siblings, `S05-capture/*`, `S06-signature/*`,
`S12-attributes/*`, `S12-methods/*`) all green locally. Perf gate (§4,
`roast/S32-num/int.t` on a release build): 0.058s wall clock — no trace of
the ADR-0025 `#2749` blowup this gate exists to catch. `make roast` itself is
delegated to CI per the project's standing policy, not run locally.

### Remaining

- **Slice 3** (probe `O`) is fixed, separately from this ADR, per its own
  explicit out-of-scope note in §2.1: `capture_var_cell_inner`
  (`src/vm/vm_data_ops.rs`) now checks the parameter's own `slot_hint` for an
  already-installed cell *before* following the `:=`-alias-root redirection
  a `is raw`/`is rw` bind also installs — see
  `news/2026-08/is-raw-param-container-identity.md`.
- **Probe `X`** needs its own design (see the ticket above); it is not a
  small extension of D1-D3.
- The §8 note (add a `docs/captured-outer-cell-sharing.md` section on the
  generalized container-capture edge) is still open — left for a follow-up
  docs-only pass rather than expanding this already-large PR further.
