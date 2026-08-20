# ADR-0025: Cell boxing of captured scalars must be value-kind-blind — retiring the Instance skip, and demoting the escape verdict from correctness gate to perf hint

- Status: Accepted (slice 1 implemented; slice 2 closed out 2026-08-20 as
  already resolved by intervening work, no dedicated implementation needed
  — see "Slice 2 outcome" below; slice 3 planned)
- Date: 2026-08-11
- Related: ADR-0010 (lineage-scoped sharing), ADR-0023 (binding provenance),
  ADR-0024 (mainline lexical cells), PR #2749 (broad closure boxing reverted),
  PR #2751 (escape-aware sibling-cell sharing)
- Addresses: `todo/deep/closure-read-only-capture-loses-to-caller-env-same-name.md`
  (directions 1 and 2; direction 3 is a separate compiler bug — see §5)

## Context

### The failure family

A closure's captured plain-scalar lexical can be wrong in two mirror-image
ways when the closure is invoked away from its creation chain:

1. **Hijack** (Cro::HTTP2 serializer/parser tests, "check 4" family): the
   closure-call captured-env merge (`vm_closure_dispatch.rs`,
   `entry_or_insert_sym` = don't-overwrite) installs a plain captured value
   only if the caller's env chain does not already hold the name. A
   same-named `my` in whatever frame happens to be calling shadows the
   closure's own lexical — lexical scoping degrading to dynamic scoping.
2. **Staleness** (http-session "Session expires appropriately"): the
   creator's post-capture rebind of the variable is invisible to a closure
   whose captured env snapshotted a plain value — especially across
   threads, where the name-keyed `shared_vars` lane does not reach a
   closure stored in an attribute and invoked on a pre-existing worker.

A shared `ContainerRef` cell satisfies both at once: the cell wins the merge
(overwrite branch — identity protection) and the creator's writes go through
the cell (liveness). Merge-order tweaks can only ever fix one direction at
the other's expense; this is why the cell route is the only sound fix shape
(established in the deep ticket, reconfirmed here).

### Sharpened root cause (2026-08-11, this ADR's diagnosis)

The deep ticket attributed the hole to "read-only captures get no cell". The
real files say otherwise. In all three affected Cro test files the pattern is

```raku
my $encoder;                            # or: my $encoder = HTTP::HPACK::Encoder.new;
$encoder = HTTP::HPACK::Encoder.new;    # reassigned repeatedly between test blocks
... ( *.headers eq $encoder.encode-headers(@headers) ) ...   # captured in check closures
```

so `$encoder` IS captured-and-mutated (`captured_mutated_locals` contains
it — the vouch/`authoritative_free_vars` refusal is *correct*), and the
check closures ARE compiled in escaping position (array-literal elements
compile under `with_escape(true)`, `expr_data.rs`), so
`needs_cell_locals` triggers boxing. Every defense fired as designed except
the last gate:

**`box_captured_lexicals` (vm_register_ops.rs) and `box_decl_local_cell`
(vm_var_assign_local_get.rs) skip boxing when the slot currently holds an
`Instance`** (also Package/Array/Hash/Sub/Proxy). At every check-closure
creation `$encoder` holds an `HTTP::HPACK::Encoder` instance, so no cell
ever forms, the capture stays a plain by-value snapshot, and the
don't-overwrite merge exposes it to the caller chain. The same skip explains
direction 2: `Instant` (`$fake-now`) and session objects are Instances.

A/B proof (both repros in-repo, dependency-free, deterministic):

- `tmp/cap-hijack-instance.raku` (24 lines): Instance-holding
  captured-mutated scalar → hijacked (`v=CALLER`, and a Str shadow
  crashes with "No such method 'v' for invocant of type 'Str'").
  `tmp/cap-hijack-str-ab.raku`: byte-identical structure with a
  boxable Str → cell forms → protected (`v3=MAIN`). The value-kind skip is
  the single differing gate.
- `tmp/cap-stale-worker-first.raku` (22 lines): attribute-stored closure on
  a worker started BEFORE the creator's rebind → mutsu `after=1` (stale),
  raku `after=2`. Same skip, staleness direction.

### Why the skip is obsolete

The skip dates to the #2749 revert, which had two fatal reasons:

1. *Correctness*: `overwrite_instance_recursive` (the id→cell registry
   writeback that propagated a mutating method's result into bindings)
   did not descend `ContainerRef`, so a boxed scalar holding an instance
   lost mutations. **That mechanism no longer exists**: instances mutate
   in place through their Gc-shared attr cell (`commit_attrs`,
   `value_instance.rs`), whose doc explicitly guarantees visibility
   "in this frame, any caller frame, a `ContainerRef`-boxed capture".
2. *Performance*: broad boxing per closure creation (int.t ~1s → 150s).
   **Irrelevant to the skip**: the skip does not bound how often boxing
   runs, only which VALUES get wrapped once a trigger already fired. The
   frequency bounds are the loop/escape/dup-shadow triggers, which this
   slice does not touch.

Decisive: **"a cell whose contents is an Instance" is an
already-exercised state on main** — assign an object into an
already-boxed scalar (`my $x = 1; my &c = {$x}; $x = Obj.new`) and every
read path must already deref it (`tmp/cell-holds-instance.raku` passes on
main). Relaxing the skip creates no new VM state; it only changes when an
existing state is entered.

## Decision

### Slice 1 (implemented with this ADR): remove `Instance` from the value-kind skip

Two lines: drop `ValueView::Instance { .. }` from the skip `matches!` in
`box_captured_lexicals` and `box_decl_local_cell`. Everything else —
triggers (loop / escape / dup-shadow / named-sub decl-site), the
`type_constrained_unboxable` skip (cas), and the remaining kind skips —
is unchanged:

- **Package / type objects**: class handles; dispatch and identity paths
  are not audited for cells. Keep.
- **Array / Hash values in `$` slots** (itemized containers): very common
  (`my $a = [1,2,3]`), broad blast radius, intersects the ADR-0010 atomic
  lanes and Track B. Own slice (§4).
- **Sub**: the `&` lane has its own registries. Keep.
- **Proxy**: FETCH/STORE must not be hidden behind a cell. Keep permanently.

Measured (debug build, 2026-08-11):

- The three synthetics above flip to raku-correct; the full pre-existing
  synthetic battery (authoritative/vouch matrix, supply/tap shapes,
  cross-thread lane shapes) is unchanged.
- Cro::HTTP2 (main → slice 1): `http2-request-serializer.rakutest`
  notok 3 → **0**; `http2-response-serializer.rakutest` notok 3 → 1;
  `http2-request-parser.rakutest` notok 1 → 1 (residuals in §5).

### What slice 1 flushed out: the inline-exec upvalue-aliasing bug

`make test` caught exactly one regression, `t/lock-protect-shared-scalar.t`
1-2 (`Lock.protect` accumulation went to 0), and it was NOT a defect in the
relaxation — it was a pre-existing, latent VM bug the new cells exposed:
the inline `Lock.protect` executors (`exec_protect_block_inline`,
`call_protect_block`) run the protect block's CompiledCode **without
installing the block's own captured upvalue array**, so a `GetUpvalue` in
the block body indexes the ENCLOSING closure's array. The protect block's
`GetUpvalue(0)` meant `$i`; the enclosing Promise closure's slot 0 was the
newly-boxed `$l` Lock cell; `$r += $i` became `$r += Lock` and accumulated
nothing. Latent until now because `capture_upvalues` freezes `Some` only
for `ContainerRef` cells — rare pre-slice-1. Both protect sites now swap
the block's array in around the exec (restored after).

**Update: the follow-up audit is complete** (see
`news/2026-08/upvalue-array-inline-exec-audit.md`). Every other
"exec a Sub's compiled code inline, outside closure dispatch" site was
checked: the eager map/grep `run_reuse` loops (and every `run_nested`/
`run_compiled_block` caller) already run inside `with_nested_registers`,
which resets `self.upvalues` to empty on entry — safe by construction, since
an out-of-range `GetUpvalue` index always falls back to a by-name env read.
The one genuinely unguarded arith site (`vm_xx_repeat_thunk`, `EXPR xx N`)
and two VM-native `gather`/`take` forcing paths
(`force_lazy_list_vm_inner`, `force_lazy_list_vm_n_inner` — a `LazyList` has
no upvalue array of its own, so these reset to empty rather than installing
a substitute) now get the same swap-or-reset treatment. The proposed
structural fix (an RAII guard making "`self.upvalues` belongs to the
currently-executing cc" a property enforced once, not per-site) remains a
candidate for slice 2 proper, now that the site inventory is complete. This
is the expected shape of slice-1 fallout: cells entering paths that never
saw them, each surfacing as a deterministic test failure (the safety net
working, per CLAUDE.md's risk definitions).

CI's full roast then caught a second member of the same fallout class
(deterministic across all three jobs):
`roast/integration/advent2013-day14.t` hung after test 6 — the
`config_combiner` shape. The captured vow `my $v = $p.vow` (an Instance,
newly cell-boxed: the nested loop-param assignment registers as a free
write, so `v` is captured-and-mutated and the `start` block is
thread-escaping) shares its name with a **multi-param for-loop parameter**
(`for %kvs.kv -> $k, $v`). `build_for_bind_stmts` binds multi-params via
plain `Stmt::Assign`, whose exec writes THROUGH a `ContainerRef` — every
iteration wrote a config Str into the vow's cell, and `$v.keep(%result)`
after the loop called `.keep` on a Str, so the promise never resolved.
This is precisely the corruption direction the loop-param ticket predicted
("a ForLoop binding that wrote through such a cell would corrupt the outer
counter"). Fixed in `exec_for_loop`'s multi-param prep
(`vm_for_loop_body.rs`): a scalar multi-param whose name is currently
bound to a cell has that binding SEVERED for the loop's duration (env
entry removed, slot reset) — the pre-loop save/restore already preserves
the cell itself, so only the loop-duration binding becomes a plain fresh
value, per ADR-0023's fresh-binding provenance. Pin: test 7 of
`t/closure-capture-instance-cell.t`. The READ-side loop-param bug
(GetUpvalue bypass, single-param shape) remains open in its ticket — this
fix removes only the write-corruption direction for multi-params.

### Slice 2 (planned): the escape verdict must stop being a correctness gate

Invariant to establish: **every captured plain user scalar is either
authoritative (proven never-written → by-value overwrite-install) or a
shared cell (overwrite-install).** The don't-overwrite merge lane stops
being load-bearing for plain scalars; it remains for dynamics, the topic,
`self`, metadata, and the §4 exclusions.

Today a closure the compiler deems non-escaping (call-arg position — a
`@registry.push($cb)`, `.tap($cb)`, `Holder.new(now => $cb)` before the
named-arg fix) gets no cell for its captured-mutated variables and relies
on the caller-chain live-read — correct only while the calling chain IS the
creating chain. Every mis-verdict is a latent hijack/staleness bug of
exactly this family. Per CLAUDE.md's gain/risk definitions an incomplete
static analysis must bound COST, never CORRECTNESS.

Mechanism: extend the existing decl-site machinery
(`needs_cell_named_sub_ref_slots` → `box_decl_local_cell`) with a
slot-indexed compile-time set (working name `cell_captured_ref_slots`):
slots of plain-scalar locals that are captured by ANY nested closure and
are vouch-refused — i.e. `captured_mutated_locals` ∪ (`own_container_writes`
∩ captured) ∪ (`own_call_arg_sources` ∩ captured ∖ `scalar_bind_locals`).
That is precisely the complement of `authoritative_free_vars` within the
captured set, so the dichotomy is exhaustive by construction. Decl-site
boxing makes the binding a cell from birth: no boxing-moment ordering
races, sibling closures share trivially, and the value-kind question of
slice 1 disappears for these names (the seed at declaration is Any).
Creation-site `box_captured_lexicals` stays as a no-op-when-already-cell
backstop.

Perf gates (mandatory, in order):

1. `MUTSU_VM_STATS` counter (`decl_cell_boxes`) — assert ≈0 across
   `benchmarks/` (debug build per the counters rule).
2. **`roast/S32-num/int.t` wall-clock is the named canary** — the #2749
   blowup (~1s → 150s+, hidden from `make test`) must be re-checked in a
   release build BEFORE pushing.
3. Bench CI history is the final verdict per CLAUDE.md.

If the canary regresses, diagnose the cost source (per-creation Arc churn
vs env insert) rather than re-gating on escape — e.g. hoist the boxing out
of per-iteration redeclaration, or intern the cell in the declaration plan.

Cross-thread audit rider: the ADR-0024 implementation notes (points 5-7)
enumerate frame-independent "assign this name by value" utilities
(`assign_rw_target_expr`, `set_env_with_main_alias_sym`,
`sync_shared_vars_to_env`) that historically replaced cells with plain
values. Points 5/7 were fixed via the mainline-map-specific
`mainline_lexical_cell` lookup; slice 2 must generalize those two sites to
preserve ANY `ContainerRef` env entry (point 6's fix already is generic).

### Slice 2 outcome (2026-08-20): closed out without a dedicated implementation

`todo/deep/adr0025-slice2-implementation-plan.md` (written 2026-08-11) was
picked up for direct implementation and re-verified against `main` first, per
this project's standing "things go stale fast" triage rule. Every premise the
plan was built on turned out to already be fixed by intervening, independently
motivated work, so no code change was needed:

- **Step 0 (the cross-thread stale-plain-over-cell race).** The named repro
  (`http2-response-serializer.rakutest`, "check 4" family, ~50% fail rate over
  8 runs on 2026-08-11) now passes **0/8 failures** over 8 fresh runs of the
  debug binary — and all 29 of its subtests pass on every run, not just check
  4. The other three HTTP/2 suites named in the plan's residuals
  (`http2-request-parser.rakutest` 61/61, `http2-request-serializer.rakutest`
  32/32, `http2-response-parser.rakutest` 9/9) are also fully green. The most
  likely fix is `2011b083b` (2026-08-19, "reuse the source cell for SetGlobal
  `:=` binds and stop dropping cell promotions across nested call frames"),
  which touched exactly the closure-dispatch captured-env merge
  (`vm_closure_dispatch.rs`) the plan's Step 0 pointed gdb at, for an
  unrelated symptom (`t/has-attr-binding.t`) that shared the same merge-site
  defect class.
- **Steps 1-4 (decl-site cells for vouch-refused captured scalars).** The
  plan's own motivating examples — a closure stored via `@registry.push($cb)`,
  `.tap($cb)`, and a constructor named-arg (`Holder.new(now => $cb)`), both as
  a pre-bound variable and as a literal written directly at the call site —
  all read the creator's post-capture rebind correctly on `main`, verified by
  ad hoc repros under `tmp/` (not committed; the mechanism is already
  regression-tested by the pins below). So do less obvious shapes the plan
  did not name: a plain (non-method) function call passed a stored closure
  variable, and a closure literal assigned directly into an array/hash
  *element* (`@cb[0] = -> { $s }`, not a `$`-named `my`). Root cause: this
  plan predates two changes that together close the "escape verdict" gap it
  targeted — `cf9dc72be` (2026-08-04) made `method_escapes_closure_args`
  unconditionally `true` (every method-call closure argument escapes, not
  just `then`/`tap`/`act`/`start`), and the pre-existing `escaping_position`
  flag already covers assignment/VarDecl RHS, `return`, bind RHS, and literal
  collection elements — which is every syntactic position a closure needs to
  reach to become reachable later. What remains classified non-escaping
  (control-flow bodies, sort/map/grep predicate blocks) is correctly
  non-escaping: those blocks are invoked synchronously and never stored, so
  no staleness window exists for them.

Existing pins (`t/closure-capture-instance-cell.t`,
`t/for-loop-param-start-sibling-isolation.t`, `t/named-sub-lexical-scope.t`,
`t/lock-protect-shared-scalar.t`, `t/closure-container-capture-alias.t`,
`t/closure-arg-shares-its-captured-container.t`) all stay green; no new
mechanism means no new pin was added. The two adjacent findings the ADR's §5
separated out (the loop-param hijack, and the `http-session` rc=139 crash)
have themselves both since been resolved and their ticket files retired —
see `news/2026-08/for-loop-param-getupvalue-hijack-fix.md` — independently of
this slice.

### Slice 3 (follow-ups, each its own slice)

- **Type/`where`-constrained scalars**: still skipped (`cas` resolves its
  target by name — S17-lowlevel/cas.t; constraints re-check at the
  assignment chokepoint). Prerequisite: make `cas`/constraint checks
  cell-aware, then fold these into slice 2's set.
- **`$`-held Array/Hash (itemized containers) and Package-valued scalars**:
  measure how often they are captured-and-mutated in real suites before
  designing; intersects Track B / ADR-0001 layer 3a.
- **`@`/`%`/`&`**: reference-shared already; rebinding staleness is a
  narrower hole. Deferred with ADR-0024's identical limitation.

## Alternatives rejected

- **Merge-order / authoritative widening**: cannot satisfy hijack
  protection and liveness simultaneously (deep ticket; the
  `my $s = 0; @cb.push({ $s }); $s = 42` regression example pins the
  liveness direction).
- **Lazy collision-triggered boxing**: shadow introduction has no single
  chokepoint; an incomplete detector yields flaky, load-order-dependent
  wrong answers (ADR-0024 rejected the same idea with precedent
  S12-construction/roles-6e.t).
- **Keeping the Instance skip and special-casing HPACK-like shapes**:
  test-specific hacks are banned; the skip is not protecting any live
  mechanism anymore (§ "Why the skip is obsolete").

## §5. Adjacent findings this diagnosis separated out

1. **Direction 3 (loop-param hijack) is a different bug** —
   `todo/tickets/closure-for-loop-param-hijacked-by-same-named-captured-outer.md`,
   root cause now verified by gdb: a for-loop parameter WITHOUT a
   pre-existing local slot allocates none (`param_local` is a lookup, not
   an alloc — `compiler/stmt.rs`), so the param name is invisible to the
   compiled body's `own` set, the body's reads register as FREE variables,
   and `compute_upvalues` rewrites them to `GetUpvalue` — reading the
   captured outer value directly, bypassing both the frame env and the
   ForLoop binding entirely. Fix is compiler-side (loop params must be own
   bindings of the cc). Co-requisite validation: its 11-line repro must be
   re-run when slice 2 lands (cells widen what a captured name can carry
   into a closure frame).
2. **Residual "check 4" failures after slice 1** (kept in the deep
   ticket, narrowed): `http2-response-serializer.rakutest` test 14 — an
   `$encoder`/`@headers` check where the cell now delivers the right
   object, so the remaining mismatch is elsewhere (suspects: `@headers`
   liveness through the capture, or HPACK dynamic-table state trajectory);
   `http2-request-parser.rakutest` test 44 — reads
   `*.body-blob.result eq $payload`, i.e. NOT the encoder family at all
   (suspects: cross-stream DATA demux or `Buf`-capture). Each needs its
   own shadow-bisect.
3. **`http-session-inmemory/persistent` crash rc=139 ON MAIN** at test 2
   (was 10/13 on 2026-08-09) — pre-existing regression unrelated to this
   campaign; filed as
   `todo/tickets/http-session-tests-crash-rc139-on-main.md`. The deep
   ticket's session acceptance criterion is blocked behind it.

## Acceptance criteria

- Slice 1 (this PR): the three synthetics (`t/closure-capture-instance-cell.t`
  pins them); `http2-request-serializer.rakutest` notok=0; no `make test`
  regressions; full roast delegated to CI.
- Slice 2: `t/` pin for the call-arg-stored closure shape (push/tap/ctor);
  perf gates above; the merge-site regression example still passes; the
  ADR-0023/0024 pin files stay green
  (`t/for-loop-param-start-sibling-isolation.t`, `t/named-sub-lexical-scope.t`).
- End state: the deep ticket's `tmp/h2-bisect14.raku` WHICH-identity check,
  and Cro session files once the rc=139 regression is fixed.
