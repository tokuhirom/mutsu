# ADR-0019 E5/E6/E7 design: routing every call entry through the resolver

Design pass for Phase E boxes E5 (ordinary VM method calls), E6 (mutation-aware and container
calls), and E7 (metaobject, qualified, and re-entrant calls). Depends on the E4 resolver and E2
rows (`adr0019-e2-e4-resolver-core.md`). These boxes rewrite the largest functions in the
codebase; the ADR already mandates that they "subdivide from a measurement, as C6 did" — this
doc fixes the measurement protocol and the per-entry cutover shape so each sub-slice is
mechanical. No code has landed for these boxes yet.

## The corrected entry inventory

The ADR preamble's inventory, verified and extended by the 2026-08-09 survey. Sizes are current
line counts.

**Opcode entries (6):**

| Opcode | Handler | Site | Size |
|---|---|---|---|
| `CallMethod` | `exec_call_method_op_impl` | `vm_call_method_ops.rs:500-1806` | ~1.3k |
| `CallMethodMut` | `exec_call_method_mut_op_impl` | `vm_call_method_mut_ops.rs:429-2169` | ~1.7k |
| `CallMethodDynamic` | `exec_call_method_dynamic_op` | `vm_call_method_mut_ops.rs:30-276` | ~250 |
| `CallMethodDynamicMut` | `exec_call_method_dynamic_mut_op` | `vm_call_method_mut_ops.rs:278-364` | ~90 |
| `HyperMethodCall` | `exec_hyper_method_call_op` | `vm_hyper_method_ops.rs:446-960` | ~515 |
| `HyperMethodCallDynamic` | `exec_hyper_method_call_dynamic_op` | `vm_hyper_method_ops.rs:1119-1339` | ~220 |

**Non-opcode entries:** `vm_call_method_with_values` / `vm_call_method_mut_with_values`
(forwarding shims, `vm_core_helpers.rs:87/233`), the `vm_run_instance_method` carrier
(`vm_core_helpers.rs:98` → `class_dispatch.rs:52`, used by `CallDefined` and user
`method sink` at `vm_exec_dispatch.rs:2460/2799`), the two JIT shims
(`vm_jit_helpers.rs:314/367` — pure re-entry wrappers whose tails must stay byte-identical to
the interpreter arms), and the three `vm_call_helpers` entries
(`call_method_all_with_fallback` :303, `call_method_mut_with_temp_target` :327,
`call_method_all_with_temp_target` :347).

**Fast-path bypass:** `ArrayPush` → `exec_array_push_op` (`vm_data_push_ops.rs:45-302`).

**Corrections/additions the ADR preamble should carry (folded into the ADR by this design PR):**

1. The interpreter slow path `call_method_with_values` lives in
   `runtime/methods_call_dispatch.rs:51-3827` (~3.8k lines), not `runtime/methods.rs`.
2. `call_method_mut_with_values` (`runtime/methods_mut_dispatch.rs:11-2565`, ~2.6k lines) is a
   **second slow path of comparable size** — E6's real target alongside
   `exec_call_method_mut_op`; it always bottoms out in `call_method_with_values` (:2564).
3. `exec_call_method_dynamic_mut_op` reaches the interpreter with **no native probe and no
   compiled-method probe at all** (`vm_call_method_mut_ops.rs:353` goes straight to
   `vm_call_method_mut_with_values`; only `try_native_buf_mut` at :342 precedes it).
4. `exec_hyper_method_call_dynamic_op` lacks the `skip_native`/`has_user_method` gate and the
   `nil_absorbs_method` arm that its static twin has (`vm_hyper_method_ops.rs:1231-1284` vs
   :626-653/:787) — a user method overriding a builtin is honored per-element by
   `HyperMethodCall` but potentially not by the dynamic form.

Items 3 and 4 are pre-existing behavior divergences, not just refactor targets: raku-verify
each (does the divergence produce observably wrong results today?) before the E6 cutover, and
fix them *by* the cutover (both entries consulting the same resolver removes the divergence by
construction) — with their own pinned tests, not silently.

## Facts that shape the cutover

- Every entry is a hand-ordered gauntlet: receiver normalization (ContainerRef/Scalar
  unwrapping, lazy forcing), dozens of method-identity intercepts (`.return`, `.throw`,
  `Pair.freeze`, `Lock.protect`, `Match.make`, proto interception, `xxKEY` fast paths, …),
  native probes, then the compiled/interpret fallthrough. The probe *order* differs per entry
  (e.g. `CallMethod` checks `try_fast_accessor_read` at :739 before junction threading at :778;
  the mut op puts the `xxKEY` block at :1369-1827 after its own gates).
- The mutation/writeback machinery is entirely at the caller boundary (opcode tails at
  `vm_exec_dispatch.rs:3090-3094` and :3160-3238: `attr_env_snapshot`, `same_binding` rebind
  test, `apply_pending_rw_writeback`, `drain_pending_local_updates_after_call`,
  `mirror_attr_env_to_cell`) and inside Tier-A native mut helpers that write back into env by
  name. **E5/E6 must not move any of this**; the boxes' own text says "retaining
  mutation/writeback semantics at the caller boundary".
- `exec_array_push_op` bypasses all dispatch stages. A user-class receiver escapes to the slow
  path by the `is_simple_array` shape test (so user `push` works by accident), but an
  `augment class Array { method push }` (or role mixed onto Array) is **not honored** for the
  1-arg `@a.push($x)` while the n-arg form (CallMethodMut) honors it via `has_user_method` —
  a real divergence today.

## Design decisions

**1. The cutover shape is "resolver decides, existing arms execute".** Each entry gains, at
the point where its *dispatch probes* start (after receiver normalization, before the first
probe), a single call:

```rust
let decision = self.resolve_dispatch(&receiver, method_sym, shape)?; // E4 sequence + guards
```

and its probe cascade becomes a `match` on the decision's first applicable candidate: user
candidate → the existing compiled/interpret invocation path; native row → the existing
`try_native_method` invocation (tier-selected); accessor → the existing accessor read;
nothing → the existing not-found path. The special-case intercepts and the writeback tails do
not move. This makes each cutover PR a *reordering-preserving* rewrite of one entry's probe
section, reviewable against the taxonomy below, instead of a rewrite of a 1.7k-line function.

**2. Interceptor taxonomy — every pre-resolver arm gets classified, per entry, before its
cutover PR.** Classes:

- **(a) receiver normalization** (ContainerRef/Scalar deref, lazy-IO force, Seq reify,
  junction auto-thread): stays before the resolver, unchanged.
- **(b) method-identity intercepts** (`.return`/`.throw`/`.emit`/`Pair.freeze`/
  `Lock.protect`/`Match.make`/proto interception/`xxKEY` fast arms/Nil pre-dispatch): stay put
  in E5/E6, each annotated with the E2 SPECIAL row that will eventually own it. Moving them is
  F-phase cleanup; E5/E6 only require that none of them *resolves by name* behind the
  resolver's back.
- **(c) dispatch probes** (`skip_native` computation, `has_user_method` gates,
  `try_native_method`, `try_fast_accessor_read`, `__mutsu_array_storage` delegation blocks,
  compiled/interpret fallthrough): replaced by the decision match. The storage-delegation
  blocks collapse into the `ReceiverExec::ArrayStorageDelegate` execution arm.
- **(d) writeback tails**: untouched (JIT shims keep byte-identical tails).

The classification lists (one table per entry, ~30-60 rows each) are produced during the
measurement slice and committed with it — they are the review artifact that makes the big
cutovers safe.

**3. Measurement before subdivision (the C6d protocol, mandated here).** A
`MUTSU_VM_STATS`-gated per-entry, per-outcome counter set
(`dispatch_entry_{callmethod,callmethodmut,...}_{intercept,native,user,accessor,notfound}`)
lands first, swept over full `t/` and whitelisted roast. The sweep decides: (i) sub-slice
order within E5/E6 (highest-traffic outcome first); (ii) which intercepts are dead or
near-dead (candidates for deletion rather than porting); (iii) the parity corpus for each
cutover (files that exercise the entry's every outcome). Do not skip this on the theory that
the survey above already measured — the survey counted code, not traffic.

**4. Slicing of the three boxes.**

- **E5 — ordinary calls**: measurement slice (counters + taxonomy tables for `CallMethod`,
  `CallMethodDynamic`, hyper non-mut paths, `call_method_all_with_fallback`), then per-entry
  cutovers: E5b `CallMethod`, E5c `CallMethodDynamic` + the two hyper entries' per-element
  probe (their loop/writeback structure is untouched; only the inner
  `try_native_method`/`call_method_mut_with_temp_target` probe pair consults the decision),
  E5d JIT-shim parity check (no code change expected — assert the shims still just re-enter
  the rewritten ops).
- **E6 — mutation-aware calls**: E6a measurement + taxonomy for `CallMethodMut`,
  `CallMethodDynamicMut`, `call_method_mut_with_values`, and the Tier-A helpers; E6b
  `CallMethodMut` probe-section cutover; E6c the two dynamic gaps (inventory corrections 3/4)
  fixed by routing through the same decision; E6d `ArrayPush`: keep the opcode and its
  container fast path, but gate it with a generation-refreshed pristine bit —
  `array_dispatch_pristine: bool` on the interpreter, recomputed on generation change as
  "no user candidates and no wrap rows under owners `Array`/`List`" — falling back to the
  `CallMethodMut` path when false. That closes the augment divergence with an O(1) hot-path
  check and no per-push registry probe.
- **E7 — metaobject/qualified/re-entrant calls**: the `run_instance_method` carrier's two VM
  sites, qualified dispatch (`methods_qualified.rs`), private-method dispatch
  (`resolution_private_method.rs` — becomes a sequence query with a private-visibility flag),
  `.^lookup`/`.^can`/`.^methods` reading the same sequence the calls use (the `.^can` live
  probe that calls `native_method_1arg` with a dummy `Value::NIL`,
  `methods_classhow_method_obj.rs:386-388`, is replaced by an E2 row lookup — also a
  correctness fix, since dummy-arg probing can false-negative), WALK, and the EVAL/`subtest`
  re-entrant paths. E7 is expected to subdivide per consumer once E5/E6 stabilize the
  decision API; scope each sub-PR to one consumer family.

**5. The interpreter slow paths shrink by attrition, not rewrite.** `call_method_with_values`
(~3.8k) and `call_method_mut_with_values` (~2.6k) are *reached less* as the VM entries consult
the resolver first; their own probe sections (the native fast path at
`methods_call_dispatch.rs:2770-3008`, the by-name dispatch groups at :3693-3715, the instance
fallback tail) get the same decision-match treatment as sub-slices of E5/E6 once the VM-side
cutovers are proven. Do not attempt a top-down rewrite of either function in one PR — that is
the "rewrites a function larger than any slice merged so far" trap the ADR warns about; the
decision-match conversion of one probe section at a time is the intended unit.

## Verification items

- **V1**: raku ground truth for inventory corrections 3/4 (dynamic-mut and hyper-dynamic
  divergences): construct programs where a user/augment method is called through
  `$obj."$name"(...)` mut forms and `».$name` — record raku behavior, pin tests.
- **V2**: augmented-Array `ArrayPush` divergence: `augment class Array { method push(...) }` +
  `@a.push($x)` vs `@a.push($x,$y)` — raku baseline, pinned test, fixed by E6d.
- **V3**: JIT parity — run the jit-stress suite after each E5/E6 cutover slice (CI covers it,
  but the local targeted check is the shim-tail byte-parity assertion).
- **V4**: hyper writeback — the per-element temp-target protocol
  (`call_method_mut_with_temp_target`'s env round-trip) must be preserved by E5c; the existing
  `TODO: compile to bytecode` there is out of scope.

## Risk notes

These are the highest-traffic code paths in the interpreter; the protections are (i) the
decision API is already shadow-proven by E4a/E4b before any entry consumes it, (ii) each
cutover changes one entry and ships with its taxonomy table and sweep evidence, (iii) writeback
tails and interceptors do not move, and (iv) local `make roast` before every cutover PR
(semantics-adjacent). Perf: each cutover PR cites the bench-CI rows for its main-merge commit;
a regression on fib/bench-tak blocks the next slice until explained.
