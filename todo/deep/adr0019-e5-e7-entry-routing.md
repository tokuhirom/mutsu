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

## Measurement slice results — CallMethod (E5 step 1)

Landed 2026-08-11: `MUTSU_VM_STATS`-gated counters per design decision 3 —
`record_dispatch_entry_outcome(entry, outcome)` (histogram keyed
`"<entry>:<outcome>"`, outcomes `intercept`/`native`/`user`/`accessor`/`notfound`) and
`record_dispatch_entry_intercept(entry, arm)` (bumps `intercept` AND a per-arm histogram
keyed `"<entry>:<arm>"`), both in `src/vm/vm_stats.rs` and generic so every later E5/E6
entry reuses them. This slice instruments exactly one entry: `CallMethod`
(`exec_call_method_op_impl`, `src/vm/vm_call_method_ops.rs`). Pure insertions only — the
diff is 0 deletions; no branch, condition, or return value changed.

Counting semantics: each executed `CallMethod` records exactly one of
`intercept`/`native`/`user`/`accessor` (verified per-file:
`sum(disjoint outcomes) == CallMethod` in the opcode histogram, e.g.
`roast/S12-methods/instance.t`: 11 == 11). `notfound` is a deliberate **overlay subset of
`user`**: `user` is recorded when the compiled/interpret fallthrough is *entered* (the only
pure-insertion point), and `notfound` is recorded additionally at the two visible
not-found completions (the `.?` Nil absorb and the propagated-`Err` peek before
`call_result?`) — X::Method::NotFound originates inside
`try_compiled_method_or_interpret_sym`, so there is no disjoint not-found tail at this
entry without restructuring. Disjoint total = `intercept + native + accessor + user`.
Note `METHOD_TOTAL` (`record_method_dispatch`) fires for all four `CallMethod*` opcodes,
so it does NOT equal the `callmethod:*` sum; cross-check against the opcode histogram's
`CallMethod` row instead.

Taxonomy table (line numbers as of this PR's revision of `vm_call_method_ops.rs`; the
anchor for each instrumented row is its `record_dispatch_entry_*` call):

| Line range | Class | Arm name / description | Counter key | Notes |
|---|---|---|---|---|
| ~500-550 | — | entry bookkeeping (arg decode, slip flatten, stack pops) | (none) | stack-underflow errors uninstrumented: internal errors, not dispatch outcomes |
| ~551-566 | a | LazyIoLines force | (none) | |
| ~567-586 | a+b | ContainerRef deref; `.^name`/`.WHAT`-on-cell Scalar-meta early return | `callmethod:containerref-scalar-meta` | the deref itself is (a); only the Scalar-meta return counts |
| ~589-597 | b | `is native(...)` NativeCall-bound method | `callmethod:nativecall` | |
| ~600-625 | b | `.hash`/`.Hash` on bare positional list | `callmethod:hash-on-list` | |
| ~627-641 | b | `Pair.freeze` on non-variable receiver | `callmethod:pair-freeze` | |
| ~636-652 | b | `proto method` body dispatch | `callmethod:proto` | includes its own `apply_pending_rw_writeback` tail (stays put) |
| ~653-662 | b | Exception `.Str`/`.gist` via user `message` method | `callmethod:exception-str-message` | |
| ~663-670 | b | `.return` control flow | `callmethod:return` | |
| ~672-711 | a | `.throw`/`.rethrow` backtrace attach | (none) | rebuilds target only; no completion |
| ~713-721 | b | `fail`/`die`/`throw`/... on Exception type object | `callmethod:exception-concreteness` | 0 in sweep — bareword receivers (`X::NYI.throw`) compile to CallMethodMut |
| ~722-737 | b | typed-array autoviv `push`/... on type object | `callmethod:autoviv-typed-array` | |
| ~738-757 | b | `.emit` supply-buffer / CX::Emit | `callmethod:emit` | one arm name for both completions |
| ~759-778 | c | fast 0-arg public-accessor read | `callmethod:accessor` | dispatch-probe outcome, not an intercept |
| ~780-797 | b | `.so`/`.not` via user-defined `Bool` | `callmethod:so-not-user-bool` | |
| ~798-799 | a | `flatten_scoped_env` | (none) | |
| ~800-857 | b | junction-invocant auto-threading | `callmethod:junction-invocant` | one count per call, not per eigenstate; inner per-eigenstate native/user probes deliberately uninstrumented |
| ~859-866 | b | junction-argument auto-threading | `callmethod:junction-args` | |
| ~867-882 | b | `Deprecation.report` | `callmethod:deprecation-report` | |
| ~884-905 | b | `Lock.protect` X::Multi::NoMatch guard | `callmethod:lock-protect-nomatch` | |
| ~906-947 | b | `Lock`/`Lock::Async.protect` inline fast path | `callmethod:lock-protect` | 0 in sweep — `$lock.protect` on a variable compiles to CallMethodMut |
| ~949-1043 | c | `skip_native` gate computation | (none) | pure gate, no completion; becomes resolver input at cutover |
| ~1044-1067 | a | Proxy auto-FETCH | (none) | |
| ~1069-1084 | b | lazy-list `gist`/`Str` placeholder | `callmethod:lazy-placeholder` | |
| ~1086-1100 | b | lazy gather `.first` incremental pull | `callmethod:lazy-first` | |
| ~1102-1121 | b | lazy `.pairs`/`.antipairs`/`.kv` index pipe | `callmethod:lazy-index-pipe` | |
| ~1123-1135 | b | lazy `.cache` identity | `callmethod:lazy-cache` | |
| ~1136-1190 | a | lazy-list forcing | (none) | the mid-normalization `X::Cannot::Lazy` return is an uninstrumented gap (error inside normalization, not a probe outcome) |
| ~1192-1255 | b | `.hyper`/`.race` with `batch`/`degree` | `callmethod:hyper-race-config` | branch-entry record also covers the two X::Invalid::Value completions |
| ~1257-1450 | b | HyperSeq/RaceSeq delegation (10 arms) | `callmethod:hyperseq-{hyper,race,is-lazy,configuration,name,what,isa,defined,map-grep,iterator}` | `map-grep` counted only at the <1000-item inline completion; larger lists fall through to the general probes (sets `hyper_race_wrap`) |
| ~1451-1457 | a | HyperSeq/RaceSeq → List conversion | (none) | |
| ~1458-1470 | b | Regex `.Bool`/`.so` topic smartmatch | `callmethod:regex-bool-topic` | 0 in sweep — variable receivers compile to CallMethodMut |
| ~1471-1484 | b | `.WHO` on pseudo-package | `callmethod:who-pseudo-package` | |
| ~1486-1501 | b | Failure `.print`/`.say`/... positional X::Multi::NoMatch | `callmethod:failure-print-nomatch` | |
| ~1503-1537 | b | unhandled-Failure explosion | `callmethod:failure-explode` | |
| ~1539-1551 | b | `.*`/`.+` on pseudo-method error | `callmethod:modifier-pseudo-error` | |
| ~1554-1560 | b | `.+` all-methods dispatch | `callmethod:modifier-plus` | delegates to `call_method_all_with_fallback` (its own E5 measurement entry, later slice) |
| ~1561-1576 | b | `.*` all-methods dispatch | `callmethod:modifier-star` | its not-found→empty-list completion is covered by this arm, not `notfound` |
| ~1578-1626 | c | Array-subclass `__mutsu_array_storage` delegation | `callmethod:native` / `callmethod:user` | a failed compiled delegation falls through to normal dispatch *uncounted* (recording at entry would double-count); gap noted |
| ~1630-1723 | b | Nil pre-dispatch block | `callmethod:nil-predispatch` / `callmethod:nil-autoviv` / `callmethod:nil-absorb` | the fall-through names (`defined`, `grep`, `say`, ...) reach the general probes and count there |
| ~1725-1751 | b | Any/Mu autoviv `push`/... | `callmethod:any-autoviv` | |
| ~1753-1768 | b | Proxy-subclass array mutate | `callmethod:proxy-subclass-mutate` | |
| ~1780-1817 | b | `shift`/`pop` value fast path | `callmethod:shift-pop` | method-identity arm inside the probe section |
| ~1818-1846 | c | hash-sentinel resolve + native probe | `callmethod:native` | |
| ~1847-1886 | b+c | `.Slip` `is default` fill / native / fallthrough | `callmethod:slip-default` / `callmethod:native` / `callmethod:user` | |
| ~1887-1901 | c | plain native probe + compiled/interpret fallthrough (incl. `skip_native` route) | `callmethod:native` / `callmethod:user` | `user` includes calls that later fail with X::Method::NotFound (see overlay note) |
| ~1903-1937 | c | not-found completions (`.?` Nil absorb; propagated-Err peek) | `callmethod:notfound` | overlay subset of `user` |
| ~1938-1976 | d | writeback tails (`drain_and_reconcile_after_cached_call`, hyper rewrap) | (none) | untouched per design decision 2 |

Summary: 45 distinct intercept arm names over ~40 class-(b) arms (the HyperSeq family
alone is 10), 6 class-(a) normalization regions, the class-(c) probe section with 4
outcome kinds, 1 class-(d) tail region. No surprises in kind, one surprise in *shape*:
several arms that look CallMethod-owned are in practice unreachable from this entry
because their receiver shapes (bareword type names, plain variables) compile to
`CallMethodMut` — the sweep confirms this (see below), which means the E6a measurement
for `CallMethodMut` is where those twins' real traffic will show up. Zero counts here
must NOT be read as dead code until the E6a sweep of the mut twin lands.

### Sweep results (2026-08-11, debug build, full `t/` 3014 files + roast S12-attributes/S12-methods/S14-roles, 3075 processes, 0 timeouts)

Outcomes (disjoint): `callmethod:user=13258` (49.2%), `callmethod:native=11794`
(43.8%), `callmethod:intercept=968` (3.6%), `callmethod:accessor=904` (3.4%);
overlay `callmethod:notfound=52` (0.4% of user). Disjoint total 26924.

Sub-slice ordering consequence (decision 3(i)): the `user` and `native` outcomes
dominate roughly equally — the E5b cutover's decision match must get the
user-candidate and native-row paths right first; accessor and the intercept gauntlet
are an order of magnitude smaller.

Intercept arms by count (27 of 45 fired): `nil-absorb=675`, `lazy-first=92`,
`hyperseq-map-grep=38`, `nil-predispatch=33`, `hash-on-list=32`, `proto=27`,
`junction-invocant=10`, `lazy-index-pipe=9`, `who-pseudo-package=6`,
`modifier-star=5`, `lazy-placeholder=5`, `modifier-plus=4`,
`hyperseq-configuration=4`, `hyper-race-config=4`, `junction-args=3`, `emit=3`,
`failure-print-nomatch=3`, `containerref-scalar-meta=2`, `hyperseq-iterator=2`,
`shift-pop=2`, `failure-explode=2`, `so-not-user-bool=2`, `lock-protect-nomatch=1`,
`lazy-cache=1`, `pair-freeze=1`, `return=1`, `modifier-pseudo-error=1`.

Zero-count arms (18): `nativecall`, `exception-str-message`,
`exception-concreteness`, `autoviv-typed-array`, `deprecation-report`,
`lock-protect`, `hyperseq-{hyper,race,is-lazy,name,what,isa,defined}`,
`regex-bool-topic`, `nil-autoviv`, `any-autoviv`, `proxy-subclass-mutate`,
`slip-default`. These are *deletion candidates for a later sweep, not deletions now*
(decision 3(ii)) — and most are explained rather than dead: bareword/variable
receivers route the same shapes through `CallMethodMut`, and this sweep covered
`t/` plus only three roast directories, not the whole whitelist. Re-run the sweep
over whitelisted roast (CI-scale) before proposing any arm removal.

### Measurement slice results — CallMethodDynamic (E5 step 2)

Landed 2026-08-11: instruments the second E5 measurement entry named in step 1's
"still to do" list, `exec_call_method_dynamic_op`
(`src/vm/vm_call_method_mut_ops.rs:30-345`, current revision — grew from the
~250-line estimate in the corrected entry inventory to ~315 lines purely from
this slice's own insertions). Reuses the exact same two generic functions step 1
added (`record_dispatch_entry_outcome`, `record_dispatch_entry_intercept`) with
`entry = "callmethoddynamic"` — no new counter functions. Pure insertions only:
the diff is 70 insertions / 1 "deletion", and that one line is a single-statement
match arm (`Err(e) if is_method_not_found_error(&e) => self.stack.push(Value::NIL)`)
rewrapped in braces so a counter call could precede the identical push — no
branch, condition, or return value changed.

Re-verifying the design doc's inventory-correction note (item 3) against the
current code: that note is about `exec_call_method_dynamic_mut_op` (the *Mut*
twin, `CallMethodDynamicMut`, a separate E6 entry, out of scope here), not this
one. `exec_call_method_dynamic_op` itself does have both a native probe and a
compiled/interpret fallthrough (:310-318) — the design doc's "no native probe
and no compiled-method probe gap" framing for this entry is correct as written;
nothing was stale.

Taxonomy table (line numbers as of this PR's revision; anchor is each
`record_dispatch_entry_*` call site):

| Line range | Class | Arm name / description | Counter key | Notes |
|---|---|---|---|---|
| ~30-56 | — | entry bookkeeping (arg decode, flatten, stack pops) | (none) | stack-underflow errors uninstrumented: internal errors, not dispatch outcomes |
| ~57-68 | a | LazyIoLines force | (none) | |
| ~70-94 | b | `.+`/`.*` all-methods modifiers | `callmethoddynamic:modifier-plus` / `callmethoddynamic:modifier-star` | delegates to `call_method_all_with_fallback` (its own E5 measurement entry, later slice); `.*`'s not-found→empty-list completion is covered by this arm, not `notfound`, matching CallMethod's own convention |
| ~96-107 | b | name-value is a `Sub`/`WeakSub`/`Routine` (`$obj.$coderef(...)`) — invocant bound positionally, dispatched via `vm_call_on_value` | `callmethoddynamic:call-sub-value` | unique to this entry: `CallMethod`'s name is always a literal identifier, never a callable value |
| ~111-116 | b | `.return` control flow | `callmethoddynamic:return` | |
| ~118-182 | b | `.hyper`/`.race` with named-arg validation, creates HyperSeq/RaceSeq | `callmethoddynamic:hyper-race-config` | branch-entry record also covers the two X::Invalid::Value completions, matching CallMethod's convention |
| ~184-192 | a | HyperSeq/RaceSeq receiver unwrap (`is_hyper`, `items_arc`) | (none) | normalization only, no completion |
| ~193-308 | b | HyperSeq/RaceSeq delegate-method dispatch (9 arms) | `callmethoddynamic:hyperseq-{hyper,race,is-lazy,configuration,name,what,defined,map-grep,delegate}` | `map-grep` and the catch-all `delegate` arm each wrap their own inner native/compiled probe, but per CallMethod's own `hyperseq-map-grep` convention the arm name is the single count — no additional `native`/`user` recorded inside |
| ~310-318 | c | plain native probe + compiled/interpret fallthrough | `callmethoddynamic:native` / `callmethoddynamic:user` | `user` includes calls that later fail with X::Method::NotFound (see overlay note below) |
| ~320-343 | c | not-found completions (`.?` Nil absorb; propagated-Err peek) | `callmethoddynamic:notfound` | overlay subset of `user`, same pattern as CallMethod |
| ~344 | d | (none — no writeback tail at this entry; the result is pushed inline at each completion point) | (none) | this entry has no separate writeback-tail region distinct from its outcome completions, unlike `CallMethod`'s dedicated tail (`drain_and_reconcile_after_cached_call`) |

Counting semantics mirror CallMethod exactly: each executed `CallMethodDynamic`
records exactly one of `intercept`/`native`/`user` (no `accessor` outcome exists
at this entry — there is no fast 0-arg public-accessor read probe here, only at
`CallMethod`). `notfound` is an overlay subset of `user`. Disjoint total =
`intercept + native + user`.

Verification (debug build, targeted files rather than a full `t/` sweep — this
entry is far smaller-traffic than `CallMethod` and a handful of files gave a
clean disjoint-sum proof quickly, so the full multi-process `t/`-wide sweep
infrastructure from step 1 was not reused, per the task's own guidance not to
over-invest): ran every `t/*.t` file matching `.$name`/`."$name"` dynamic-call
syntax or a `dynamic`/`dispatch`/`indirect`/`hyper`/`proxy` filename (161
candidates). 5 files actually exercised `CallMethodDynamic` (most `.$name`/`."$"`
call sites on a plain variable receiver compile to `CallMethodDynamicMut`
instead — the same "bareword/variable receiver picks the Mut opcode" pattern
CallMethod's own sweep documented). All 5 are disjoint-and-complete
(`sum(intercept+native+user) == CallMethodDynamic` opcode-histogram count):

| File | CallMethodDynamic (opcode histogram) | Outcome sum | Detail |
|---|---|---|---|
| `t/array-value-path-mutation.t` | 8 | 8 | `user=8` |
| `t/buf-write-native.t` | 5 | 5 | `native=5` |
| `t/dynamic-method-type-object.t` | 4 | 4 | `native=3`, `user=1` (overlay `notfound=1`) |
| `t/format-class.t` | 11 | 11 | `user=11` |
| `t/topic-quoted-method-call.t` | 1 | 1 | `native=1` |

No intercept-arm traffic was observed in this targeted set (`.$coderef(...)`,
`.hyper`/`.race`, HyperSeq/RaceSeq delegation, and the `.*`/`.+` modifiers were
not exercised by these particular files) — those arms remain unmeasured until a
broader sweep (full `t/` + whitelisted roast, CI-scale, as step 1 ran) is done.
This is a smaller/simpler entry than `CallMethod` so that broader sweep is
deferred rather than run locally; the box stays open regardless — the remaining
E5 measurement entries (hyper non-mut paths, `call_method_all_with_fallback`)
and all cutover sub-slices (E5b/E5c/E5d) are still to do.

### Measurement slice results — hyper non-mut paths (E5 step 3)

Landed 2026-08-11: instruments the two remaining opcode entries named in step
1's "still to do" list, `exec_hyper_method_call_op` (`HyperMethodCall`,
`src/vm/vm_hyper_method_ops.rs:446-984`, entry name `hypermethodcall`) and
`exec_hyper_method_call_dynamic_op` (`HyperMethodCallDynamic`, same file
`:1150-1385`, entry name `hypermethodcalldynamic`). Reuses the same two
generic functions from step 1/2, no new counter functions. Pure insertions
only — every new line is a `record_dispatch_entry_outcome`/
`record_dispatch_entry_intercept` call; no branch, condition, guard, or
return value changed. `make test` (full `t/`, 3018 files, 28265 subtests)
passes unchanged after the insertion.

**Granularity differs from `CallMethod`/`CallMethodDynamic` by design.**
Those two entries dispatch exactly one method call per opcode execution, so
`sum(outcomes) == opcode count` was the verification identity. A hyper
opcode instead loops over every element of its target and dispatches once
per element (design decision 4's "per-element probe" — the whole point of
E5c's eventual cutover is to touch only that inner per-element probe pair).
So here `sum(outcomes)` counts **element-level dispatches**, not opcode
executions, and is expected to exceed the `HyperMethodCall`/
`HyperMethodCallDynamic` opcode-histogram count whenever a target has more
than one element (confirmed directly: `t/hyper-nested-itemize.t` executes
`HyperMethodCall` 12 times per the opcode histogram but records 18
`hypermethodcall:*` outcomes, from arrays with >1 element per call). Do not
treat opcode-count parity as the correctness bar for these two entries —
per-file *plausibility* (arm names matching the source's actual `>>`/`».`
usage, no arm firing on a file that doesn't exercise its shape) is the
check used instead, same as the informal spot-checks below.

Taxonomy table — `HyperMethodCall` (line numbers as of this PR's revision):

| Line range | Class | Arm name / description | Counter key | Notes |
|---|---|---|---|---|
| ~481-489 | b (op-level) | metaobject introspector (`.WHAT`/`.WHO`/`.HOW`/`.DEFINITE`/`.WHERE`) applies to the target itself, no per-element loop | `hypermethodcall:target-introspector` | consumes the whole opcode; entire result path elsewhere in this table does not run |
| ~496-511 | b (op-level) | `>>++`/`>>--` on a Bag/Mix/Set (applies to weights, no per-element loop) | `hypermethodcall:quant-postfix` | consumes the whole opcode via `exec_hyper_quant_postfix` |
| ~564-579 (per element) | b | `CALL-ME` on a callable item (`>>.(args)` syntax) | `hypermethodcall:call-me` | |
| ~583-596 (per element) | b | user-defined `postfix:<...>` operator via hyper (excludes builtin `++`/`--`) | `hypermethodcall:user-postfix-op` | function call, not method dispatch |
| ~601-616 (per element) | b | builtin `++`/`--` on a `ContainerRef` element (shared alias, e.g. `@a.grep(...)>>++`) | `hypermethodcall:containerref-postfix` | mutates through the cell in place |
| ~663-696 (per element) | c | modifier `?` (`».?method`) native/user probe | `hypermethodcall:native` / `hypermethodcall:user` | errors swallowed to `Any`; no `notfound` overlay possible here |
| ~697-733 (per element) | c | modifier `+` (`».+method`) native/user probe via `call_method_all_with_temp_target` | `hypermethodcall:native` / `hypermethodcall:user` | one result per MRO candidate |
| ~734-757 (per element) | c | modifier `*` (`».*method`) native/user probe, same helper as `+` but swallows errors to `()` | `hypermethodcall:native` / `hypermethodcall:user` | |
| ~758-775 (per element) | b | hyper subscript with a slice index (`@a>>.[0..2]`, `%h>>.{1,2}`) | `hypermethodcall:subscript-slice` | applies the postcircumfix subscript, not the single-key accessor |
| ~791-806 (per element) | b | non-nodal descend into a nested Iterable/Hash element | `hypermethodcall:descend-recursive` | delegates to `hyper_method_apply_recursive`, itself uninstrumented (recursion-internal probes deliberately out of scope, same convention as `CallMethod`'s `junction-invocant`) |
| ~807-819 (per element) | b | Nil absorbs an undefined method (`Nil.FALLBACK`) | `hypermethodcall:nil-absorb` | |
| ~820-861 (per element) | c | plain (no modifier) native/user probe | `hypermethodcall:native` / `hypermethodcall:user` | includes the resumable-warn carry-through; errors otherwise propagate via `?` (no observable not-found completion at this entry, unlike `CallMethodDynamic`) |
| ~866-984 | d | writeback tails (array/hash/QuantHash rebuild, `write_back_hyper_target_var`, identity-scan overwrite) | (none) | untouched per design decision 2 |

Taxonomy table — `HyperMethodCallDynamic` (line numbers as of this PR's
revision; this entry has no op-level early-return branches — hash-keys
handling and the per-element loop are the only structure):

| Line range | Class | Arm name / description | Counter key | Notes |
|---|---|---|---|---|
| ~1198-1225 (per element) | b | `>>.&callable` where the callable name is nodal (applies at the node level, one call per top-level element) | `hypermethodcalldynamic:callable-nodal` | approximated by name (`is_nodal_list_method`); see the pre-existing `TODO` at this site about a missing `is nodal` trait on user `Sub`s |
| ~1226-1232 (per element) | b | `>>.&callable` where the callable is not nodal (descends recursively) | `hypermethodcalldynamic:callable-descend` | delegates to `hyper_sub_apply_recursive`, uninstrumented internally, same convention as `descend-recursive` above |
| ~1237-1251 (per element) | c | modifier `?` native/user probe (name-value branch) | `hypermethodcalldynamic:native` / `hypermethodcalldynamic:user` | no `skip_native` gate exists at this entry at all — unlike the static `HyperMethodCall`, there is no `has_user_method` check anywhere in the dynamic dispatch path |
| ~1252-1266 (per element) | c | modifier `+` native/user probe via `call_method_all_with_temp_target` | `hypermethodcalldynamic:native` / `hypermethodcalldynamic:user` | |
| ~1267-1287 (per element) | c | modifier `*` native/user probe | `hypermethodcalldynamic:native` / `hypermethodcalldynamic:user` | |
| ~1288-1301 (per element) | c | plain (no modifier) native/user probe | `hypermethodcalldynamic:native` / `hypermethodcalldynamic:user` | any error (including not-found) propagates via `?`, same as the static entry's plain-probe arm |
| ~1302-1341 | d | writeback tails (array rebuild, QuantHash/Hash reassembly) | (none) | untouched |

**Real finding from the classification pass, not just a table gap**: unlike
`exec_hyper_method_call_op`, `exec_hyper_method_call_dynamic_op` has no
`skip_native`/`has_user_method` gate anywhere — every per-element dispatch
tries `try_native_method` first regardless of whether the item's class
defines a same-named user method. This is exactly inventory correction 4 in
the design doc's "Facts that shape the cutover" section (which named this
gap for `exec_hyper_method_call_dynamic_op` vs its static twin). It was not
re-verified against raku here (out of scope for a measurement-only slice —
V1 in the doc's "Verification items" still needs to raku-verify it before
the E6/E5c cutover fixes it by construction).

### Sweep results

Full `t/` sweep (debug build, all 3018 files, `MUTSU_VM_STATS=1`, one
process per file): 50 files recorded at least one `hypermethodcall*`
outcome. Aggregated element-level outcome totals:

| Key | Count |
|---|---|
| `hypermethodcall:native` | 575 |
| `hypermethodcall:user` | 191 |
| `hypermethodcall:intercept` | 99 |
| `hypermethodcalldynamic:intercept` | 65 |
| `hypermethodcalldynamic:callable-descend` | 57 |
| `hypermethodcall:descend-recursive` | 31 |
| `hypermethodcall:user-postfix-op` | 25 |
| `hypermethodcall:subscript-slice` | 10 |
| `hypermethodcall:containerref-postfix` | 10 |
| `hypermethodcalldynamic:callable-nodal` | 8 |
| `hypermethodcall:target-introspector` | 7 |
| `hypermethodcall:quant-postfix` | 7 |
| `hypermethodcall:call-me` | 5 |
| `hypermethodcall:nil-absorb` | 4 |

`hypermethodcalldynamic:native` and `hypermethodcalldynamic:user` were
**zero across the entire local `t/` sweep** — every `t/*.t` exercise of
`HyperMethodCallDynamic` went through the `>>.&callable` branch
(`callable-nodal`/`callable-descend`) or an intercept, never the plain
`».method`/`».$name(...)` string-dispatch branch. This is a coverage gap in
`t/`, not evidence the branch is dead: three whitelisted roast files use
`»."name"`/`»."$name"` syntax (`roast/S03-metaops/hyper.t`,
`roast/S12-methods/parallel-dispatch.t`, `roast/S05-mass/properties-script.t`),
and running the first two directly (`MUTSU_FUDGE=1 MUTSU_VM_STATS=1`, debug
build) confirms real traffic there: `hyper.t` records
`hypermethodcalldynamic:native=8`, `parallel-dispatch.t` records
`hypermethodcalldynamic:user=12` (both files' TAP output unaffected —
all `ok`, same as pre-change). Sub-slice ordering (decision 3(i)):
`hypermethodcall`'s `native`/`user` dominate (575/191, ~75%/25% of its
disjoint element dispatches), so E5c's decision-match conversion of
`HyperMethodCall`'s plain-probe arm is the highest-value single change
in this pair; the dynamic entry's real native/user traffic requires the
roast corpus, not `t/`, as its parity set.

**What's left for E5 (per design decision 4's slicing)**: the last
measurement entry, `call_method_all_with_fallback`
(`vm_call_helpers.rs:303`, backing the `.+`/`.*` all-methods modifiers at
the `CallMethod`/`CallMethodDynamic` entries — already visible as zero-detail
intercept arms `modifier-plus`/`modifier-star` in steps 1/2's own tables).
Once that lands, all four E5 measurement sub-slices are done and E5b
(`CallMethod` cutover) can start.

### Measurement slice results — call_method_all_with_fallback (E5 step 4)

Landed 2026-08-11: instruments `call_method_all_with_fallback`
(`src/vm/vm_call_helpers.rs:309-331`, entry name `callmethodallfallback`),
the last of the four E5 measurement entries named in design decision 4. Pure
insertion — one `record_dispatch_entry_outcome` call before the native early
return, one before the `call_method_all_with_values` fallback; no branch,
condition, or return value changed. `make test` (full `t/`, 3018 files,
28265 subtests) passes unchanged.

Unlike the opcode entries in steps 1-3, this is a single **shared helper
function**, not an opcode handler — it has no opcode histogram to check
disjoint-sum completeness against, and no receiver-normalization/intercept
gauntlet of its own (its whole body is the two-arm `native`/`user` probe
already shown above). Its taxonomy is therefore trivial (2 outcomes, 0
intercept arms) but its *callers* are not: grep confirms 6 call sites
across 5 files — `CallMethod`'s own `modifier-plus`/`modifier-star`
intercept arms (measured at the caller in step 1, already known
zero-detail there), `CallMethodMut` (2 call sites,
`vm_call_method_mut_ops.rs:76/85`), `CallMethodDynamicMut`
(`vm_call_method_mut_ops.rs:381/386`), and three call sites unrelated to
the `.+`/`.*` modifiers entirely: `vm_exec_dispatch.rs:2616` (a `.cache`
coercion), `vm_var_assign_coerce.rs:341` (a `.Map` coercion), and
`vm_var_assign_set_local.rs:828` (a cached scalar-accessor probe). The E6
measurement slice for `CallMethodMut`/`CallMethodDynamicMut` (E6a) will
re-encounter this same helper as their own `.+`/`.*` outcome source — this
slice measures the helper itself once, not per-caller, since it is exactly
the entry the design doc's own inventory names standalone.

Full `t/` sweep (3018 files): 7 files recorded any outcome —
`callmethodallfallback:user=22`, `callmethodallfallback:native=3`. All 7
hits were confirmed by inspection to be genuine `.+`/`.*` MRO-walk tests on
*variable* receivers (`$b.*tag`, `$obj.+m`, `.VAR.+name`) — i.e. they
compile to `CallMethodMut`/`CallMethodDynamicMut`, not `CallMethod`,
matching the "bareword/variable receiver picks the Mut opcode" pattern
steps 1/2 already documented. `t/builtin-mro-all-candidates.t` is the only
file with `native` traffic (3 of 4 hits): a `.*`/`.+` walk over a built-in
type, exercising the `builtin_mro_method_candidate_count` multiplication
path. `user` dominates overall (22 vs 3) but the sample is small (25 total
hits) — not enough to draw a sub-slice-ordering conclusion on its own; this
helper's real traffic profile will be clearer once E6a's `CallMethodMut`
sweep runs (its own `.+`/`.*` arms are the majority caller of this helper,
per the call-site count above).

**All four E5 measurement sub-slices are now done** (steps 1-4: `CallMethod`,
`CallMethodDynamic`, the two hyper non-mut opcodes, and this shared helper).
Per design decision 4's slicing, E5b (`CallMethod` probe-section cutover to
the E4 resolver decision) can start next.

### E5b step 1: shadow-verifying the `Native` candidate at CallMethod itself — a real divergence found, NOT safe to consume yet

Design decision 1's cutover shape needs `resolve_dispatch(&receiver, method_sym,
shape)` to answer "native row or user candidate?" for `CallMethod`'s own
highest-traffic arm (the plain probe at the end of its cascade, `native=43.8%`/
`user=49.2%` per step 1's sweep). Before writing that function, this step
reused the *existing*, already-landed E4b step 9 machinery
(`Interpreter::shadow_check_native_row_candidate`, `src/runtime/resolution_sequence.rs`)
and called it — unmodified, no new counter function — from `CallMethod`'s own
plain-probe arm (`vm_call_method_ops.rs`, both the `native`-outcome and
`user`-outcome branches), passing the already-computed `native_result.is_some()`
as `real_served` so the cascade is never invoked twice. Pure insertion, zero
behavior change (`cargo build`, `cargo clippy -- -D warnings`, `cargo fmt`, and
the full local `prove -j4 t/` suite — 3018 files, 28265 subtests — all green,
identical to before the insertion).

**Finding: the `Native` candidate does NOT reliably predict `try_native_method`'s
real outcome at this call site.** Full `t/` sweep (3018 files, `MUTSU_VM_STATS=1`):
39558 shadow checks, ~965 mismatches (~2.4%), spread across 253 distinct files —
both directions occur in comparable volume (row says servable but the cascade
declined: ~545; cascade served but no row exists: ~409), and no single method
dominates (`gist`/`raku` are the largest single buckets at roughly 120-190
combined mismatches each, but `join`/`sprintf`/`comb`/`DEFINITE`/`head`/`Int`/
`List`/`substr`/`Str`/`split`/`contains`/`AT-KEY`/`EXISTS-POS`/`EXISTS-KEY`/
`throw`/... all contribute tens of hits each). This is qualitatively different
from E4b step 9's own report of the *same* shadow-check function at its
original call site (`call_method_with_values`), which found essentially zero
mismatches — that site is the interpreter's slow-path fallback, reached far
less often and with a narrower receiver/method mix than `CallMethod`'s own
hot-path probe, so its clean result was a **sampling artifact of where it was
placed**, not evidence the underlying `native_row_servable` predicate
(`native_method_row_table.rs` + arity/definite gating) is actually sound
across the full traffic `CallMethod` sees. The two concrete mismatch shapes,
by direct inspection:

- **`real=false shadow=true`** (row claims servable, cascade declined): e.g.
  `t/anon-sub-name-gist.t`'s `anon sub foo {...}.gist`/`.raku` — a `Sub` value
  whose `gist`/`raku` rendering the row table's generic `"Any"`-owner row
  predicts as servable, but `try_native_method`'s actual dispatch for a `Sub`
  receiver declines (returns `None`) so the call falls through to the
  interpreted/compiled path instead, which has its own bespoke Sub-rendering
  logic not modeled by the row catalog at all. `native_row_servable` checks
  only `(owner, method, arity, definite)` — it has no notion of "this
  receiver's *concrete value shape* makes the generic row inapplicable",
  the same class of gap E4b step 2 already named for
  `should_bypass_native_fastpath`'s category-1 gates ("row presence/absence
  is the wrong axis... what matters is whether the cascade itself would
  misbehave if reached").
- **`real=true shadow=false`** (cascade served, no row at all): e.g. `DEFINITE`
  at 0 arity (the very first mismatch found, `t/hyper-nodality.t`) — a
  pseudo-method handled directly inside `try_native_method`'s own dispatch
  (or a pre/post step around the row cascade) without ever being registered
  as a `native_method_row_table.rs` entry, so `native_row_servable` can never
  see it regardless of receiver shape.

**Consequence for E5b's real cutover: do not build `resolve_dispatch`'s
"native or user" branch purely from `native_row_servable`/the `Native`
candidate — it will silently mis-route ~2.4% of `CallMethod`'s highest-traffic
arm.** This is a genuine blocker finding, not a design-doc typo: design
decision 4's `Native` candidate needs either (a) a per-method-shape refinement
so it stops over/under-claiming for cases like `Sub.gist`/`DEFINITE`, mirroring
the E4b category-1 audit's granularity, or (b) E5b's decision match keeping a
"try the real cascade, and only consult the `Native` candidate as a routing
*hint*, never as ground truth" shape — i.e. the actual invocation stays
`try_native_method` itself (self-guarding, returns `None` on no match) rather
than a resolver decision that skips calling it. Option (b) is cheaper and
matches how `is_native_method`/`NativeCallBinding` was already found not worth
routing through the resolver at the E4b step 12 call site ("no gain over a
direct call") — the same reasoning likely applies here too, but has NOT been
verified for the User-vs-Native ordering question this finding is really about
(does `Native` ever need to *outrank* a matching `User` candidate, or does
`CallMethod`'s pre-existing `skip_native`/`has_user_method` gate already
settle that before the plain-probe arm is reached? — open, next step).

No code was fixed here (this is a measurement/shadow-verify slice per the
project's own established methodology, same as E4a/E4b); the mismatch
buckets are the review artifact, not a to-do list to clear item-by-item.
**Next E5b step**: before writing any real `resolve_dispatch` consumption,
decide between options (a)/(b) above — likely by checking whether
`CallMethod`'s existing `skip_native` gate (computed earlier in the function,
the class-c "skip_native gate computation" row in step 1's own taxonomy
table) already prevents `Native` from ever needing to outrank a `User`
candidate at this specific arm, which would make option (b) sufficient and
this whole `Native`-candidate refinement moot for `CallMethod` specifically
(though not for other E5/E6 entries that reach this arm's twin without an
equivalent gate).

### E5b step 2: the top-level `skip_native` gate does NOT settle the ordering question -- the real guarantee is ~22 scattered per-shape bypass checks inside `try_native_method_raw` itself; option (b) confirmed, option (a) is now actively discouraged

Answers step 1's open next question directly, by raku-verified experiment plus code
inspection (no code change; this step is analysis only, like several prior scoping
slices in this campaign).

**The top-level gate does not cover every receiver shape that can carry a user
override.** `exec_call_method_op_impl`'s `skip_native`/`has_user_method` computation
(`vm_call_method_ops.rs:964-980`) only extracts a `class_name` to check for
`ValueView::Instance`/`ValueView::Package` receivers -- a `Mixin`-shaped receiver
(`"hello" but SomeRole`, the standard `but`-mixin idiom) falls through the `_ => None`
arm and never sets `skip_native`. So the top-level gate, by itself, does **not**
guarantee `User` outranks `Native` for a mixed-in method whose name collides with a
native row.

**Yet the actual behavior is already correct.** Verified directly:

```raku
role Loud { method uc { "MIXED-UC" } }
my $s = "hello" but Loud;
say $s.uc;   # raku: MIXED-UC, mutsu: MIXED-UC (confirmed, tmp/mixin-native-outrank.raku)
```

The reason is NOT the top-level gate -- it is a *second*, independent bypass check
living inside `try_native_method_raw` itself (`vm_native_dispatch.rs:164-166`):

```rust
// Mixin role method bypass
if self.mixin_role_has_method(target, &method_name) {
    return None;
}
```

This is one of **22 distinct `return None` bypass sites in `vm_native_dispatch.rs`
alone** (grep count, this file only -- `builtins/` and the row-cascade modules likely
add more), several of which exist specifically to decline native dispatch when a user
override applies to a receiver *shape* the top-level `skip_native` gate cannot see:
`mixin_role_has_method` (Mixin), `has_user_method("Match", ...)` +
`exception_render_needs_interpreter` (lazy Match render overrides), the parallel
`has_user_method(&cn, ...)` + `exception_render_needs_interpreter` block for realized
Instance/Exception receivers (lines 239-263, a second, finer-grained check than the
top-level gate's own Instance handling -- it additionally distinguishes "pure render"
methods from others and checks a `Bridge` method), plus Seq-deferred-iterator and
Buf-write-method bypasses unrelated to user overrides at all.

**The augment-collision angle is not a real threat either, but for a different
reason: raku itself forbids it.** Tried to construct a case where a *plain* builtin
value (not mixin) has a colliding augmented method:

```raku
augment class Str { method uc { "AUGMENTED-UC" } }
# raku: ===SORRY!=== Package 'Str' already has a method 'uc' (did you mean to declare a multi method?)
augment class Str { multi method uc { "AUGMENTED-UC" } }
# raku: Ambiguous call to 'uc(Str: )'; ... (real multi-dispatch ambiguity, a separate unimplemented feature)
```

Both forms raku rejects before the ordering question even arises (mutsu currently
does neither -- it silently native-dispatches to the builtin `uc`, `HELLO` -- a
latent gap, but a *compile-time-redeclaration/multi-ambiguity-detection* gap, not an
E5b dispatch-ordering gap; out of scope here, not filed separately since augmenting
an already-declared core method without `multi` is not a legitimate program shape
worth a dedicated ticket by itself).

**Conclusion, closing step 1's open question:**

1. **Option (b) is confirmed correct** -- and more than "cheaper": it is *already
   the only mechanism keeping today's dispatch correct*. The top-level `skip_native`
   gate is a fast common-case bypass (avoids the `try_native_method_raw` call
   entirely for the frequent Instance/Package-with-user-override case), not the
   safety mechanism itself.
2. **Option (a) (refining `native_row_servable` to be shape-aware) is now actively
   discouraged, not just unnecessary.** To make the `Native` candidate alone safe to
   route on, it would have to absorb the same ~22-and-growing scattered per-shape
   checks `try_native_method_raw` already encodes -- at which point the resolver
   candidate *is* a reimplementation of the cascade's own guard logic, with two
   copies to keep in sync instead of one. That is strictly worse than calling the
   real function.
3. **This generalizes past `CallMethod`.** Every E5/E6/E7 entry that currently calls
   `try_native_method`/`try_native_method_raw` inherits the same guarantee from the
   same shared function -- the `Native` candidate from `resolve_sequence` should be
   treated as **measurement/hint-only, never a routing decision, at every entry**,
   not re-litigated per entry. Design decision 1's "decision match" framing should be
   read as applying to the `User`/`NativeCallBinding` candidates (which E4a's
   `shadow_check_resolver` already proved trustworthy) -- the native probe stays a
   direct, self-guarding call in its existing cascade position at every cutover,
   E5b through E7.
4. **Left open for the actual E5b cutover PR** (not resolved by this analysis step):
   whether the `User` candidate can cleanly replace any part of
   `try_compiled_method_or_interpret_sym`'s own dispatch. Inspection shows that
   function (`vm_call_method_compiled_interpret.rs`) is not a simple MRO-walk call
   sitting behind the native probe -- it carries its own substantial native-ish
   interceptor cascade first (default construction, Buf/Blob construction, Seq
   reification, ...) before reaching the actual compiled/interpreted method lookup.
   Scoping how much of *that* is safe to fold into a decision match is real,
   unstarted work for the cutover PR itself, separate from the native-ordering
   question this step closes.

### E5b step 3: shadow-verifying the `User` candidate at `try_compiled_method_or_interpret`'s own resolution point -- confirms E4a's trust extends to the hottest, previously-unchecked call site

Closes part of step 2's item 4 (the "`User` candidate" half, not the interceptor-cascade
half, which is still open). Inspection of `vm_call_method_compiled_interpret.rs`'s
Instance/Package resolution block (the code the ADR's step-2 note calls "the actual
compiled/interpreted method lookup") shows it duplicates `resolve_method_cached`'s exact
three-tier cache (monomorphic inline cache -> non-multi `HashMap` -> sound multi-resolution
cache) and its two `resolve_method_with_owner_invocant` calls almost verbatim -- but as an
inlined copy, not a shared call. **`resolve_method_cached` already carries E4a's
`shadow_check_resolver` probe at both of its own resolve call sites; this inlined duplicate,
reached from `CallMethod` -- the higher-traffic non-mut opcode `resolve_method_cached`'s own
caller (`vm_call_method_compiled_mut.rs`, the Mut path) does not serve -- carried no shadow
probe at all.** So E4a's "resolver is trustworthy" conclusion, to date, had only ever been
measured on the Mut path; the busier non-mut path was untested by construction.

**Change (zero behavior change, pure instrumentation):** added the same
`self.shadow_check_resolver(...)` call `resolve_method_cached` already makes, at the
equivalent two points inside `try_compiled_method_or_interpret_inner`'s resolution block
(`vm_call_method_compiled_interpret.rs`) -- the multi-resolve-cache-miss branch (site
`try_compiled_method_or_interpret:multi`) and the fresh-resolve branch (site
`try_compiled_method_or_interpret:fresh`). Both pass the `Option<(Symbol, MethodDef)>`
already computed by the existing `resolve_method_with_owner_invocant` call, before it is
Arc-wrapped and cached -- identical shape to the two call sites in `resolve_method_cached`.

**Sweep (debug build, `MUTSU_VM_STATS=1`, full `t/*.t`, 3022 files, `-P8`):** 15,085 total
`resolver_shadow_checks` across both `resolve_method_cached`'s and this box's two new sites,
25 mismatches (0.166%) -- same order of magnitude as E4a's original near-zero baseline. Every
mismatch decomposed by the existing per-site breakdown (`resolver-shadow mismatches by site`)
to the **single already-documented, already-accepted divergence class** from the
`resolution_sequence` module doc: a non-multi candidate whose own signature does not match the
call (e.g. `method assign-rw($a is rw)` invoked with a literal argument) -- the real resolver
returns that sole visible candidate anyway (Raku resolves a non-multi method by name alone,
raising the signature-bind error only after resolution), while the E4a shadow winner correctly
answers `None` since it only ranks candidates that already passed
`method_args_match_for_invocant`. All 25 mismatches showed `shadow=None` opposite a `real=Some(...)`
non-multi candidate; none were the reverse direction, and none touched a new method/class pattern
not already named in the module doc. `make test`-equivalent local suite (`prove -j8 t/*.t`, 3022
files / 28,279 tests) green, unchanged.

**Conclusion:** E4a's resolver is now empirically confirmed trustworthy at the actual
highest-traffic non-mut call site, not just inferred by construction from the Mut-path sweep --
closing the "`User` candidate" half of step 2's open item 4. **Still open, unstarted:** whether
any of the pre-lookup interceptor cascade (`try_compiled_method_or_interpret_inner`'s ~430
lines before this resolution block -- Seq reification, the seven `.new`/`bless` native
construction forks, the IO::Handle/IO::Path native-method chain, MOP pseudo-methods, private
methods, `^`-metamethods) can be folded into a decision match, or whether (per step 2's general
conclusion about the `Native` candidate) each must stay a direct, self-guarding pre-check for
the same reason the native probe does -- most of them already gate on `has_user_method`/
`is_native_method` internally, which is exactly the per-shape-check pattern step 2 found
irreplaceable for `Native`. That inventory and classification is the next E5b sub-slice.

### E5b step 4: inventory and classification of the pre-lookup interceptor cascade -- nothing folds into a decision match, but the resolution block itself dedups to a direct `resolve_method_cached` call, closing the User-candidate cutover

Closes step 3's open item: whether any of `try_compiled_method_or_interpret_inner`'s ~430-line
pre-lookup cascade (Seq reification, the ten `.new`/`bless`/class-method native construction
forks, the IO::Handle/IO::Path Instance chain, MOP pseudo-methods, private methods,
`^`-metamethods) can fold into a `resolve_sequence`-style decision match.

**Self-guarding inventory, the ten `.new`/`bless`/class-method forks (item numbering matches the
file's top-to-bottom order):**

| Fork | Call-site guard | Internal guard | Guard mechanism |
|---|---|---|---|
| Native default `new` | none visible | yes | `try_native_default_construct` -> `native_ctor_plan(..).eligible` -> `is_native_default_constructible` (`src/runtime/methods_object.rs:95-96`) checks `!class_def.methods.contains_key("new")` |
| Native builtin `new` (Buf/Blob/Version/...) | yes | n/a | `vm_call_method_compiled_interpret.rs:99`: `!self.has_user_method(&class_name.resolve(), "new")` |
| Native QuantHash `new` (Set/Bag/Mix/...) | yes | n/a | `vm_call_method_compiled_interpret.rs:113`: `!self.user_declared_classes.contains(..)` |
| Native aggregate `new` (Array/List/Hash/Map) | yes | n/a | same `user_declared_classes` pattern, `vm_call_method_compiled_interpret.rs:126` |
| Native IO::Path family `new` | none | none | `try_native_io_path_construct` (`methods_object_native_ctors_io.rs:6-38`) gates only on `is_io_path_lexical_class` -- a fixed-name-list match, no user-method check anywhere in the chain |
| Native `Failure.new` | none (`class_name == "Failure"`) | none | `build_native_failure_value` (`methods_object_native_ctors_misc.rs:230`) reads only args/`$!`/MRO, no user-method check |
| Native `Seq.new` | none (`class_name == "Seq"`) | none | `try_native_seq_construct` (`methods_object_native_ctors_misc.rs:168-228`) -- pure iterator registration, no user-method check |
| Native `IO::Socket::INET.new` | none (`class_name == "IO::Socket::INET"`) | none | `dispatch_socket_inet_new` (`methods_collection_ops/socket_inet_proc.rs:10`) -- pure arg-parse + bind/connect, no user-method check |
| Native `bless` | via `loan_env!` | yes | `try_native_bless` (`methods_dispatch_new.rs:600`): `if self.native_ctor_plan(class_name).has_custom_bless { return None; }` |
| Native builtin class method | none | none | `try_native_builtin_class_method` (`methods_object_native_ctors_io.rs:487-501`) currently handles only `Instant.from-posix`, no user-method check |

Five forks (IO::Path family, `Failure`, `Seq`, `IO::Socket::INET`, builtin class method) have no
`has_user_method`/`user_declared_classes` guard anywhere in the chain -- guarded only by exact
class-name equality. Checked whether that is a real gap, raku-first, then on mutsu
(`cargo build`, `target/debug/mutsu`):

```raku
class MySeq is Seq { method new(*%a) { "USER-SUBCLASS-OVERRIDE" } }
say MySeq.new;   # raku: USER-SUBCLASS-OVERRIDE, mutsu: USER-SUBCLASS-OVERRIDE (subclassing is fine)

use MONKEY-TYPING;
augment class Seq { method new(*%a) { "USER-OVERRIDE" } }
# raku: ===SORRY!=== Package 'Seq' already has a method 'new' (did you mean to declare a multi method?)
```

Identical redeclaration error from raku for `IO::Path`, `Failure`, `Instant` (`from-posix`);
`IO::Socket::INET` rejects `augment` outright (`is a builtin type, not an external module`). The
`multi method new` dodge fails too, with `X::Multi::Ambiguous` at the call site. mutsu, however,
silently accepts the illegal `augment` and the native fork still wins (verified for all five --
e.g. `mutsu -e 'use MONKEY-TYPING; augment class Seq { method new(*%a) {"USER-OVERRIDE"} }; say
Seq.new;'` prints `()`, not `USER-OVERRIDE`). **This is not a new E5b gap** -- it is the same
pre-existing bug class step 2 already found for `Str.uc` and explicitly declined to file
separately ("augmenting an already-declared core method without `multi` is not a legitimate
program shape worth a dedicated ticket by itself"). The root cause is mutsu's missing
compile-time redeclaration/multi-ambiguity detection for `augment`, not a dispatch-ordering
defect in these five forks -- each is unreachable via any *legal* raku program that collides
with it. If that redeclaration-detection gap is ever closed, all five forks become moot
automatically (the illegal program becomes a compile error before reaching them), so no
independent action is needed on the forks themselves. (Aside, unrelated to this finding:
`class MyInstant is Instant {...}` itself fails in mutsu today -- `'MyInstant' cannot inherit
from 'Instant' because it is unknown` -- a separate, narrower limitation that makes the builtin
class method fork's gap moot for `Instant` specifically regardless of the augment question.)

**Items 12-15 (IO::Handle/IO::Path Instance chain, MOP pseudo-methods, private methods,
`^`-metamethods): all four must stay direct pre-checks, for four different reasons:**

1. **IO::Handle/IO::Path Instance chain** (~10 stacked probes, `vm_call_method_compiled_interpret.rs:213-361`):
   each is its own shape-specific self-guarding check. Folding them into the resolver would mean
   reimplementing the same shape catalog inside `resolve_sequence` -- step 2's "two copies to
   keep in sync instead of one" trap, applied to a second cascade.
2. **MOP pseudo-methods** (`DEFINITE`/`WHAT`/`WHO`/`HOW`/`WHY`/`WHICH`/`WHERE`/`VAR`, lines
   365-371): not a dispatch probe at all -- a class-(b) method-identity intercept per design
   decision 2's taxonomy, matching on method name alone regardless of receiver. The design doc
   already assigns class-(b) intercepts to stay put through E5/E6 (moving them is F-phase
   cleanup).
3. **Private methods** (lines 374-455): not part of `resolve_sequence`'s public-method walk at
   all -- `resolve_private_method_for_vm` is a wholly separate visibility-scoped tier, and the
   design doc's own E7 slicing already assigns private-method dispatch's fold-in to E7, not E5b.
4. **`^`-metamethods** (lines 458-476): closest to foldable -- its guard
   (`self.has_user_method(cn, method)`) is literally the same predicate a resolver "does a User
   candidate exist" answer would give -- but the invocation shape (`how_args = [target, ...args]`,
   threading `target` as an explicit leading positional) is metamethod-specific calling
   convention the decision match's existing "user candidate -> compiled/interpret path" arm
   cannot drive as-is.

**Conclusion: none of the pre-lookup cascade folds into a decision match.** Every `.new`/`bless`
fork is not a guard-then-dispatch pair but a self-contained construction routine with side
effects beyond ordinary method dispatch (registry mutation, deferred-iterator table
registration, real socket I/O, `$!`-env reads). Per step 2's reasoning, routing these through a
resolver decision would mean either reimplementing each one's side-effecting body inside the
resolver (a regression) or having the resolver answer only the guard question and still call the
same direct function (no simpler than today). All items 2-15 stay direct, self-guarding
pre-checks, exactly like the `Native` candidate at `CallMethod`.

**But the actual resolution block this cascade guards *does* cut over.** Step 3 already showed
this function's Instance/Package resolution block (`vm_call_method_compiled_interpret.rs:554-644`,
pre-cutover) was an inlined duplicate of `resolve_method_cached`'s exact three-tier cache and its
two `resolve_method_with_owner_invocant` calls, and shadow-verified it trustworthy (15,085 checks,
0.166% mismatches, the single already-documented divergence class). Since both blocks read and
write the same instance-level caches (`last_method_resolve`/`method_resolve_cache`/
`multi_resolve_cache`), replacing the ~90-line duplicate with a direct
`self.resolve_method_cached(cn, method, class_sym, method_sym, &args, &target)` call is a pure
dedup with no behavior change -- not a new decision match, but the concrete cutover step 2's
open item 4 was asking about: "whether the `User` candidate can cleanly replace any part of
`try_compiled_method_or_interpret_sym`'s own dispatch." It can, exactly at this one block. Landed
together with this step's analysis (`adr0019-e5b-step4-callmethod-resolve-dedup` branch).
Verified: `cargo test --lib` (779 tests), full local suite (`prove -e scripts/run-t-test.sh t/`,
3022 files / 28,279 tests) green; `cargo clippy -- -D warnings` clean.

**E5b is now closed at `CallMethod`'s own entry point.** The `Native` candidate stays a direct
probe (step 2), the `User` candidate resolution is deduped onto the shared cached resolver (this
step), and the surrounding interceptor cascade stays as direct self-guarding pre-checks (this
step). What is left for E5c/E5d is the two `CallMethodDynamic`/hyper entries measured in E5 steps
2-3, not further work on `CallMethod` itself.

### E5c, part 1: `CallMethodDynamic` -- already closed by inheritance from E5b, no code change

Per design decision 4's slicing, E5c covers `CallMethodDynamic` plus the two hyper entries' per-
element probe. This is part 1 (the `CallMethodDynamic` opcode itself); the hyper entries are a
separate part (below).

`exec_call_method_dynamic_op` (`src/vm/vm_call_method_mut_ops.rs:30-345`) was already fully
taxonomized by E5 step 2's measurement slice (see that section's table above) -- this pass turned
that table into the per-arm numbered classification design decision 2 asks for, and checked
whether the general-case fallthrough is genuinely already in target end-state shape rather than
assuming it from the table alone.

**Every one of the 14 named intercept arms is class (a) or (b)**: LazyIoLines force (a); `.+`/`.*`
modifiers, `$obj.$coderef(...)` (unique to this entry -- the name-value is itself a `Sub`/
`WeakSub`/`Routine`, bypassing method lookup via `vm_call_on_value` entirely), `.return`,
`.hyper`/`.race` config, and the 9 HyperSeq/RaceSeq delegate arms (all (b) -- method-identity
intercepts, receiver-shape-independent for the 7 pure ones, wrapping their own inner (c) probe
pair for `map`/`grep`/the catch-all, matching `CallMethod`'s own established convention for
identically-shaped wrapped arms). No arm needed reclassification against the E5 step 2 table.

**The general-case fallthrough (`:310-318` current revision) is the target decision-match shape
already, not a duplicate to converge**:

```rust
if let Some(native_result) =
    self.try_native_method(&target, Symbol::intern(&method), &args)
{
    /* native */
} else {
    /* user */
    self.try_compiled_method_or_interpret(target, &method, args)
}
```

`grep -n "cache\|resolve_method" src/vm/vm_call_method_mut_ops.rs` confirms there is no inline
cache and no inlined resolution logic anywhere in this file between the native probe and this
call -- unlike `CallMethod`'s own entry point (E5b step 3/4's find), this entry never inlined a
duplicate of `resolve_method_cached`'s three-tier cache; it always called the shared
`try_compiled_method_or_interpret` function directly. Since that function is the exact one E5b
step 3 shadow-verified and step 4 deduped onto `resolve_method_cached`, `CallMethodDynamic`'s
`User` candidate inherited E5b's closure for free -- there was never a second implementation here
to shadow-check or dedup. The same 2-line idiom repeats identically inside the two HyperSeq
delegate arms (`hyperseq-map-grep`/the catch-all) against an `array_target` receiver -- also
already end-state shape, not a duplicate.

**No dedup opportunity found**, and this is a structurally different situation from E5b step 4:
that step's find was two *independent implementations* of the same cache (one inlined, one
shared) that had to be shadow-verified before converging. Nothing like that exists here -- every
dispatch-probe call site in this entry already calls the same shared `try_native_method`/
`try_compiled_method_or_interpret` pair directly, so there is no second implementation to
converge onto the first. (The 2-line idiom's 3x textual repetition inside one function is a
DRY/line-count nit at best -- all three sites already call the identical functions in the
identical order, so there is nothing to drift; not treated as blocking closure.)

**`CallMethodDynamicMut` (E6-scoped) reconfirmed unchanged**: still reaches the interpreter with
no native-method probe and no compiled-method probe at all (`vm_call_method_mut_ops.rs:347-433`)
-- after `.+`/`.*` handling and the `call-sub-value` branch, the only native-ish check is
`try_native_buf_mut` (Buf write-method fast path only), and everything else falls straight
through to `vm_call_method_mut_with_values` behind a pre-existing
`// TODO: compile to bytecode -- generic mut method fork (ledger §1).` comment. Matches the design
doc's inventory correction 3 exactly; correctly out of scope for E5c (it is E6's job).

**Real finding, out of scope for this campaign**: raku-verifying representative dynamic-call
shapes turned up a genuine, pre-existing correctness gap unrelated to native-vs-user dispatch
ordering -- `.$name` (unquoted) should require `$name` to be Callable/type-object/`CALL-ME`-able
(raku: `No such method 'CALL-ME' for string 'uc'` for a bare-string `$name`), but mutsu's
`dynamic_method_name` (`vm_call_method_mut_ops.rs:23-28`) accepts any value via
`.to_string_value()`, so `.$m()` and `."$m"()` compile to the identical AST and behave
identically. Filed as `todo/tickets/dollar-dot-dynamic-method-name-should-require-callable.md`,
not fixed here.

**Conclusion: E5c part 1 (`CallMethodDynamic`) is closed, docs-only, no code change.** The `Native`
candidate is a direct self-guarding probe at every dispatch point in this entry (generalizing E5b
step 2's rule), and the `User` candidate already routes through the exact function E5b closed --
this entry inherited that closure automatically, with nothing local to shadow-check or dedup.

### E5c, part 2: the hyper entries' per-element probe -- raku-verified, no live divergence found; downgrades inventory correction 4 from "must fix" to "redundant gate", but surfaces an unrelated real bug

E5 step 3's measurement slice already flagged (its "real finding" note, reproduced from the
design doc's own inventory correction 4) that `exec_hyper_method_call_dynamic_op`
(`HyperMethodCallDynamic`) has **no `skip_native`/`has_user_method` gate anywhere** in its
per-element probe, unlike its static twin `exec_hyper_method_call_op` (`HyperMethodCall`), which
computes a per-element `skip_native` from `has_user_method(class_name, method)`
(`vm_hyper_method_ops.rs:643-670`) before its four modifier-keyed native/user probe arms. That
note left it explicitly unverified: "V1 in the doc's Verification items still needs to
raku-verify it before the E6/E5c cutover fixes it by construction."

Raku-verified now, with three targeted collision attempts (Instance method override colliding
with a name `try_native_method` could plausibly intercept, a `but`-mixin role override, per E5b
step 2's own precedent for finding this class of gap): a user Instance overriding `.reverse`,
`.gist`, `.Str`, `.raku`, `.perl`, and a `but Loud { method uc {...} }` string mixin, all
dispatched via `@a»."name"()` (the dynamic per-element probe's plain string-dispatch branch) --
**every one matched raku exactly**, no divergence found. This means, per the same generalization
step 2 already established for `CallMethod`'s top-level `skip_native` gate: `try_native_method`/
`try_native_method_raw`'s own internal self-guards (the `mixin_role_has_method` bypass, the
`render_overridden` check for `gist`/`Str`/`Stringy`/`raku`/`perl`, the `is_native_method`
Instance check, ...) already provide the real safety net *inside the shared function itself*,
independent of whether the caller computes an outer `skip_native` gate first. `HyperMethodCall`'s
own outer gate is therefore a fast-path bypass (skip the `try_native_method_raw` call entirely
for the common case), not a distinct correctness mechanism -- exactly what step 2 concluded, now
confirmed at a second entry. **Inventory correction 4 downgrades from "a live ordering bug" to
"an redundant defense-in-depth gate `HyperMethodCallDynamic` happens not to have" -- no code
change needed to close it.**

**But the raku-verification pass did find a real, different divergence**: `@a»."WHICH"()`/a plain
`.WHICH`/`.WHY` override is silently ignored (native answer wins) in every call form except a
compile-time-literal quoted method call (`.'WHICH'()`). This is *not* a native-vs-user dispatch-
ordering bug in the ADR-0019 sense -- it traces to a narrower, pre-existing gap specific to
`WHICH`/`WHY` (the two MOP pseudo-methods that are genuinely user-overridable in raku, unlike the
other six) across two independent "skip native pseudo dispatch" mechanisms (one VM-opcode-level,
one interpreter-level). Root-caused and filed as
`todo/deep/pseudo-method-which-why-user-override-ignored-in-bareword-and-dynamic-form.md` -- out
of scope for this campaign, not fixed here.

**Conclusion: E5c part 2 (hyper per-element probes) is closed, docs-only, no code change.**
`HyperMethodCall`'s existing outer gate and `HyperMethodCallDynamic`'s absent one both resolve to
the same safety net inside `try_native_method_raw`, matching step 2's generalized finding. **E5c
is now fully closed** (both parts); **E5d** (JIT-shim parity check, no code change expected) is
the only remaining E5 item.

### E5d: JIT-shim parity check -- confirmed, no code change

Per design decision 4, E5d is "assert the shims still just re-enter the rewritten ops." Of the
two JIT shims named in the corrected entry inventory (`vm_jit_helpers.rs:314/367`), only
`call_method` (`OpCode::CallMethod`, :314-362) is in E5's scope -- `call_method_mut`
(`OpCode::CallMethodMut`, :367+) is E6's `CallMethodMut`, not an E5 entry. `CallMethodDynamic` and
the two hyper opcodes have **no JIT shim at all** (`grep -n 'extern "C" fn' vm_jit_helpers.rs`
lists no dynamic/hyper method-call shim), so they are not JIT-compiled and this check does not
apply to them.

Read `call_method` (:314-362) directly: it reads the `OpCode::CallMethod` payload in place, calls
`interp.sync_source_line(...)` then `interp.exec_call_method_op(code, *name_idx, *arity,
*modifier_idx, *quoted, *arg_sources_idx)` -- the exact same entry point `vm_exec_dispatch.rs`'s
own `CallMethod` arm calls -- and its post-call tail (`apply_pending_rw_writeback`,
`drain_pending_local_updates_after_call`, resume-point recording on error) is byte-identical to
the non-JIT dispatch arm's tail, unchanged by E5b/E5c. Since the shim re-enters
`exec_call_method_op` itself rather than duplicating any of its logic, every change E5b/E5c made
inside that function (the `resolve_method_cached` dedup, the cascade classification) is
automatically covered under JIT with zero shim-side change needed -- confirmed by inspection, not
just assumed. **E5d is closed, docs-only, no code change. All of E5 (steps 1-4, E5b, E5c parts
1-2, E5d) is now closed.** Next up per design decision 4's slicing is E6 (mutation-aware and
container calls).

## Measurement slice results — CallMethodMut (E6a)

Landed 2026-08-11: instrumented `exec_call_method_mut_op_impl` (`src/vm/vm_call_method_mut_ops.rs`,
~line 498-2480), the `CallMethodMut` opcode handler and mutation-aware twin of `CallMethod`'s
`exec_call_method_op_impl` that E5 step 1 instrumented. Reused the same generic counter functions
E5 introduced (`record_dispatch_entry_outcome`/`record_dispatch_entry_intercept`, entry key
`"callmethodmut"`) — no new counter functions. Pure insertions, 233 lines added / 0 deleted, zero
behavior change (verified by inspection: every insertion sits alongside an existing `return`/push
completion, none alters a condition or control-flow branch).

Per design decision 4's E6a scope ("measurement + taxonomy for `CallMethodMut`,
`CallMethodDynamicMut`, `call_method_mut_with_values`, and the Tier-A helpers"), **this slice
covers `CallMethodMut` only** — mirroring how E5 step 1 measured just `CallMethod` and left
`CallMethodDynamic`/hyper/`call_method_all_with_fallback` to steps 2-4. `CallMethodDynamicMut`,
`call_method_mut_with_values` (the second slow path, `runtime/methods_mut_dispatch.rs`), and the
Tier-A helpers are still to do as later E6a sub-slices.

Outcome/arm vocabulary matches E5's: `intercept`/`native`/`user`/`accessor` are disjoint,
`notfound` is an overlay subset of `user` (recorded additionally at the two visible not-found
completions, same convention E5 step 1 established). 33 named intercept arms were added, several
sharing a name with `CallMethod`'s own arms (`pair-freeze`, `proto`, `nativecall`,
`exception-str-message`, `exception-concreteness`, `junction-invocant`, `junction-args`,
`lock-protect`, `lock-protect-nomatch`, `who-pseudo-package` [not fired, see below],
`lazy-first`/`lazy-cache`/`lazy-placeholder`/`lazy-index-pipe`, `modifier-plus`/`modifier-star`,
`hyperseq-*`, `hyper-race-config`) — this function is structurally the mutation-aware twin of the
same interceptor cascade `CallMethod` runs, confirming the design doc's inventory framing. A
distinct sub-family exists only here, tied to writeback: `at-key`/`assign-key`/`delete-key`/
`bind-key`/`bind-pos` (index/attribute mutation ops with no read-only equivalent at `CallMethod`)
and `shared-array-push-atomic`/`shared-array-push-legacy`/`shared-array-pop-shift`/
`shared-array-splice`/`subst-mutate`/`match-make`/`lazy-array-mutate-reject`/`undeclared-type-new`/
`so-not-user-bool`.

### Verification: per-file `sum(disjoint outcomes) == CallMethodMut` in the opcode histogram

The full-sweep aggregate opcode histogram (`opcode_histogram()`, dumped as "opcodes executed
total... (top 30)") is NOT usable for a global cross-check here: with ~340 opcode kinds and only
the top 30 shown per process, `CallMethodMut` silently drops out of many single-file dumps whose
top-30 profile is dominated by other opcodes, undercounting an aggregate sum (observed: naive
aggregate `CallMethodMut=71812` vs `dispatch-entry outcomes` disjoint total `89902` — a 20%
mismatch purely from top-30 truncation, not a counting bug). The dispatch-entry outcome/intercept
dumps do NOT have this problem: total distinct keys stayed at or under 8 for outcomes (cap 25) and
5 for intercept arms (cap 40) in every single-process line observed across the full sweep — no
truncation, so the whole-suite aggregate sums below are exact.

Following E5 step 1's actual per-file verification method instead, five representative files were
run individually (not via the aggregated `-j8` sweep) and cross-checked:

| File | `callmethodmut` disjoint sum | opcode histogram `CallMethodMut` | Match |
|---|---|---|---|
| `t/array-mutate.t` | `native=5` → 5 | 5 | 5 == 5 |
| `t/interp-hash-method-and-typeobject-str.t` | `native=2` → 2 | 2 | 2 == 2 |
| `t/shared-array-mutate-keeps-container-cell.t` | `user=31+native=6+intercept=1` → 38 | 38 | 38 == 38 |
| `t/dot-assign-accessor.t` | `user=22+accessor=16+native=9` → 47 | 47 | 47 == 47 |
| `t/class-is-rw.t` | `accessor=9+user=4` → 13 | 13 | 13 == 13 |

All five exact matches, 0 mismatches — the counting semantics (each executed `CallMethodMut`
records exactly one of `intercept`/`native`/`user`/`accessor`) hold at this entry the same way they
did at `CallMethod`.

### Sweep results (2026-08-11, debug build, full `t/` 3023 files, `prove -j8`, 68 wallclock secs)

Note: 6 files (`say-env-roundtrip.t`, `slip-listop-args.t`, `sink-warning.t`,
`undeclared-routine-compile-time.t`, `weird-errors-parse-forms.t`,
`vendored-real-test-module.t`) report subtest failures under `MUTSU_VM_STATS=1` — confirmed
**pre-existing on `main` before this PR** (they assert on exact stderr content, and the vm-stats
dump itself writes to stderr at process exit; unrelated to this slice's instrumentation). Not a
regression; the counts below are unaffected since the dump still fires on every process exit
regardless of the test's own pass/fail.

Outcomes (disjoint): `callmethodmut:user=45097` (50.2%), `callmethodmut:native=38640` (43.0%),
`callmethodmut:intercept=3830` (4.3%), `callmethodmut:accessor=2335` (2.6%); overlay
`callmethodmut:notfound=28` (0.06% of user). Disjoint total 89902 — roughly 3.3x `CallMethod`'s
26924 from E5 step 1's sweep, confirming the design doc's prediction that bareword/variable
receivers (which compile to `CallMethodMut`, not `CallMethod`) carry the bulk of ordinary method
traffic in `t/`.

`user`/`native` again dominate roughly equally (93.2% combined) — same sub-slice-ordering
consequence E5b step 1 drew for `CallMethod`: the eventual E6b cutover's decision match must get
the user-candidate and native-row paths right first.

Intercept arms by count (32 of 33 fired; `who-pseudo-package` is the lone zero, all of its `.WHO`
traffic apparently reaching this entry as a Package receiver via `CallMethod` instead):
`lock-protect=2072`, `shared-array-push-atomic=1445`, `at-key=68`, `assign-key=27`,
`hyper-race-config=26`, `junction-args=25`, `lazy-placeholder=17`, `delete-key=15`,
`hyperseq-iterator=15`, `proto=13`, `nil-predispatch=12`, `nativecall=11`, `subst-mutate=11`,
`modifier-star=9`, `lazy-first=8`, `modifier-plus=7`, `exception-concreteness=7`,
`shared-array-push-legacy=6`, `junction-invocant=5`, `pair-freeze=4`, `bind-pos=4`,
`lazy-index-pipe=4`, `lazy-array-mutate-reject=3`, `bind-key=3`, `exception-str-message=2`,
`match-make=2`, `lock-protect-nomatch=2`, `lazy-cache=2`, `hyperseq-defined=1`,
`undeclared-type-new=1`, `hyperseq-name=1`, `hyperseq-is-lazy=1`, `hyperseq-what=1`.

Two findings confirm predictions E5 step 1 made about arms that scored zero at `CallMethod`:
`lock-protect` (2072, by far the largest single intercept arm — `$lock.protect`/
`Lock::Async.protect` on a *variable* receiver, which E5 step 1 noted compiles to `CallMethodMut`
not `CallMethod`, where it scored 0) and `exception-concreteness` (7 here vs 0 at `CallMethod`,
same "bareword receiver compiles to `CallMethodMut`" explanation). `shared-array-push-atomic`
(1445) is the second-largest arm and has no `CallMethod` twin at all — it belongs entirely to the
mutation-only writeback sub-family this entry adds.

`make test`-equivalent (`prove -e target/debug/mutsu t/*.t`, 3023 files) green other than the 6
pre-existing MUTSU_VM_STATS stderr-content files noted above (confirmed unaffected by this diff by
reproducing the same 6 failures on `main` with `MUTSU_VM_STATS=1` before this change).
`cargo clippy -- -D warnings` and `cargo fmt` clean.

**E6a's `CallMethodMut` measurement is done. Still to do**: `CallMethodDynamicMut` and
`call_method_mut_with_values` measurement slices (mirroring E5 steps 2/4), then the Tier-A helper
survey, then the actual E6b/E6c/E6d cutover work.

## Measurement slice results — CallMethodDynamicMut (E6a, second slice)

Landed 2026-08-11: instrumented `exec_call_method_dynamic_mut_op`
(`src/vm/vm_call_method_mut_ops.rs:347-433`), a much smaller function than `CallMethodMut`'s
~87 lines vs ~2300. Same counter functions, entry key `"callmethoddynamicmut"`, pure insertions
(14 lines), zero behavior change.

The function has exactly four completion shapes: the `.+`/`.*` modifiers (`modifier-plus`/
`modifier-star` intercepts, delegating to `call_method_all_with_fallback`, its own already-measured
entry), a `$obj.$coderef(...)` call-sub-value form (`call-sub-value` intercept), a narrow native fast
path for dynamic-name Buf mutating writes (`try_native_buf_mut`, outcome `native`), and the generic
fallback to `vm_call_method_mut_with_values` (outcome `user`, the interpreter slow path — marked
`// TODO: compile to bytecode` in the source already). No accessor probe, no distinct not-found
completion (the error just propagates via `?`), matching the design doc's inventory-correction
observation that this entry "reaches the interpreter with no [compiled-method] probe at all" — the
`try_native_buf_mut` fast path is narrow (Buf-only) rather than a general native probe.

Verified via 5 individually-run files (the same file set E5 step 2 used for `CallMethodDynamic`,
since this is its Mut twin): `buf-write-native.t` (`native=1` == opcode `CallMethodDynamicMut=1`),
`format-class.t` (`user=4` == 4), `indirect-method-call-lvalue.t` (`user=1` == 1),
`self-in-nested-sub-coherence.t` (`user=1` == 1), `array-value-path-mutation.t` (0 in both — the
entry simply isn't exercised by that file). 5/5 exact matches.

Full `t/` sweep (3023 files, same 6 pre-existing `MUTSU_VM_STATS`-stderr files noted in the
`CallMethodMut` section above, unrelated): `user=29`, `call-sub-value=11` (== `intercept=11`),
`native=1`. Disjoint total 41 — a low-traffic entry, consistent with E5 step 2's finding that
`CallMethodDynamic` itself (this entry's non-mut twin) was "far lower-traffic" than `CallMethod`.
No `modifier-plus`/`modifier-star` traffic in `t/` (0 — matches `CallMethodDynamic`'s own 0 for
the same modifiers in E5 step 2's sweep). `make test` (3023 files/28293 tests) green; `cargo
clippy -- -D warnings` and `cargo fmt` clean.

## Measurement slice results — call_method_mut_with_values (E6a, third slice)

Landed 2026-08-11: instrumented `call_method_mut_with_values`
(`src/runtime/methods_mut_dispatch.rs:11-2748`), "the second slow path" per design decision 4's
E6a scope — a single ~2750-line function that IS the whole file (only one `impl Interpreter`
block), comparable in size to `CallMethodMut`'s own ~2300-line handler. Same counter functions,
entry key `"callmethodmutwithvalues"`, pure insertions (182 lines added / 0 deleted), zero behavior
change (every insertion sits immediately before an already-existing `return`/fall-through path;
none alters a condition).

Unlike `CallMethodMut`/`CallMethodDynamicMut` (VM opcode handlers reached once per bytecode
dispatch), this function is a plain `Interpreter` method reached from ~10 call sites across the
codebase: `CallMethodMut`'s own generic-fork tail (`vm_call_method_mut_ops.rs:2363/2432/2441`,
the `user` outcome), `CallMethodDynamicMut`'s fallback, `vm_call_method_compiled_mut.rs`,
`vm_var_trait_ops.rs` (`.VAR`), `vm_call_helpers.rs`, `vm_for_loop_dispatch.rs` (`pull-one`),
`class_dispatch.rs`, `methods_call_dispatch.rs`, `builtins_multidim_subscript.rs`,
`methods_mut_method_lvalue.rs`, and `methods_collection_ops/tail_rotate.rs` — plus the function
recurses into itself once (the `ContainerRef` cell-unwrap branch) and calls itself again from the
Instance delegation branch. So its traffic is not a strict subset of `CallMethodMut`'s `user`
count; the two are correlated but distinct populations.

The function's body is almost entirely a cascade of top-level `if`/`match method` special cases —
30-odd named receiver/method-identity checks — with no single dominant "generic" middle tier of its
own; the true generic tail is the very last line, which delegates the receiver to the non-mut
sibling `call_method_with_values`. 41 named intercept arms were added. Three families repeat with
near-identical bodies for different receiver shapes and got distinct arm names per shape rather
than being collapsed: the `@`-sigil array mutator match (`array-push`/`array-append`/
`array-unshift`/`array-prepend`/`array-pop`/`array-shift`/`array-splice`/`array-squish`), the
sigilless-array-binding twin of the same match (`sigilless-push-append`/`sigilless-pop`/
`sigilless-unshift`/`sigilless-prepend`/`sigilless-shift` — no separate splice/squish arm here,
those route through the `@`-sigil block instead per a shared `scalar_holds_real_array`/`starts_with
('@')` guard), and the `%`-sigil hash push/append match (`hash-push-append`, one arm covering both
`push`/`append` since an internal `is_push` flag distinguishes them, mirroring the granularity
`CallMethodMut`'s own `lock-protect` used). Other arms: `container-ref-cell`, `immutable-list-
reject`, `incdec`, `keyof`, `var-reflect` (`.VAR`), `of`, `collation-set`, `sethash-set-unset`, ten
`buf-*` arms (`buf-read-bits`/`buf-write-bits`/`buf-write-num-mut`/`buf-write-num-fresh`/
`buf-write-int-mut`/`buf-write-int-fresh`/`buf-reallocate`/`buf-pop-shift-splice`/
`buf-mutate-append`/`buf-bits-instance-fallback`), `map-rw-writeback`,
`sethash-grab`/`baghash-grab`/`mixhash-grab`, `promise-channel-delegate`, `classhow`,
`iterator-protocol` (one arm covering the whole `class_name == "Iterator"` sub-cascade, which is
itself ~170 lines with its own internal method dispatch — left uninstrumented internally per the
"not every return needs a counter" rule, since every path through it already returns before
reaching the end), `delegation`, and two accessor-write sub-outcomes (`rw-proxy-signal`,
`rw-readonly-reject`) alongside the `accessor` outcome itself for the successful-write case.
Outcomes used: `intercept`/`native`/`user`/`accessor` — no `notfound`, since every error this
function raises is a typed `X::` error thrown from within an already-committed named arm (not a
generic "no such method" completion); the final generic fallback line hands `notfound`
classification to whatever the non-mut sibling decides, outside this entry's own count.

### Verification: no opcode-histogram cross-check available; self-consistency against `callmethodmut:user` instead

This entry is a plain function, not an opcode handler, so there is no `opcode_histogram()` row to
cross-check against (unlike `CallMethodMut`/`CallMethodDynamicMut`, which are VM dispatch loop
entries). Five individually-run files were used instead, checking `callmethodmutwithvalues`'s
disjoint sum against `callmethodmut:user` from the same run as an order-of-magnitude sanity check
(not a formal subset, per the multi-caller point above, but the dominant caller by far):

| File | `callmethodmutwithvalues` disjoint sum | `callmethodmut:user` (same run) | Relationship |
|---|---|---|---|
| `t/array-push-byref-coherence.t` | `intercept=20` (`sigilless-push-append=17`, `-pop=1`, `-shift=1`, `-unshift=1`) | 20 | 20 == 20 |
| `t/buf-splice-count.t` | `intercept=5` (`buf-pop-shift-splice=5`) | 11 | 5 <= 11 |
| `t/buf-splice-list-bytes.t` | `intercept=5` (`buf-pop-shift-splice=5`) | 11 | 5 <= 11 |
| `t/from-iterator.t` | `user=15` | 15 | 15 == 15 |
| `t/pop-shift-sub-empty-failure.t` | `user=1` | 1 | 1 == 1 |
| `t/array-subclass-vector.t` | `user=10` | 25 | 10 <= 25 (the other 15 resolve inside `try_compiled_method_mut_or_interpret_sym` without reaching this function) |

All six checked files are consistent (`callmethodmutwithvalues` never exceeds `callmethodmut:user`
in the same run), three are exact matches. This is the closest available correctness signal short
of a formal cross-check; it is not proof of completeness the way the opcode-histogram match was for
`CallMethodMut`, and is reported as such rather than overstated.

### Sweep results (2026-08-11, debug build, full `t/` 3023 files, `prove -j8`, 63 wallclock secs)

No truncation in this sweep: the `dispatch-entry outcomes` line's displayed `(top N)` never exceeded
10 per process (cap 25), and the `intercept arms` line's never exceeded 7 (cap 40) — so the whole-
suite aggregate sums below (computed by summing every `key=count` token across all 3023 per-process
lines) are exact, not undercounts.

Outcomes (disjoint): `callmethodmutwithvalues:native=14501` (52.9%),
`callmethodmutwithvalues:user=11100` (40.5%), `callmethodmutwithvalues:intercept=1812` (6.6%),
`callmethodmutwithvalues:accessor=0` (0%). Disjoint total 27413 — about 61% of `callmethodmut:user`'s
own full-sweep total (45085, close to but not identical to E6a slice 1's 45097, expected drift from
intervening commits), consistent with this function being `CallMethodMut`'s dominant but not sole
feeder.

`accessor=0` is a notable negative finding: the single-arg rw-accessor-write fast path
(`attributes.contains_key(method)` / `is_rw` public attribute, writing `args[0]` directly) never
fired anywhere in `t/`, meaning ordinary `$obj.attr = val`-style rw-attribute writes in the local
suite are all resolved before reaching this function (compiled-method fast paths, or Proxy-mediated
writes that reach `rw-proxy-signal` instead — which itself is very low-traffic at 7).

Intercept arms by count (28 of 41 fired, 13 scored zero in `t/`):
`promise-channel-delegate=1011`, `delegation=212`, `buf-pop-shift-splice=87`,
`sigilless-push-append=75`, `var-reflect=74`, `map-rw-writeback=59`, `buf-mutate-append=47`,
`iterator-protocol=44`, `incdec=44`, `array-splice=28`, `array-push=26`, `classhow=22`, `of=20`,
`hash-push-append=10`, `array-append=8`, `sigilless-pop=7`, `rw-proxy-signal=7`,
`sethash-set-unset=6`, `array-pop=5`, `sigilless-shift=4`, `array-unshift=4`, `buf-reallocate=3`,
`array-squish=2`, `array-shift=2`, `array-prepend=2`, `sigilless-unshift=1`, `keyof=1`,
`immutable-list-reject=1`. The sum of these 28 counts is exactly 1812, matching the `intercept`
outcome total above — confirms the counting semantics hold here too (every intercept bump goes
through `record_dispatch_entry_intercept`, which bumps both the outcome and the arm histogram
atomically). Zero in `t/`: `container-ref-cell`, `collation-set`, `sethash-grab`, `baghash-grab`,
`mixhash-grab`, `buf-read-bits`, `buf-write-bits`, `buf-write-num-mut`, `buf-write-num-fresh`,
`buf-write-int-mut`, `buf-write-int-fresh`, `buf-bits-instance-fallback`, `rw-readonly-reject` — all
rare/edge-case receiver shapes (native-int Buf bit/num/int writes, SetHash/BagHash/MixHash `.grab`,
`state`-cell-held aggregate mutation, readonly-attribute-assignment rejection) that the local `t/`
suite happens not to exercise, not evidence they are dead code.

`promise-channel-delegate` (1011) being the single largest arm is notable: `Promise`/`Channel`
mutation calls are common in `t/`'s concurrency tests and this entry's *only* job for them is an
immediate one-line delegate to the non-mut sibling (`ValueView::Promise(_) | ValueView::Channel(_)
=> return self.call_method_with_values(...)`), so essentially all of that traffic is pure pass-
through overhead — a concrete future E6c/E6d cutover target (a fast pre-check could route straight
to the non-mut entry without ever compiling/reaching the mut fork for these two receiver kinds).

20 files fail under `MUTSU_VM_STATS=1` in this sweep (broader than the 6 files noted in the
`CallMethodMut` section above): `cli-lines-regressions.t`, `command-line-negation.t`,
`constant-hash-coerce-once.t`, `dd-instance.t`, `exit-skips-main-dispatch.t`, `get-out.t`,
`io-handle-lock.t`, `io-handle-stdout-stderr-native.t`, `io-pipe-slurp-rest.t`, `is-run.t`,
`note-with-parens.t`, `precomp-warm-cache-parity.t`, `proc-async.t`, `quietly.t`,
`say-env-roundtrip.t`, `sink-warning.t`, `slip-listop-args.t`,
`undeclared-routine-compile-time.t`, `vendored-real-test-module.t`, `weird-errors-parse-forms.t`.
All share the same root cause the `CallMethodMut` section already identified — the vm-stats dump
unconditionally writes to stderr at process exit, and any test asserting exact/empty stderr (either
of its own process, or of a subprocess it spawns via `is_run`/`shell`/`Proc::Async` inheriting
`MUTSU_VM_STATS` from the parent environment) fails. Verified pre-existing and unrelated to this
diff: `t/cli-lines-regressions.t` and `t/constant-hash-coerce-once.t` were individually re-run
against the pre-slice commit (`be980a448`, this branch's base, before any of the 182-line diff) with
`MUTSU_VM_STATS=1` and failed identically. The earlier 6-file list was evidently a partial spot-
check, not an exhaustive one — this slice's sweep is the first full `MUTSU_VM_STATS=1` `t/` run
against this many call sites at once, so more of the pre-existing subprocess-stderr-inheritance
files got exercised. Not a regression; `make test` (no `MUTSU_VM_STATS`) is unaffected by this
class of failure entirely, since it never sets the env var.

`make test` (no `MUTSU_VM_STATS`, 3023 files/28293 tests) green. `cargo clippy -- -D warnings` and
`cargo fmt` clean.

**E6a's `call_method_mut_with_values` measurement is done — all three E6a sub-slices
(`CallMethodMut`/`CallMethodDynamicMut`/`call_method_mut_with_values`) are now measured. Still to
do**: the Tier-A helper survey, then the actual E6b/E6c/E6d cutover work.

## Tier-A helper survey (E6a, final sub-slice)

Docs-only investigation, no dispatch behavior changed. Closes the last item design decision 4
names for E6a: "measurement + taxonomy for `CallMethodMut`, `CallMethodDynamicMut`,
`call_method_mut_with_values`, and the Tier-A helpers." The first three are the three measurement
slices above; this slice cross-checks `native_method_row.rs`'s `MUTATES_RECEIVER` flag (see its
doc comment, `src/builtins/native_method_row.rs:81-85`) — generated once by E2a's 2026-08-10
probe — against what the two instrumented files (`vm/vm_call_method_mut_ops.rs`,
`runtime/methods_mut_dispatch.rs`) actually do, the same way E2b did for the ordinary native rows.

### Method: enumerate every named intercept arm, then trace each to its owner/method guard

Every `record_dispatch_entry_intercept("callmethodmut", ...)` call in
`vm/vm_call_method_mut_ops.rs` and every `record_dispatch_entry_intercept("callmethodmutwithvalues",
...)` call in `runtime/methods_mut_dispatch.rs` was located by grep and read in place (the guarding
`if`/`match` immediately above each counter call), plus the four *unnamed* Tier-A helper functions
that record only the generic `native` outcome rather than a per-arm name
(`try_native_array_mut`/`try_native_array_splice`/`try_native_hash_mut_bound`/`try_native_buf_mut`,
`vm_call_method_mut_ops.rs:2566/2867/2541/2982`) — these are exactly the "Tier-A mutable-method
dispatch" the flag's doc comment names, so they belong in the survey even without an arm label.
Then each arm's (owner, method) pair(s) were looked up in `native_method_row_table.rs`'s `RAW_ROWS`
(grepped by owner and by method-name literal) to classify per the task's three outcomes, plus two
outcomes the strict three did not anticipate but the data forced (see "Additional findings" below).

`RAW_ROWS` currently has exactly 41 `MUTATES_RECEIVER` rows (grep `', [0-9]+, 2),$'`, verified no
row combines the flag with `SPECIAL`/`TYPE_OBJECT_OK`, i.e. every hit is flags-value `2` exactly),
all under four owners: `Str.subst-mutate` (1); `List`/`Array` each with the identical 15-name set
`map, grep, rotate, push, pop, shift, unshift, splice, append, prepend, classify, categorize,
rotor, produce, reduce` (30); `Hash.push`/`Hash.append` (2); `Blob.new, push, pop, shift, unshift,
append, prepend, splice` (8). Every one of these 41 rows also carries arity `8` (`N` — "not served
by any pure `native_method_{0,1,2}arg` cascade"), with no exception.

### Taxonomy — `CallMethodMut` (`vm/vm_call_method_mut_ops.rs`)

Named intercept arms (line numbers current as of this PR). "Owner/method" is `N/A (cross-cutting)`
for arms that are method-identity intercepts independent of a `builtin_type_methods` owner (e.g.
`.return`-style control flow, junction threading) — those were never candidates for a `RAW_ROWS`
row in the first place, so they are listed for completeness but excluded from the cross-check
counts below.

| Arm | Line | Owner / method served | RAW_ROWS status |
|---|---|---|---|
| `exception-concreteness` | ~612 | Exception type-object `.throw`/`.fail`/... (any `X::*` subclass name) | N/A (cross-cutting, not a fixed owner) |
| `nativecall` | ~653 | `is native(...)`-bound methods (any owner) | N/A (cross-cutting) |
| `lazy-array-mutate-reject` | ~687 | `LazyList` push/pop/append (error path) | owner `LazyList` never probed by E2a — no row |
| `pair-freeze` | ~714 | `Pair.freeze` | **no row at all** (`Pair` has 33 other rows, none named `freeze`) — case (c) |
| `proto` | ~721 | `proto method` body dispatch (any owner) | N/A (cross-cutting) |
| `exception-str-message` | ~733 | Exception `.Str`/`.gist` via user `message` | N/A (cross-cutting) |
| `lazy-placeholder` | ~748 | `LazyList` `.gist`/`.Str`/`.raku`/`.perl` | owner never probed — no row |
| `lazy-first` | ~766 | `LazyList.first` | owner never probed — no row |
| `lazy-index-pipe` | ~784 | `LazyList` `.kv`/`.pairs`/`.antipairs` | owner never probed — no row |
| `lazy-cache` | ~800 | `LazyList.cache` | owner never probed — no row |
| `so-not-user-bool` | ~883 | `.so`/`.not` via user `Bool` (any owner) | N/A (cross-cutting) |
| `undeclared-type-new` | ~913 | error path, not a real owner | N/A |
| `junction-invocant` | ~943 | junction auto-threading (any owner) | N/A (cross-cutting) |
| `junction-args` | ~997 | junction auto-threading, args (any owner) | N/A (cross-cutting) |
| `who-pseudo-package` | ~1008 | `.WHO` pseudo-method (any owner) | N/A (pseudo-method) |
| `lock-protect-nomatch` | ~1034 | `Lock`/`Lock::Async`/`Lock::Soft.protect` (bad-arg error) | owner `Lock`/`Lock::Async` never probed — no row |
| `lock-protect` | ~1053 | `Lock`/`Lock::Async.protect` (fast path) | owner never probed — no row |
| `shared-array-push-atomic` | ~1121 | `Array`/`List` `push`/`unshift`/`append`/`prepend` (shared `@`-var) | **row exists, `MUTATES_RECEIVER`** — case (a) |
| `shared-array-push-legacy` | ~1139 | `Array`/`List` `push`/`unshift` (shared, legacy fallback) | case (a), same rows |
| `shared-array-pop-shift` | ~1166 | `Array`/`List` `pop`/`shift` (shared) | case (a), same rows |
| `shared-array-splice` | ~1190 | `Array`/`List.splice` (shared) | case (a), same rows |
| `match-make` | ~1299 | `Match.make` | **no row at all** (`Match` has ~70 other rows, none named `make`) — case (c) |
| `subst-mutate` | ~1316 | `Str.subst-mutate` | **row exists, `MUTATES_RECEIVER`** — case (a), clean |
| `hyper-race-config` | ~1351 | `.hyper`/`.race` with named args (any Iterable) | N/A (cross-cutting) |
| `hyperseq-{hyper,race,is-lazy,defined,name,what}` | ~1458 (one call site, 6 string outcomes) | `HyperSeq`/`RaceSeq` identity methods | owners never probed — no rows |
| `hyperseq-map-grep` | ~1491 | `HyperSeq`/`RaceSeq` `.map`/`.grep` delegate | owners never probed — no rows |
| `hyperseq-iterator` | ~1499 | `HyperSeq`/`RaceSeq.iterator` | owners never probed — no rows |
| `at-key` | ~1555 | `Hash`/`Set`/`Bag`/`Mix` `AT-KEY` (**read**, not a mutation) | `Hash.AT-KEY` row exists, flags `0` (correctly *not* `MUTATES_RECEIVER` — this arm never writes) — case (a) |
| `assign-key` | multiple, ~1592-1711 | `Hash`/`Set`/`Bag`/`Mix` `ASSIGN-KEY` (genuine write) | **no row for `ASSIGN-KEY` on any owner** — case (c) |
| `delete-key` | multiple, ~1719-1866 | `Hash`/`Set`/`Bag`/`Mix` `DELETE-KEY` (genuine write) | **no row at all** — case (c) |
| `bind-key` | multiple, ~1930-1998 | `Hash`/`Set`/`Bag`/`Mix` `BIND-KEY` (genuine write) | **no row at all** — case (c) |
| `bind-pos` | ~2085 | `Array.BIND-POS` (genuine write) | **no row at all** — case (c) |
| `modifier-plus`/`modifier-star` | ~2133/2142 | `.+`/`.*` MRO-walk modifiers (any owner) | N/A (cross-cutting; delegates to `call_method_all_with_fallback`, its own E5 entry) |
| `nil-predispatch` | ~2107 | `Nil` pre-dispatch errors | N/A (not a `builtin_type_methods` owner) |

Plus the four unnamed Tier-A helpers (record only the generic `native` outcome, no per-arm name):

| Helper | Line | Owner / method served | RAW_ROWS status |
|---|---|---|---|
| `try_native_array_mut` | 2566 | `Array` (plain `@`-array, `ArrayKind::Array`) `push`/`append`/`prepend`/`unshift`/`pop`/`shift` | **rows exist, `MUTATES_RECEIVER`**, both under `Array` and `List` — case (a) for `Array`; `List` shares the identical row set even though this helper only ever sees an `ArrayKind::Array` receiver in practice (List-kind values route to the interpreter fallback) — harmless over-coverage, not a mismatch |
| `try_native_array_splice` | 2867 | `Array.splice` (plain `@`-array, simple non-erroring forms) | case (a), same rows |
| `try_native_hash_mut_bound` | 2541 | `Hash.push`/`Hash.append` (bound-cell variant) | case (a), same `Hash` rows |
| `try_native_buf_mut` | 2982 | `Blob`/`Buf` family `write-bits`/`write-ubits`/`write-num*`/`write-int*` | **no row at all** for any `write-*` name on `Blob` — case (c); distinct from the `Blob.new/push/pop/shift/unshift/append/prepend/splice` rows, which this helper does NOT serve (those are served by the `buf-mutate-append`/`buf-pop-shift-splice`/`buf-reallocate` arms in `methods_mut_dispatch.rs` below) |

Also present in this file but not a `record_dispatch_entry_intercept` arm at all: the
Array-subclass Instance-delegation block (`vm_call_method_mut_ops.rs:2219-2407`, reached via the
generic `native`/`user` outcome, not a named arm) contains an `is_array_method` allow-list that
literally includes the strings `map`, `grep`, `rotate`, `classify`, `categorize`, `rotor`,
`produce`, `reduce` (`vm_call_method_mut_ops.rs:2244-2276`) alongside `push`/`pop`/`shift`/
`unshift`/`append`/`prepend`/`splice`. This allow-list is the direct explanation for the
`List`/`Array` `MUTATES_RECEIVER` rows on those eight non-mutator names — see "Additional
findings" below; it is the single most consequential thing this survey found.

### Taxonomy — `call_method_mut_with_values` (`runtime/methods_mut_dispatch.rs`)

| Arm | Owner / method served | RAW_ROWS status |
|---|---|---|
| `container-ref-cell` | dispatch wrapper, any owner | N/A |
| `immutable-list-reject` | `List` (non-real-array) push/append/pop/shift/unshift/prepend/splice — error path | N/A (error path, not a servable dispatch) |
| `incdec` | `postfix:<++>`/`postfix:<-->` operators (any owner) | N/A (operator, not a method name) |
| `keyof` | `Mix`/`Set`/`Bag.keyof` | no row for `keyof` on any of these owners — case (c) |
| `var-reflect` | `.VAR` pseudo-method (any owner) | N/A (pseudo-method) |
| `of` | `.of` on `@`/`%` containers | N/A (container reflection, not a `builtin_type_methods` owner) |
| `collation-set` | `Collation.set` | owner `Collation` never probed — no row |
| `sethash-set-unset` | `SetHash.set`/`SetHash.unset` | **no row for `set`/`unset` on `SetHash`** — case (c) |
| `array-push`/`array-append`/`array-unshift`/`array-prepend`/`array-pop`/`array-shift`/`array-splice` | `Array`/`List` (`@`-sigil variable) | **rows exist, `MUTATES_RECEIVER`** — case (a), same rows the Tier-A helpers above serve |
| `array-squish` | `Array`/`List.squish` | row exists, flags `0` (**not** `MUTATES_RECEIVER`) — consistent: this arm only writes back conditionally (`if self.in_lvalue_assignment`), and `.squish` alone never needs the mut path — case (a), correctly unflagged |
| `hash-push-append` | `Hash.push`/`Hash.append` | case (a), same `Hash` rows as `try_native_hash_mut_bound` |
| `sigilless-push-append`/`-pop`/`-unshift`/`-prepend`/`-shift` | `Array`/`List` (sigilless-bound variable form of the same methods) | case (a), same rows — no distinct owner, just a different variable-shape gate |
| `buf-read-bits` | `Blob`/`Buf` `read-bits`/`read-ubits` (**read**, not a mutation) | no row for `read-*` on `Blob` — case (c), but consistent (this arm never mutates) |
| `buf-write-bits` | `Blob`/`Buf` `write-bits`/`write-ubits` | **no row at all** — case (c) |
| `buf-write-num-mut`/`buf-write-num-fresh` | `Blob`/`Buf` `write-num8`/`16`/`32`/`64` family | **no row at all** — case (c) |
| `buf-write-int-mut`/`buf-write-int-fresh` | `Blob`/`Buf` `write-int*`/`write-uint*` family | **no row at all** — case (c) |
| `buf-reallocate` | `Blob`/`Buf.reallocate` | **no row at all** — case (c) (note: `Blob.new` DOES have a row, but `reallocate` is a distinct method with none) |
| `buf-pop-shift-splice` | `Blob`/`Buf` `pop`/`shift`/`splice` | **rows exist, `MUTATES_RECEIVER`** — case (a) |
| `buf-mutate-append` | `Blob`/`Buf` `push`/`append`/`prepend`/`unshift` | case (a), same rows |
| `buf-bits-instance-fallback` | `Blob`/`Buf` bits fallback | covered by the same no-row family as `buf-write-bits` |
| `map-rw-writeback` | `Array`/`List.map` (rw `$_` writeback) | **row exists, `MUTATES_RECEIVER`** — case (a), the ONE genuinely-mutating name among the eight `is_array_method`-only names (see next section) |
| `sethash-grab` | `SetHash.grab`/`.grabpairs` | owner has no `grab`/`grabpairs` row at all — case (c) |
| `baghash-grab` | `BagHash.grab`/`.grabpairs` | **row exists but flagged `SPECIAL`, not `MUTATES_RECEIVER`**, despite the arm doing a genuine `self.env.insert` mutation — see "Additional findings" |
| `mixhash-grab` | `MixHash.grab`/`.grabpairs` | same as `baghash-grab`: row flagged `SPECIAL`, not `MUTATES_RECEIVER` |
| `promise-channel-delegate` | `Promise`/`Channel` (pure pass-through to the non-mut sibling, no mutation here itself) | owners never probed — no rows; consistent, since this arm does not mutate |
| `classhow` | `.HOW`-adjacent class reflection, any owner | N/A |
| `iterator-protocol` | `Iterator` (`pull-one` etc.) | owner `Iterator` never probed — no rows |
| `delegation` | generic instance delegation fallback, any owner | N/A |
| `rw-proxy-signal` | `Proxy` accessor-write signal | owner `Proxy` never probed — no row |
| `rw-readonly-reject` | readonly-attribute-write rejection (error path) | N/A (error path) |

### Additional findings (beyond the task's three anticipated outcomes)

1. **The `MUTATES_RECEIVER` probe only ever grepped `vm/vm_call_method_mut_ops.rs`, never
   `runtime/methods_mut_dispatch.rs`.** The flag's own doc comment says so explicitly ("the name
   also appears in the Tier-A mutable-method dispatch, `vm/vm_call_method_mut_ops.rs`") and the
   data confirms it: `"grab"` appears nowhere in `vm_call_method_mut_ops.rs` (`grep -c` = 0) but
   is a fully-formed, genuinely-mutating named arm (`baghash-grab`/`mixhash-grab`/`sethash-grab`)
   in `methods_mut_dispatch.rs`. Since `methods_mut_dispatch.rs` is an equally-sized second
   Tier-A surface (~2750 lines vs. `vm_call_method_mut_ops.rs`'s ~3070) with its own 30-odd named
   mutating arms, this is the single biggest systematic source of the case-(c)/case-(b) gaps
   above — every genuinely-mutating name whose *only* textual occurrence is in
   `methods_mut_dispatch.rs` (not also present as a literal string somewhere in
   `vm_call_method_mut_ops.rs`) was structurally unreachable by the original probe's
   `MUTATES_RECEIVER` refinement, regardless of how real its Tier-A mutation is. `keyof`,
   `sethash-set-unset`, `collation-set`, the whole `buf-write-*`/`buf-read-*` family, and the
   `*-grab` arms are all examples.

2. **`List`/`Array`'s `map`/`grep`/`rotate`/`classify`/`categorize`/`rotor`/`produce`/`reduce`
   rows over-claim `MUTATES_RECEIVER`.** All eight names appear together in one place only:
   the `is_array_method` allow-list inside the Array-subclass Instance-delegation branch
   (`vm_call_method_mut_ops.rs:2219-2407`) — a narrow path for `class Foo is Array {...}`
   instances, unrelated to plain `@`-array dispatch. Within that branch, all eight are routed
   through explicitly non-mutating helpers (`try_native_array_map`/`try_native_first`/
   `try_native_minmax`, or the whitelist-gated `try_native_method` call at
   `is_array_storage_native_safe`) that borrow the backing storage immutably and return a fresh
   value — never mutating the instance. Cross-checked all eight against both files end-to-end:
   only `map` has a *separate*, genuinely-mutating arm anywhere (`map-rw-writeback` in
   `methods_mut_dispatch.rs`, which writes back `$_`-mutated elements — real Raku `rw` semantics
   for `.map`). `grep` conspicuously has no equivalent writeback arm despite also `rw`-binding
   `$_` per the Raku spec — a possible separate correctness gap, out of scope here, not filed
   separately since confirming it needs its own raku-baseline comparison. `rotate`/`classify`/
   `categorize`/`rotor`/`produce`/`reduce` have no mutating arm at all in either file. So the
   `MUTATES_RECEIVER` flag on these seven (of the eight) rows is best read as "this literal string
   co-occurs with a real Tier-A mutator's name inside the same allow-list," not "this name is
   genuinely receiver-mutating" — a probe-methodology artifact, not a hand error.

3. **`BagHash.grab`/`MixHash.grab` are genuinely Tier-A-mutating (`self.env.insert` on the
   receiver variable, confirmed by direct read) but are flagged `SPECIAL`, not
   `MUTATES_RECEIVER`.** This is explained by finding 1 (the probe never saw `methods_mut_dispatch.rs`
   at all) rather than being a one-off mistake. Practically inert today: `native_row_servable`'s
   `reachable()` excludes a row if it has *either* `SPECIAL` *or* `MUTATES_RECEIVER`
   (`native_method_row.rs:119-120`), so both flags currently produce the identical answer for
   these two rows. Recording this as a finding rather than fixing it: the "correct" flag value
   depends on how a future regeneration wants to distinguish "handled by a named interceptor
   ahead of the cascade" from "handled by a Tier-A mutable-method helper" — both are literally
   true here, and I am not confident which the maintainers intend as authoritative without
   re-running the full E2a-style probe (out of scope for a docs-only survey).

4. **Every current `MUTATES_RECEIVER` row also carries arity `N` (`8`), making the flag
   currently redundant with the arity encoding.** `native_row_servable`'s arity check
   (`arity.contains(call_arity)`) already returns `false` for an `N`-only row at any real call
   arity (0/1/2), independent of the `MUTATES_RECEIVER`/`SPECIAL` flags. So for the 41 rows that
   exist today, the flag has zero *additional* effect on the one production reader
   (`native_row_servable`) beyond what `arity = N` alone already achieves — it is documentary,
   not load-bearing, until a future row combines `MUTATES_RECEIVER` with a non-`N` arity (a
   combination that does not exist in the table today).

5. **Whole owners were never probed by E2a at all**, so they have zero rows regardless of
   mutation status: `Lock`, `Lock::Async`, `Lock::Soft`, `Collation`, `Promise`, `Channel`,
   `LazyList`, `HyperSeq`, `RaceSeq`, `Proxy`, `Iterator`. This is a pre-existing E2a coverage
   gap (not new to this survey), but it explains most of the "no row at all" verdicts above that
   are not really about the `MUTATES_RECEIVER` flag specifically — there was never any row to
   flag correctly or incorrectly.

### Why no `native_method_row_table.rs` edits landed with this survey

Per the task's own guidance, a correction is only made when confident it is right; recording is
the fallback. Two candidate corrections were identified (findings 2 and 3), and both were left
unmade:

- **Finding 2** (removing `MUTATES_RECEIVER` from `List`/`Array`'s `grep`/`rotate`/`classify`/
  `categorize`/`rotor`/`produce`/`reduce` rows) is not obviously safe: these names still need
  `&mut self` to invoke a user block/comparator (per `CLAUDE.md`'s own description of the slow
  path), even though they never mutate the *receiver*. Whether `MUTATES_RECEIVER` was meant to
  capture "receiver is mutated" specifically or the broader "not servable by the plain `&self`
  arity cascade at all" is not settled by the doc comment alone, and guessing wrong would make
  the table *less* accurate, not more.
- **Finding 3** (changing `BagHash.grab`/`MixHash.grab` from `SPECIAL` to `MUTATES_RECEIVER`, or
  to both) is currently behaviorally inert (see finding 4), so there is no urgency, and the
  "right" combined value depends on the same unsettled question as finding 2.

Both are recorded here as open findings for whoever next regenerates or hand-extends `RAW_ROWS`
(most likely as part of E6b, which needs a definitive answer to "what does `Native` mean for a
mutation-adjacent method" to build its decision match) rather than guessed at in a docs-only PR.

### Summary and what this means for E6b

- **~74 named intercept arms surveyed** across the two files (33-40 in `CallMethodMut` depending
  on whether the 6-string `hyperseq-*` call site is counted once or six times — see the taxonomy
  table; ~41-42 in `call_method_mut_with_values`), plus the 4 unnamed Tier-A helper functions in
  `vm_call_method_mut_ops.rs`.
- **Clean matches (case a):** `Str.subst-mutate`; `Hash.push`/`.append`; `List`/`Array`
  `push`/`pop`/`shift`/`unshift`/`append`/`prepend`/`splice`/`map` (the one non-mutator-family
  name that turned out genuinely mutating); `Blob`/`Buf` `push`/`pop`/`shift`/`unshift`/
  `append`/`prepend`/`splice`. `Hash.AT-KEY` and `List`/`Array.squish` are confirmed correctly
  **un**flagged (they never mutate).
- **Missing rows (case c):** `Pair.freeze`, `Match.make`, `ASSIGN-KEY`/`DELETE-KEY`/`BIND-KEY`/
  `BIND-POS` on every owner that has them, `SetHash.set`/`.unset`/`.grab`/`.grabpairs`,
  `Collation.set`, the whole `Blob`/`Buf` `write-bits`/`write-num*`/`write-int*`/`read-*`
  family, `Mix`/`Set`/`Bag.keyof` — plus several owners (`Lock`, `Promise`, `Channel`,
  `LazyList`, `HyperSeq`, `RaceSeq`, `Proxy`, `Iterator`, `Collation`) with zero rows at all.
- **Mislabeled rows (case b, found in both directions):** `BagHash`/`MixHash.grab` are `SPECIAL`
  but should arguably also/instead be `MUTATES_RECEIVER`; `List`/`Array`'s `grep`/`rotate`/
  `classify`/`categorize`/`rotor`/`produce`/`reduce` are `MUTATES_RECEIVER` but have no
  genuinely-mutating arm in either file (an over-claim, traced to the `is_array_method`
  allow-list's coarse text-co-occurrence origin, finding 2 above).
- **No RAW_ROWS edits landed** — both candidate corrections were left as recorded findings
  rather than guessed at (see above).

**None of this blocks E6b.** Design decision 1 already settled (in the E5b steps above, which
generalize past `CallMethod`) that the `Native` candidate is *hint-only* — the real
`try_native_method`/`try_native_method_raw` call is always self-guarding and stays a direct probe
at every entry, never replaced by a resolver decision. Since `native_row_servable` (the only
production reader of `MUTATES_RECEIVER`) is not consulted by any real dispatch path today, and
since every current `MUTATES_RECEIVER` row is arity-`N` (finding 4, already excluded from
`native_row_servable`'s "reachable" answer on arity grounds alone), none of this survey's findings
change what E6b needs to build: the mutation-aware decision match still routes on the `User`
candidate (already trustworthy, per E5b step 3) with `try_native_method` staying a direct,
self-guarding pre-check exactly as E5b concluded for the non-mut entries. The gaps found here are
a data-quality backlog for whoever eventually wants `native_row_servable` to become authoritative
for mutation-adjacent methods (relevant to a later Phase F cleanup, not to E6b's cutover shape).

**All of E6a is now closed.** Next: E6b.
