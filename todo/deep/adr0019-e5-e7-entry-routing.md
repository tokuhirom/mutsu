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
