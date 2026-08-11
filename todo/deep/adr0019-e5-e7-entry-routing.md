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
