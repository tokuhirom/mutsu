# §D(b) — tree-walk method-dispatch removal plan

**Status: design / measurement substrate (2026-06-26).**
Goal of §D(b) (PLAN.md §2-D): structurally remove the tree-walking dispatch chain so
the bytecode VM is the *only* execution engine for user code. This memo scopes that
work from measurement, not speculation.

## 1. What "tree-walk dispatch chain" actually means

The Explore mapping of the method-dispatch chain (VM opcode → catch-all → interpreter
slow path) established a key fact: **the only genuine AST tree-walking of *user code*
is `run_instance_method` / `run_instance_method_resolved` in `src/runtime/class.rs`**
(they end at `run_block(&method_def.body)`, class.rs:1300). Everything else in the
chain — `dispatch_method_by_name_1/2/3`, `dispatch_new_and_constructors`,
`dispatch_instance_and_fallback`, the `dispatch_*` family, every `call_native_*` — is
**native Rust** that merely *routes* by name/arity. Those stay; only the user-body
tree-walk is the removal target.

So §D(b) reduces to: **make every user method body reach `dispatch_compiled_method`
(compiled bytecode), so `run_instance_method[_resolved]` is never entered, then delete
it** (and the now-unreachable `dispatch_instance_and_fallback` user-method arm).

## 2. How `run_instance_method` is still reached today

The catch-all (`vm_call_method_compiled.rs::try_compiled_method_or_interpret_inner`)
already resolves + compiles user methods well: it tries the fast method cache, the
monomorphic/`method_resolve_cache`, and `populate_uncompiled_method` (compile-on-demand
for methods added after the registration compile pass). A synthetic
`multi method` / plain `method` / proto test produces **zero** tree-walk events — they
all dispatch as compiled bytecode.

Tree-walk is still entered via:

1. **`dispatch_instance_and_fallback` (methods_instance_ops.rs:867)** — `if
   self.has_user_method(class, method) { run_instance_method(...) }`. Reached when the
   catch-all's compiled dispatch did **not** fire (resolver returned `None`, or the
   resolved def had no `compiled_code` and `populate_uncompiled_method` failed), and the
   slow path's simpler name-based `has_user_method` still finds the method.
2. **Internal `call_method_with_values(...)` redispatch** that bypasses the compiled
   catch-all entirely. Examples in the slow path itself: the numeric bridge
   (`.Numeric`/`.Bridge`/`.log`, methods_instance_ops.rs:853-864), instance render in
   `say`/`print`/`gist`, `.COERCE`. Any internal `self.call_method_with_values(user_obj,
   "...")` lands straight in the tree-walk slow path.
3. **Private-method slow path** (methods_instance_ops.rs:106, direct
   `run_instance_method_resolved`) when the compiled private fast path (vm_call_method_
   compiled.rs:387) did not fire.
4. **Construction submethods** `BUILD`/`TWEAK` run during `.new`.

## 3. Measurement (MUTSU_VM_STATS `tree-walk method bodies` counter, full whitelist)

A new counter `record_tree_walk_method` (vm_stats.rs) fires at `run_instance_method`
entry. Across the full roast whitelist (1285 files), **only ~81 files** produce any
tree-walk method body execution; **total ≈ 10983 events**, distributed:

| method (name)                        | events | category |
|--------------------------------------|--------|----------|
| `m`                                  | 10005  | **`samewith` method redispatch — driver: `S12-methods/defer-next.t`** |
| `Bridge` / `Numeric` / `Int` / `Num` / `Rat` / `COERCE` | ~405 | numeric-bridge + coerce redispatch (cat 2) |
| `Str` / `gist`                       | ~59    | render redispatch / landmine (cat 2) |
| `abs` / `succ` / `doit` / `foo` / `mul` / `test_method` / `prime` / `hi` / `cool` / … | ~250 | misc user methods (cat 1) |
| `BUILD` / `TWEAK` / `new`            | ~60    | construction submethods (cat 4) |
| `pull-one` / `skip-one` / `count-only` | ~34  | user-defined Iterator protocol (lever B/Phase2 adjacent) |
| `find_method` / `accepts_type` / `CALL-ME` / `name` | ~21 | custom HOW / Metamodel (cat 1, MOP-ish) |
| `FALLBACK`                           | ~10    | `method FALLBACK` (missing-method handler) |
| `print`                              | ~65    | user `print` method redispatch |

**Conclusion: `run_instance_method` is already nearly vestigial.** 91% of all tree-walk
is a single method name `m`; excluding it, the residual is ~978 events across diverse,
mostly-niche categories.

### 3a. The `m` driver — `samewith` method redispatch (the deep 91%)

`S12-methods/defer-next.t` accounts for ~10003 of the 10005 `m` events, from one
construct amplified by a loop:

```raku
multi method m(Int $x) { samewith $x.Str }
multi method m(Str)    { }
multi method call()    { self.m(42) }
C.call for ^10000      # 10000 iterations × (m(Int) + samewith → m(Str))
```

The test **passes** — this is correct-but-tree-walked, not a bug. The `samewith`
redispatch (and `nextsame`/`callsame`/`nextwith`) of a `multi method` runs through the
interpreter's **`method_dispatch_stack`** machinery (`dispatch.rs` ~1631-1670,
`builtins_dispatch_next.rs`), which tree-walks each candidate body via
`run_instance_method`. This is **exactly the blocker PLAN.md §2-D already flagged**
("nextsame+rw チェーン … method 経路 … 単一スライス不可・§C の env↔locals/VM-frame cell
共有を redispatch 境界へ広げる substrate 作業（要設計）", PLAN.md:193-209). So 91% of
tree-walk *volume* is this one deep substrate; it is the **capstone**, not the first
slice.

## 4. Slice plan (tractable-first, not volume-first)

The high-volume `m` case is the *deepest* (method redispatch substrate). Drain the
tractable categories first; they share the same structural lever.

- **Structural lever (the core fix):** make `call_method_with_values` (the slow-path
  entry, `runtime/methods.rs:310`) try the **compiled** method path for a user method on
  an Instance *before* descending to `dispatch_instance_and_fallback` →
  `run_instance_method`. Today every *internal* redispatch (coercion `.Bridge`/
  `.Numeric`, render `.gist`/`.Str`, `.COERCE`, and any `self.call_method_with_values(
  user_obj, …)`) bypasses the compiled catch-all and tree-walks. Routing user-method
  calls here through `resolve_method_with_owner_invocant` + `dispatch_compiled_method`
  (the same machinery `try_compiled_method_or_interpret` uses) drains cat 1/2/4 at once.
  Risk: `call_method_with_values` has many callers with slow-path expectations — gate to
  the safe case (Instance receiver, resolves to a compilable single/non-redispatch user
  method) and keep `run_instance_method` as the fallback for everything else.

### 4a. Slice 1 ATTEMPTED + REVERTED — the lever is blocked on env/writeback coherence

Slice 1 was implemented (route a resolved, compilable, non-multi user method at the
tree-walk site `methods_instance_ops.rs:867` through `dispatch_compiled_method` instead
of `run_instance_method`) and **reverted**. Findings (the real substrate, confirmed
empirically — this is *why* PLAN said "要設計"):

- **`dispatch_compiled_method` is NOT byte-identical to `run_instance_method` for a
  method that mutates outer state.** It commits the receiver's *attributes* and fetches
  a returned Proxy identically, but when invoked from this deep internal-redispatch
  context it does **not** link the method body's **captured-outer / closure env** back
  to the caller's lexicals. A method like `method Numeric { $calls++; $.x }` (closing
  over a `my $calls`) **loses the `$calls++` write** — the counter stays 0. The
  tree-walk `run_instance_method` sets up the env so that write propagates.
- The general (un-gated) version additionally broke **junction-invocant eigenstate
  writeback** (`class { method a { $n1++ } }` on `JC1 | JC2`, t/junction-invocant-
  autothread-writeback-coherence.t) and **reduce-time dynamic-var scoping** in grammar
  action methods (`method delim { $*L = '<' }`, t/grammar-reduce-time-dynvar.t).
- Even a **name-gated** version (only `Numeric`/`Bridge`/`Int`/`Num`/`Rat`/`Real`)
  passed `make test` + 228 roast files, but a focused pin (`method Numeric { $calls++ }`)
  exposed the same captured-outer write loss — so it is a *latent* correctness bug, not
  safe to ship.

**∴ the real §D(b) substrate is not the dispatch routing — it is extending the
captured-outer / closure-env writeback coherence (the `docs/captured-outer-cell-sharing.md`
machinery) to the compiled-method redispatch boundary**, so that a method run via
`dispatch_compiled_method` propagates writes to closed-over caller lexicals, junction
eigenstate `our`-vars, and reduce-time dynamic vars exactly as `run_instance_method`
does. Until that lands, `run_instance_method` stays. This mirrors the function-side
`nextsame+rw` blocker (PLAN §2-D): same root cause (writeback across a redispatch
frame boundary), now confirmed for the method-coercion redispatch path too.

- **Slice 1a (revised) — DONE (#3615): coercion-redispatch writeback coherence.**
  The reverted Slice 1's premise (that `dispatch_compiled_method` fails to *link* the
  callee frame to the caller env) was **wrong about the mechanism**. The captured-outer
  write *does* reach the caller env: `call_compiled_method`'s `merge_method_env`
  propagates it and records the changed caller-visible names into
  `pending_rw_writeback_sources` (vm_method_dispatch.rs:682). The break is purely that an
  **internal coercion redispatch has no surrounding CallMethod op to drain that list**
  into the caller's local *slot*, so the slot stays stale (env is correct, slot is not —
  the dual-store gap). Fix = drain at each op-level coercion redispatch site using the
  existing `current_code` + `reconcile_caller_after_lazy_force` machinery (the same
  pattern `say`/`note` already use for a `.gist` closure). Sites covered:
  `exec_num_coerce_op` (`+$obj`), `exec_str_coerce_op` (`~$obj`, `.Stringy`/`.Str`),
  `coerce_numeric_bridge_value` (infix arith + numeric comparison coercion), and
  `eval_truthy` (`if $obj`/`?$obj` Bool). The three reverted tests (junction-invocant /
  grammar-dynvar / `$calls++` coercion) all pass — no closure-env rooting was needed.
  pin=`t/coercion-method-captured-writeback.t`(10).
- **Slice 1b (render redispatch) — DONE (#3617): string-render writeback coherence.**
  The same drain applied to the user-`.Str`/`.Stringy` render sites that, like the
  coercion ops, run a user method with no surrounding CallMethod op: `exec_string_concat_op`
  (interpolation `"…$obj…"`), `exec_put_op` (`put`), `exec_print_op` (`print`). `say`/
  `note` already did this for `.gist`. pin=`t/render-method-captured-writeback.t`(8).
  **★Critical subtlety — retain-on-miss:** an internal redispatch can fire *inside another
  method body that has not yet returned* (e.g. a `submethod BUILD`'s `$gather ~= "($a)"`
  interpolation runs while a *sibling* BUILD's captured-outer write `$parent-counter++` is
  still queued in `pending_rw_writeback_sources` for the outer `.new` call site to drain).
  The drop-on-miss `apply_pending_rw_writeback` would *consume and discard* that sibling
  write here (its slot is in the outer frame, not the BUILD frame), so the `.new` drain
  finds nothing → caller slot stays stale (roast `S12-construction/BUILD.t` "Called
  Parent's BUILD method once" caught this). Fix = a dedicated
  `reconcile_caller_after_internal_dispatch` that **retains** a miss instead of dropping
  it. ALL the op-level internal-redispatch reconciles (this slice's render sites + #3615's
  coercion sites + `say`/`note`) use it; only the genuine lazy-force reify keeps the
  drop-on-miss `reconcile_caller_after_lazy_force` (its pending entries are always its own
  callee's).
  **`sprintf` `%s` user-`.Str` dispatch — DONE (#TBD).** `sprintf("%s", $obj)` rendered the
  object's `.gist` (`S()`) instead of dispatching a user-defined `.Str` (Rakudo → the
  `.Str` result). `builtin_sprintf` is now `&mut self` and pre-stringifies each `%s`-directive
  Instance/Package arg (located via the new `sprintf_str_arg_indices` directive walk) by
  calling its user `.Str` (`.Str` only — Rakudo `%s` does NOT use `.Stringy`; verified the
  gist falls through). The captured-outer writeback drains through the surrounding `sprintf`
  call op's existing `apply_pending_rw_writeback`, so no manual drain is needed (the runtime
  builtin records the write; the VM call op consumes it). mutsu calls `.Str` once per use;
  Rakudo's double-call-per-sprintf quirk is not reproduced (pin asserts the value + that the
  write propagates, not the exact count). pin=`t/sprintf-user-str-dispatch.t`(8, raku-validated).
  **`sprintf` numeric directives — DONE (#TBD).** Generalized the same mechanism to numeric
  directives: integer (`%d %i %u %b %o %x %X %c`) → user `.Int`, float (`%e %f %g`) →
  user `.Numeric` (the `sprintf_str_arg_indices` helper became `sprintf_arg_specs`, returning
  `(arg_index, spec)`). `%d` prefers `.Int` over `.Numeric` (Rakudo behavior). The coerced
  numeric value (not a string) replaces the arg so the formatter renders it. pin=
  `t/sprintf-numeric-coerce.t`(10, raku-validated).
  **Remaining same-shape sites (deferred, lower traffic):** `.fmt` `%s`
  formatting drops the `.Str` writeback too; and `value ~~ $instance`
  / `$instance ~~ (key => …)` do not even *dispatch* the user `ACCEPTS`/key method today
  (`smart_match` bottoms out at `(_, Instance) => false`), so they are a missing-dispatch
  bug, not a writeback one — out of scope for this slice.
- **Slice 1c (next) — generalize the drain to the remaining `call_method_with_values`
  internal redispatches** (the numeric bridge `.Bridge`/`.Int`/`.Real`, `.COERCE`),
  each at its own op-handler boundary with `current_code` + `reconcile_caller_after_lazy_force`.
  THEN the coerce/render *routing* (original Slice 1: route tree-walked
  `dispatch_instance_and_fallback` user methods through `dispatch_compiled_method`)
  becomes safe, because the drain point exists.
- **Slice 2 — generalize the lever to all user-method calls — DONE (#3645).** The
  `run_instance_method` wrapper (reached for multi-method dispatch and `samewith`
  re-dispatch through `call_method_with_values` → `dispatch_instance_and_fallback`,
  e.g. roast S12-methods/defer-next.t method `m` ×10000) executed the resolved candidate
  through `run_instance_method_resolved`, which `run_block`-recompiles the body on every
  call. It now runs the candidate via the cached VM-native `call_compiled_method` when
  **writeback-safe** (~2.3× faster on a 50k multi-method+samewith loop). The MRO frame is
  already pushed in `run_instance_method`, so the candidate's own redispatch continues the
  chain. **Writeback-safety gate** (else keep the env-merging tree-walk): body writes no
  captured-outer (free) vars; not a submethod; not a delegation forwarder. The pending
  writeback list is saved/restored around the call (it clears on entry) so a sibling
  BUILD write survives a nested `.new`; the returned attrs are the live cell unless
  `:=`-adjusted (no clobber of an in-place `self.attr ~= …`). pin=
  `t/multi-method-compiled-dispatch.t`(6). **★The `record_tree_walk_method` counter fires
  at `run_instance_method` ENTRY (resolution), not at execution, so it does NOT drop after
  this conversion — measure the win by wall-clock, not the counter.**
- **Slice 3 — construction submethods (BUILD/TWEAK) — DONE (#3649, #3651, #3652).** Class
  BUILD/TWEAK (#3649: dropped the `!is_submethod` gate), role-composed BUILD/TWEAK + DESTROY
  (#3651), and a second custom-constructor BUILDALL path's role BUILD/TWEAK (#3652) now run
  compiled. The `fail`-inside-BUILD → Failure gap (`call_compiled_method` converts `fail`
  to an unhandled Failure *value*, but construction sites drive off `Err(is_fail)`) is
  bridged by re-raising an unhandled-Failure **submethod** result as `Err(is_fail)`, scoped
  to submethods. Attribute-threading commits the live cell unless `:=`-adjusted; the
  free_var_writes gate keeps captured-outer-writing BUILDs on the tree-walk path
  (sibling-writeback / #3620); `pending_rw_writeback_sources` is saved/restored around the
  call. ~1.4× faster on a 50k-construction loop. pins=`t/construction-submethod-compiled.t`,
  `t/role-submethod-destroy-compiled.t`.
  **★Shared helper (#3651): `run_resolved_method_compiled_or_treewalk`** encapsulates the
  whole gate+compiled-execute+fail-re-raise+adjusted-commit+save/restore logic with the
  same `(result, attrs)` contract as `run_instance_method_resolved`, so every
  resolved-candidate site is a drop-in. Private methods (`$obj!m`), the `.*` walk (#3652),
  and `does`/`but` mixin role methods (#3655) were converted via it too.
  **★§B method-execution-compiled migration COMPLETE:** every resolved-candidate
  method/submethod dispatch runs as cached compiled bytecode; the only interpreter-bound
  method execution left is the synthesized `proto` body (`compiled_code = None`). **Next
  phase = remove the resolution+setup overhead**: `run_instance_method` is still ENTERED
  per call (resolve + frame push + env clone) even though execution is now compiled — the
  `record_tree_walk_method` counter (entry, not execution) still shows the hot names
  (`m`/`BUILD`/`doit`). VM-native multi-method resolution caching (so a `CallMethod` op
  resolves+dispatches a multi/submethod without entering `run_instance_method`) is the
  larger remaining win, plus eventually deleting `run_instance_method_resolved`.
  **★Pre-existing blocker — BUILDALL sibling writeback clobber — FIXED (#3620).** A
  *nested method dispatch inside one BUILD clobbered a sibling BUILD's captured-outer
  writeback*:
  ```raku
  my $pc = 0;
  class P { submethod BUILD { $pc++ } }
  class C is P { submethod BUILD { my $x = S.new } }   # any nested dispatch
  C.new; say $pc;   # raku: 1   mutsu before: 0
  ```
  **Root cause (confirmed by MUTSU_DBG tracing of `apply_pending_rw_writeback`):** the
  method-call op-tail drain is **drop-on-miss**. BUILDALL runs Parent.BUILD (records `$pc`
  into `pending_rw_writeback_sources`, owned by the outer `.new` caller's frame), then a
  nested `S.new` inside Child.BUILD reaches that drain at a frame where `$pc` is NOT a
  slot (`find_local_slot == None`) → drop-on-miss discards it; the outer `.new` drain then
  finds nothing. (Earlier hypothesis — the `call_compiled_method` entry `clear()` — was
  WRONG; BUILD writes via the `run_instance_method_resolved` env merge, and the loss is at
  the nested call's drain, not the clear. **Lesson: locate the actual drain site by
  tracing before hypothesising.**) **Fix:** when `apply_pending_rw_writeback` finds a
  source whose slot is not in the current frame, MOVE it to the retain-on-miss
  `pending_caller_var_writeback` list instead of dropping it, so the owning frame drains
  it. pin=`t/build-sibling-writeback-coherence.t`(6). This was the same retain-on-miss
  principle as Slice 1b, now applied to the real method-call op tail, and it unblocks the
  nested-dispatch-inside-BUILD shape for coercion/render (and a future index-coercion).
- **Slice 4 (capstone) — `nextsame`/`callsame` method redispatch — DONE (#3640).** The
  `method_dispatch_stack` candidate dispatch in `dispatch_next_candidate` ran the next MRO
  candidate through `run_instance_method_resolved`; it now uses `call_compiled_method` when
  the candidate has compiled code (the active MRO frame stays in place, so a further
  `nextsame` continues the chain). Commits the attr snapshot only when `attrs_adjusted`
  (else the shared cell already holds the in-place mutation). The §D nextsame+rw chaining
  (slot writeback via `current_code`) is preserved. (`samewith` re-dispatch is covered by
  Slice 2, since it routes through `call_method_with_values`.) pin=
  `t/method-redispatch-compiled.t`(8); `t/nextsame-rw-redispatch.t` still green.
  **★DONE — nextsame+rw redispatch (2026-06-26, #TBD).** Both the function path
  (`pr($v)` → **1011**) and the method path (no longer dies `X::Parameter::RW`, → **1011**)
  now chain the rw write through the redispatch, including `callsame` continuations
  (first candidate resumes and reads the chained value, → **1016**) and 3+ candidate
  chains (→ **1106**). pin=`t/nextsame-rw-redispatch.t`(11, validated against raku).
  **How it landed** (close to the scoped plan, with two refinements found while building):
  (1) `multi_dispatch_stack`'s tuple gained a 4th element — NOT the raw arg sources, but
  the FIRST (winning, compiled) candidate's scalar rw params as `Vec<(positional_index,
  param_name)>` (`MultiDispatchEntry`, `rw_scalar_positional_params`). The first
  candidate's param name (not the caller source) is what locates its VM slot for the
  writeback, so it is what must be stored; it stays FIXED across the chain (always the
  first candidate's slots). `MethodDispatchFrame` gained the same `rw_params` field.
  (2) In `dispatch_next_candidate`, before the redispatch: read each rw param's CURRENT
  value from `env[first_param]` (the body-mutated live value, e.g. 11), rebuild
  `call_args[pos]` with it (keeping the varref source for the function path; the method
  path has none, so `set_pending_call_arg_sources` names the source). (3) AFTER the
  redispatch: the chain's final value is in env; write it back into the first candidate's
  VM local slot via `self.current_code` (the live ancestor compiled frame) AND into
  `env[first_param]` — the slot so the exit flush reads the chained value (not the
  candidate's own pre-nextsame value, the §C clobber), the env so a `callsame` body that
  resumes after the redispatch reads the chained value by name. The function path routes
  the next candidate's writeback through the caller source (varref name); the method path
  routes it through the first candidate's param name (`source = first_param`), because
  method args carry no varref. **Refinements vs the plan:** the stored element is the
  first candidate's *param name* (for slot location), not the caller arg source; and the
  fix is NOT "prevent the exit flush from clobbering" but "update the slot the flush reads
  to the chained value", which composes cleanly with `callsame` continuations and N-level
  chains. The prior reverted 4-tuple attempt failed because it wrote env only; the slot is
  what the compiled exit flush reads. Guarded entirely behind `!rw_params.is_empty()` so
  non-rw `nextsame`/`callsame`/`callwith` are byte-identical. Out of scope (unchanged):
  `nextwith`/`callwith`+rw (override args; raku itself rejects a literal into an rw param).
- **Out of scope (stay / separate axis):** user `Iterator` `pull-one`/`skip-one`/
  `count-only` (lever B / Phase 2), custom-HOW Metamodel methods (`find_method`/
  `accepts_type`/`name` — MOP reflection), `method FALLBACK` (missing-method semantics).
  These gate the *final deletion* of `run_instance_method`, but draining Slices 1-4
  removes >99% of tree-walk volume and all the high-traffic categories.

## 5. Risks / open questions

- Whether `populate_uncompiled_method` fails for some body constructs (closures over
  caller env, `nextsame`/`callsame` in a method, `where`-constrained candidates) — if
  so those need the compiled binding path extended (mirrors the function-side multi OTF
  work, ledger §D multi-dispatch).
- The internal-redispatch sites (cat 2) must preserve the caller `self`/env exactly
  (the catch-all's `try_compiled_method_or_interpret` save/restore of `self`).
- byte-identical is the bar: each slice keeps `run_instance_method` as the fallback for
  whatever it does not yet route, so a missed case degrades to today's behavior, not a
  regression.
