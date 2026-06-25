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

- **Slice 1a (revised) — DONE (#TBD): coercion-redispatch writeback coherence.**
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
- **Slice 1b (next) — generalize the drain to the remaining `call_method_with_values`
  internal redispatches** (the numeric bridge `.Bridge`/`.Int`/`.Real`, `.COERCE`,
  render `.gist`/`.Str` reached via paths other than `say`/`note`), each at its own
  op-handler boundary with `current_code` + `reconcile_caller_after_lazy_force`. THEN the
  coerce/render *routing* (original Slice 1: route tree-walked `dispatch_instance_and_
  fallback` user methods through `dispatch_compiled_method`) becomes safe, because the
  drain point exists.
- **Slice 2 — generalize the lever** to all user-method calls in
  `call_method_with_values` (cat 1 misc user methods abs/succ/foo/…, and the
  catch-all-miss path feeding `dispatch_instance_and_fallback:867`).
- **Slice 3 — construction submethods (BUILD/TWEAK/new, ~60).** Run `.new`'s BUILD/TWEAK
  as compiled bytecode.
- **Slice 4 (capstone) — `samewith`/`nextsame`/`callsame` method redispatch (the `m`
  91%).** Compile the `method_dispatch_stack` candidate dispatch. Coordinate with the
  PLAN nextsame+rw substrate (rw-param slot coherence across the redispatch boundary).
  This is the deep one; do it last, after the volume from Slices 1-3 confirms the
  approach and after the function-side multi-redispatch lessons.
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
