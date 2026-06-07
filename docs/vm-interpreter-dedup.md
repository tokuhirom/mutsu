# Collapsing the VM ↔ Interpreter duplication

## Why this matters (the real cost is maintenance, not perf)

mutsu has **two engines**: the bytecode **VM** (`src/vm/`, with pure native ops in
`src/builtins/`) and the legacy tree-walking **Interpreter** (`src/runtime/`). For
many Raku operations — builtin functions, methods, operators, coercions — the
*same logic is implemented in both places*. The VM is meant to be authoritative;
the Interpreter is being eliminated.

The problem this document targets is **not** runtime cost (the interpreter
"bridge" rarely fires — fallback rates on normal code are ~0%). It is the
**maintenance hazard of duplicate implementations**:

- A fix applied to one copy is silently forgotten in the other → the two drift.
- Someone (human or AI) reads the codebase, finds the *wrong* copy, and "fixes"
  a place that isn't on the hot path — or can't tell which copy is authoritative.
- Two sources of truth for one behavior is a standing invitation to bugs.

So the goal is to **collapse each duplicated operation to a single
implementation** — keep the native one, delete the interpreter one — even when
the duplicate "isn't hurting performance." Fewer implementations of the same
thing is the win.

## How the interpreter reaches native code (the mechanism that makes deletion safe)

The interpreter's giant `Interpreter::call_function` match (`src/runtime/builtins.rs`)
has per-name arms (`"abs" => self.builtin_abs(...)`, …). Its catch-all is:

```rust
_ => self.call_function_fallback(name, &args)   // src/runtime/builtins.rs
```

and `call_function_fallback` (`src/runtime/builtins_operators.rs`) tries the
**native** table:

```rust
if let Some(native_result) = crate::builtins::native_function(Symbol::intern(name), args) {
    return native_result;
}
```

So when the interpreter itself evaluates Raku code (EVAL, regex embedded `{ }`
blocks, gather/react bodies, anywhere the interpreter is the carrier) and the
per-name arm is **removed**, the call falls through to the native implementation.
That is what lets us delete an interpreter arm + its `builtin_*` body without
breaking the interpreter-evaluated paths — **provided** native actually covers
the name through that fallback.

## Safe-deletion procedure (per duplicated builtin)

1. Confirm the name has a real `=> Some(...)` arm in `src/builtins/functions.rs`
   `native_function` (grep for `"<name>" =>`, not just the string).
2. Confirm the interpreter `builtin_<name>` has **no caller other than** the
   `call_function` match arm (`grep -rn "builtin_<name>\b" src/runtime/`).
3. **Verify behavioral equivalence through the fallback path**, not just the VM
   path: `mutsu -e 'say EVAL(q{<name>(...edge cases...)})'` must match `raku`.
   The VM path and the interpreter-fallback path are **different entry points**
   (`VM::try_native_function` vs `Interpreter::call_function_fallback` →
   `native_function`) and do **not** have identical coverage — see the gotcha.
4. Delete the arm and the `builtin_<name>` body (a `pub(super)` fn with no caller
   trips `clippy -D warnings`, so it must go, not just be orphaned).
5. `make test` + `make roast` — the interpreter copies are exercised by EVAL /
   regex / carrier paths across the suite, so the full roast run is the net.

### Gotcha found in practice (`chrs` / `ords`)

`chrs` and `ords` have native arms in `functions.rs`, but they are **not reached
through `call_function_fallback`'s `native_function` call** (multi-arg /
array-returning dispatch differs from the single-`arg` fallback table). Deleting
their interpreter arms made `EVAL(q{chrs(72,105)})` fail with *"Unknown function:
chrs"*. They were **kept**. Lesson: step 3 (EVAL-path verification) is mandatory;
a native arm existing is necessary but not sufficient.

## Progress

- **Done (Slice 6.3 dedup, first batch)** — deleted the interpreter copies of 9
  pure value builtins, all verified equivalent through the EVAL fallback path and
  `make roast`: `abs`, `lc`, `uc`, `tc`, `trim`, `flip`, `chr`, `ord`, `chars`.
  Kept `chrs`/`ords` (fallback-unreachable native, see gotcha).

## Remaining duplication (prioritized)

Audit (2026-06) found ~23 builtins duplicated between `src/builtins/functions.rs`
and `src/runtime/builtins*.rs`, plus method and operator duplication.

- **Category A — pure value builtins, delete-and-fallthrough** (low risk; the
  above batch + any stragglers). Each needs the EVAL-path check.
- **Category B — genuine forks** (need real consolidation, not a delete):
  `min`/`max`/`sort`/`reverse`/`join`/`flat`/`first`/`elems`/`index`/`rindex`.
  The native copy bails (returns `None`) on hard cases (a comparator block, a
  `LazyList`, junction threading) and the interpreter copy handles them. To
  collapse these, the native side must learn the hard cases first, *then* the
  interpreter copy can be deleted. Multi-PR.
- **Category C — methods** duplicated across `src/builtins/methods_*` (native
  fast path) and `src/runtime/methods.rs` / `methods_mut.rs` (slow path), and
  operator/arith/coercion logic across `src/builtins/arith.rs` /
  `src/vm/vm_arith_ops.rs` vs `src/runtime/`. Larger, do after the function
  forks.

The end state is the legacy `Interpreter` method/function execution paths
deleted entirely (PLAN.md final goal); the `env_dirty` dual-store flag then falls
out because the only remaining by-name arbitrary writer (the interpreter bridge)
is gone. Until then, prefer **deleting a confirmed-redundant interpreter copy**
over adding a third place to keep in sync.

## Category C progress — arith / operator / coercion (2026-06-07)

Phased, low-risk-first (user-approved); detection by **manual audit** of
`should_bypass_native_fastpath` (`methods_native_bypass.rs:105`) + native
coverage, each change gated on EVAL equivalence + `make roast`.

### Phase 1a — reduction `%`/`mod` → native `arith_mod`
`runtime/ops.rs::apply_reduction_op` reimplemented `%`/`mod` locally (Int + f64
only, soft divide-by-zero `Failure`), duplicating `builtins::arith_mod`. Replaced
the two local arms with a delegation. **2 duplicate arms removed**; also a
correctness fix — `[%] 2**70, 3` was lossy via `to_int` (now exact), and
`[%] 5, 0` now throws like `raku` / the `%` operator instead of a soft Failure.

### Phase 1b — unify Instance→numeric bridge coercion
Of 4 numeric-coercion helpers, two were duplicates of the *Instance→numeric
bridge*: VM `coerce_numeric_bridge_value` (full: Failure throw, Match, has_user_method,
~25 arith/comparison call sites) and interpreter `coerce_infix_operand_numeric`
(simplified copy, 2 callers). Made `coerce_infix_operand_numeric` the single
authoritative impl (expanded; dispatch via `call_method_with_values`); the VM
helper delegates to it. **1 duplicate removed.** `utils::coerce_numeric(l,r)` and
`coerce_to_numeric(v)` are already single-impl (shared by VM + native) — not
duplicates, left as-is.

### Phase 2 — dead method copies + operator-body dedup
**Audit finding**: the "dead simple-value method copy" harvest is **largely
already done** — simple 0-arg value methods (`flip`/`fc`/`succ`/`pred`/`abs`/
`sign`/`sqrt`/`floor`/`ceiling`/`round`/…) have **no** interpreter dispatch copy.
What remains in `dispatch_method_by_name_{1,2,3}` is **live** (Instance-only forks
that coerce + re-dispatch e.g. `dispatch_trig_instance_method`; Buf/Blob
`X::Buf::AsStr` special-cases; Failure-explosion exclusion lists; Promise/Supply/
Match methods; Category-B comparator/lazy forks). So Phase 2's real remaining
duplication is **operator-body reimplementation** in `apply_reduction_op` (which
is itself authoritative — the VM delegates to it via `vm_dispatch_helpers.rs:115/453`
etc.).
- [x] **`~` (concat)**: `apply_reduction_op`'s `~` arm reimplemented Buf~Buf +
      a `format!` fallback lacking the VM's Buf~non-Buf decode and non-ASCII NFC
      normalization. Made `VM::concat_values` a state-free `pub(crate)` assoc fn
      and delegated both the VM `~` op and the reduction `~` arm to it. **1
      duplicate removed**; fixes a latent bug (`[~]` now NFC-normalizes like `~`).
- [x] **`minmax`**: `apply_reduction_op`'s `minmax` arm had its own `range_bounds`
      closure that (unlike the VM's `minmax_bounds_of_value`) did **not** recurse
      into elements, so `[minmax] (1,5),(2,8)` etc. computed wrong bounds. Made
      `vm_misc_ops::minmax_bounds_of_value` a `pub(crate)` fn (module exposed
      `pub(crate)`) and delegated the arm to it. **1 duplicate removed** + bug fix
      (`[minmax]` over nested arrays/ranges now matches raku, e.g. `0..8`, `1..9`).
- [ ] Remaining `apply_reduction_op` bodies: logic short-circuit `&&`/`||`/`//`
      are the reduction-only impl (the VM compiles infix short-circuit to jumps, so
      not VM-op duplicates); comparisons already use shared `runtime::compare_values`.
      Likely little left here.
- [ ] Genuine-fork methods → fold into native (Category B style; depends on the
      `%`-chain block-dispatch blocker noted under Category B in PLAN.md).
