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
