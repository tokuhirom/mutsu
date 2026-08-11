# Audited every inline closure-body exec site for the missing-upvalue-array bug class

`compute_upvalues` rewrites an anonymous closure body's pure read of a
read-only scalar free variable to `GetUpvalue { index, name_idx }`, an index
into THE CLOSURE'S OWN `upvalue_syms`. The array is normally installed
per-call by closure dispatch (`self.upvalues = data.upvalues.clone()`). Any
code path that executes a closure's compiled ops *without* going through
that dispatch leaves `self.upvalues` pointing at whatever the enclosing
frame installed — if that array happens to hold `Some(cell)` at the same
index, the block silently reads an unrelated capture instead of its own.
This bug class was first confirmed and fixed at two sites during ADR-0025
slice 1 (`exec_protect_block_inline`, `call_protect_block` —
`$l.protect({ $r += $i })` read the enclosing Promise-closure's Lock cell as
`$i`). This entry audits every other "exec a Sub's compiled code inline"
site in the interpreter for the same gap.

## Verified already safe

- `resolution_map_grep.rs` / `resolution_map_grep_rw.rs`'s eager `.map`/
  `.grep` loops (all three `run_reuse` call sites) run inside a
  `with_nested_registers` closure, which already resets `self.upvalues` to
  empty on entry and restores the outer array on exit — regardless of how
  the closure returns. An out-of-range `GetUpvalue` index falls back to a
  by-name env read, which is always correct here since the loop's own
  `env_mut().insert(...)` bindings and the enclosing frame's live env are
  the same env this inline exec shares directly. Verified with a
  synthetic collision repro (an enclosing closure with a boxed upvalue at
  index 0, an inline `.map({ $free-scalar })` block whose own first
  upvalue is a different name): output matches `raku` exactly.
- `run_nested` (and its callers `run_compiled_block`, `run_compiled_block_raw`,
  the top-level EVAL/dies-ok/lives-ok carriers) also goes through
  `with_nested_registers` — same protection, transitively.
- `vm_given_when_ops.rs`'s inline `given`/`when` body loop and the JIT
  `step` shim (`vm_jit_helpers.rs`) execute a *slice of the same compiled
  unit's own bytecode* mid-frame, not a separate closure's code — there is
  no dispatch boundary to bypass, so `self.upvalues` is already correct by
  construction.

## Fixed

- `vm_arith_int_ops.rs`'s `vm_xx_repeat_thunk` (the `EXPR xx N` list-repeat
  fast path for a re-evaluated thunk, e.g. `rand-thing() xx 3`) called
  `run_reuse` directly on `self`, with no register reset at all — the exact
  unguarded shape as the original protect-block bug. Now saves/installs/
  restores `data.upvalues` around the repeat loop (both the success and the
  early-error exit path).
- `vm_helpers_lazy.rs`'s `force_lazy_list_vm_inner` and
  `vm_helpers_lazy_pull.rs`'s `force_lazy_list_vm_n_inner` — the VM-native
  `gather`/`take` forcing paths — ran a `LazyList`'s compiled gather body via
  direct `exec_one` calls with no upvalues handling either. Unlike the other
  fixed sites, `LazyList` carries no upvalue array of its own (its captures
  live in `list.env`, installed as the run's scoped/resumed env), so the fix
  resets `self.upvalues` to empty for the duration rather than installing a
  substitute array — matching exactly what `with_nested_registers` already
  does, and safe for the same reason (the by-name env fallback is always
  correct once the gather body's own env is in place).

Neither of the two new gather sites had a synthetic repro that showed a
behavior difference against the pre-fix binary (unlike the `xx`-thunk site,
where a repro was also attempted but likewise did not reproduce a visible
difference) — these are proactive hardening against the documented bug
class, not confirmed-regression fixes. All three are minimal, structurally
identical to the already-proven-correct fix at the protect-block sites, and
verified against the full local suite (`make test`: 3013 files / 28241
tests) plus the targeted `t/*gather*.t`, `t/*lazy*.t`, `t/*xx*.t`, and
`t/lock-protect-shared-scalar.t` clusters (69 files / 705 tests) with no
regressions.

## Deferred

The ticket's proposed structural fix — an RAII guard (swap-on-enter,
restore-on-drop) around every "exec this cc inline" helper, making
"`self.upvalues` always belongs to the currently-executing cc" a property
enforced once rather than per-site — remains open, flagged in the ticket as
a candidate for a future ADR-0025 slice-2 session now that more sites are
inventoried.
