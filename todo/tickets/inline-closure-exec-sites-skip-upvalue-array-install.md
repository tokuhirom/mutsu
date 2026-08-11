# Inline closure-execution sites skip installing the closure's upvalue array — GetUpvalue reads the ENCLOSING closure's captures on index collision

## The bug class (root-caused 2026-08-11, during ADR-0025 slice 1)

`compute_upvalues` rewrites an anonymous closure body's pure read of a
read-only scalar free variable to `GetUpvalue { index, name_idx }`. The
index is into THE CLOSURE'S OWN `upvalue_syms`. The array itself is
installed per-call by closure dispatch (`vm_closure_dispatch.rs:668`,
`self.upvalues = data.upvalues.clone()`), and `capture_upvalues` freezes
`Some(...)` ONLY for `ContainerRef` cells (non-cells stay `None` → env
fallback by name).

Any code path that executes a closure's CompiledCode **without going
through closure dispatch** leaves `self.upvalues` pointing at whatever the
enclosing frame installed. A `GetUpvalue` in the block body then indexes
the WRONG closure's array: if the enclosing array happens to hold a
`Some(cell)` at that index, the block reads an unrelated capture — silently.

Observed concrete failure (fixed): `$l.protect({ $r += $i })` — the protect
block's `GetUpvalue(0)` meant `$i`, but the inline protect executor left the
enclosing Promise-closure's array installed, whose slot 0 was the boxed
`$l` Lock cell → `$i` evaluated to the Lock, `$r += Lock` accumulated
nothing (`t/lock-protect-shared-scalar.t` 1-2 went `r=0`). The failure was
LATENT before ADR-0025 slice 1 because upvalue arrays held `Some` only for
the rare boxed cells; Instance boxing made cells common enough to collide.

## Fixed sites (2026-08-11, same branch as ADR-0025 slice 1)

- `exec_protect_block_inline` (`vm_call_method_compiled.rs`) — swap in
  `sub_data.upvalues` around the inline exec loop.
- `call_protect_block` (`resolution_eval.rs`) — same swap around
  `run_compiled_block`.

## Sites still to audit (same pattern: exec a Sub's cc outside dispatch)

- Eager `.map`/`.grep` loops: `resolution_map_grep.rs:460/530/723`,
  `resolution_map_grep_rw.rs:269/534` — `vm.run_reuse(&code, ...)` inside
  `with_nested_registers`, with `data` (the SubData) in scope; the fix is
  the same `std::mem::replace(&mut vm.upvalues, data.upvalues.clone())`
  swap for the loop duration.
- `vm_arith_int_ops.rs:160` — another `run_reuse` of a block cc.
- Any other `run_reuse`/`run_nested` caller executing a closure body
  (grep for `run_reuse(` / `run_compiled_block` and check whether the cc
  can contain `GetUpvalue`, i.e. is an anonymous-closure body rather than
  a statement list compiled fresh).

Repro sketch for the map case (unverified): an enclosing closure whose
FIRST upvalue is a boxed cell, whose body runs an eager
`@a.map({ $free-scalar })` where the map block's own first upvalue is a
different name — the map block should read the enclosing cell instead.
Verify with a pin before/after each site fix.

## Longer-term shape

The invariant should be: **`self.upvalues` always belongs to the cc whose
ops are currently executing.** An RAII guard (swap-on-enter, restore-on-
drop) around every "exec this cc inline" helper would make the property
structural instead of per-site. Candidate for the ADR-0025 slice-2
implementation session, since slice 2 (more cells) further widens exposure.
