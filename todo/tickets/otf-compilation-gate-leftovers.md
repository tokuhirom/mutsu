# OTF (module-sub on-the-fly compilation) gate — the remaining exclusion

Extracted from PLAN.md §3 (2026-08-02). The module-sub OTF gate relaxation campaign
(#4427 → #4429 → #4431 → #4437) is complete; the gate itself is
`def_is_otf_compilable_module_single` (`src/vm/vm_call_func_ops.rs`). The frontier of "just remove
the gate and experiment" is exhausted — each remaining exclusion needs mechanism work first.

**Sigilless scalar (`\x`) parameters — done.** Verified 2026-08-14: `def_is_otf_compilable_module_single`'s
own comment now says "sigilless scalars (`\x`) are compiled-safe since ADR-0019 C6e-2a (the compiled
return path flushes the alias chain before the caller-env merge, covering the EVAL-boundary
writeback)". `t/sigilless-params.t` ("sigilless aliases are writable through EVAL calls") passes. No
longer an open exclusion; removed from this ticket.

## `start` blocks — needs per-call capture cells

When a recursive sub's `start` closure captures a parameter, the re-bind of the recursive call
clobbers the captured value, so `start` is excluded wholesale. Regression pin:
`t/start-block-return-value.t` test 3. The proof of infeasibility and the history are in
`news/2026-07.md`. The real fix is per-call capture cells.

**Still excluded, confirmed 2026-08-14**: `expr_needs_interpreter` (`src/vm/vm_call_func_ops.rs`, ~line
1996) still has `name.resolve() == "start"` unconditionally forcing the interpreter fallback, with the
comment "start blocks need the interpreter for proper thread spawning". `t/start-block-return-value.t`
currently passes, but only via that fallback path — it does not demonstrate the OTF-compiled path is
safe, since the gate routes it away from OTF entirely. Not re-attempted this round: this is the same
capture-cell mechanism gap as `todo/tickets/inline-start-blocks-clobber-a-later-declared-variable.md`
(a *different*, currently-reproducing `start`-block cross-thread env bug narrowed the same day — see
that ticket for a live repro and root-cause leads in the current, post-ADR-0018 architecture), so the
two should likely be picked up together rather than independently.

## Intentionally excluded (decided not to do)

- **Default-param builtin-shadow single candidate** — name-cache pollution risk (user policy).
- **`is encoded(...)`** — NativeCall; zero practical harm.
- **`state` sharing across signature alternates** — kept as an interpreter boundary.
