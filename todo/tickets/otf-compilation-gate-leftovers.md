# OTF (module-sub on-the-fly compilation) gate — the two remaining exclusions

Extracted from PLAN.md §3 (2026-08-02). The module-sub OTF gate relaxation campaign
(#4427 → #4429 → #4431 → #4437) is complete; the gate itself is
`def_is_otf_compilable_module_single` (`src/vm/vm_call_func_ops.rs`). The frontier of "just remove
the gate and experiment" is exhausted — each remaining exclusion needs mechanism work first.

## 1. `start` blocks — needs per-call capture cells

When a recursive sub's `start` closure captures a parameter, the re-bind of the recursive call
clobbers the captured value, so `start` is excluded wholesale. Regression pin:
`t/start-block-return-value.t` test 3. The proof of infeasibility and the history are in
`news/2026-07.md`. The real fix is per-call capture cells — the same cell-based capture work as
[needs-env-sync-blanket-removal.md](../deep/needs-env-sync-blanket-removal.md).

## 2. Sigilless scalar (`\x`) parameters — needs a caller-slot mechanism

Caller writeback of raw aliases across `EVAL` needs a mechanism equivalent to #4091 (the `is rw`
compile-time caller slot). FAIL pin: `t/sigilless-params.t`, "sigilless aliases are writable through
EVAL calls".

## Intentionally excluded (decided not to do)

- **Default-param builtin-shadow single candidate** — name-cache pollution risk (user policy).
- **`is encoded(...)`** — NativeCall; zero practical harm.
- **`state` sharing across signature alternates** — kept as an interpreter boundary.
