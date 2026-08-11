# List-element cell capture picked a same-named shadow slot — the CSV::Table comment-strip heisenbug

`CSV::Table`'s `TWEAK` read loop (`($line, $comment) = strip-comment $line, ...`
inside `for $fh.lines -> $line is copy`, with a `my $line = @lines.head` shadow
in a conditional block further down the body) kept 4 spurious comment rows that
raku drops, and inserting a `note` call made the bug vanish — the classic
env/slot state-sync heisenbug shape recorded in
`todo/deep/csv-table-comment-strip-loop-var-state-sync.md`.

## Root cause

`capture_var_cell_inner` (vm_data_ops.rs) — the runtime that boxes a
`WrapVarRef`-tagged scalar into a shared `ContainerRef` cell when a List /
Capture / Pair aliases it — resolved the variable's local slot **by name**:

```rust
let Some(idx) = code.locals.iter().rposition(|n| n == name) else { ... };
```

Under §1.4 shadow slots, several `code.locals` entries can share one name. A
same-named `my` in a sibling or inner block (even one that had not executed
yet, or whose scope had already closed) made `rposition` pick the *shadow's*
slot, box that slot's stale value into a fresh cell, and insert the cell into
`env[name]` — poisoning the by-name lane. The next `GetLocal` of the *outer*
variable then hit the lazy env→slot cell-adopt sync and replaced the correct
slot value with the stale cell. The multi-assign form `($a, $b) = ...` was the
usual trigger because its assignment-result List (`GetLocal; WrapVarRef; ...;
MakeArray`) runs the capture on every execution.

A second face of the same defect: for an **env-based** variable (a `for` loop
param with no local slot), the by-name search matched a same-named shadow's
slot anyway, with the same env poisoning — this is what broke the actual
CSV::Table loop even without the assignment-result list being read back.

Any statement that forced a fresh env sync in between (the `note` call) hid
the stale adopt, which is why every minimal reduction failed until the slot
mechanism itself was traced with `rust-gdb` watchpoints.

## Fix (PR #6255)

Carry the compile-time slot resolution through to the capture:

- `OpCode::WrapVarRef` now carries `{ name_idx, slot }`, where `slot` is the
  emitting site's `local_map` resolution (`u32::MAX` = "provably not a local
  of this frame").
- The `VarRef` value payload records that slot (`VarRefBox::slot`, exposed via
  `Value::varref_slotted` / `Value::varref_slot`; the existing `index` field
  keeps its slurpy-element meaning).
- `capture_var_cell_inner` prefers the recorded slot; for the `u32::MAX`
  sentinel it does **not** guess a slot by name at all (the variable lives in
  env); the `rposition` fallback survives only for VarRefs built without
  compiler slot info (legacy constructors). The hint is dropped when `:=`
  alias-root resolution redirects the name to a different variable.

With this, `CSV::Table`'s own suite goes 9/10 files (`t/2-commented.t` and
`t/7-half-matrix.t` newly green); the reduced repros match raku exactly.
Pinned by `t/list-alias-shadowed-name.t`.

## Residue

`t/5-save.t` still fails, but on an unrelated bug: element values bound to a
`$`-sigil loop param lose their itemization, so `sprintf "%-*.*s", $w, $w, $v`
flattens the row array `$v` into extra arguments. Filed as
`todo/deep/element-itemization-lost-in-scalar-binding.md`.
