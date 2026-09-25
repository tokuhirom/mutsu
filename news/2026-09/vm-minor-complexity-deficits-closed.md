# The VM audit's five minor complexity deficits are closed

The VM opcode complexity audit (`news/2026-09/vm-opcode-complexity-audit.md`)
grouped five bounds that were worse than Rakudo's into
[#9173](https://github.com/tokuhirom/mutsu/issues/9173). None of them was known
to change a realistic program's order, but each one scaled with something
unrelated to its operands. All five now meet the issue's order goal, and no
`-- see #9173` suffix is left in `src/`.

## `temp` / `let` no longer deep-copies

`exec_let_save_op` snapshotted an `@`/`%` variable with a recursive deep copy,
O(total nodes). It now copies one level (`shallow_copy_value`), O(e), which is
also what raku's `.clone` does: `temp @a; @a[0][0] = 99` keeps the nested write
after the restore in raku, and now in mutsu too. The one-level copy also
decontainerizes a bound element (`@c[0] := $x`), so the restore brings back the
value instead of the binding. Before this change `@c` was not restored at all.

A plain `$` variable is not copied at all now. Its snapshot is the value it
held, so `temp $s; $s[0] = 7` keeps the write, as it does in raku. Before, the
deep copy undid it.

The multi-level element form (`temp $s[1]<k>[1] = v`) is still lowered to a save
of the whole base variable. That save alone keeps the deep copy (the new
`Stmt::Let::nested_lvalue` / `LetSave { deep }` flag). Saving just the element
container, as Rakudo does, is
[#9434](https://github.com/tokuhirom/mutsu/issues/9434). A remaining difference
for bound elements inside the `temp` scope is
[#9435](https://github.com/tokuhirom/mutsu/issues/9435).

Measured with the new `temp @a vs nested element size` case of
`scripts/vm-complexity-check.sh`: t(2N)/t(N) = 1.05 (debug build).

## `goto`, label validation and `ResetStateLocals` stop rescanning the chunk

A new lazily built per-chunk index, `src/op_scan_index.rs` (a `OnceLock` on
`CompiledCode`), holds three things: the `Label` table, the first duplicate
label, and the `StateVarInit` positions of each `state` local.

- `goto LABEL` and a labelled control exception look their target up in O(1)
  instead of scanning every op.
- `validate_labels` used to run on every `run_inner` / `run_reuse` entry, and so
  on every `map`/`grep` callback iteration. It now reads the cached duplicate
  instead of rebuilding a `HashSet` over the ops.
- `reset_state_locals_in_range` / `sync_state_locals_in_range` cost O(t) instead
  of O(t·b): each state local's init-in-range test is a binary search over its
  init positions.

The index records the `ops.len()` it was built from. If the op vector's length
changes afterwards (a runtime-patched chunk), queries fall back to the old scan.

## A `map` callback no longer scales with its enclosing frame

The `run_reuse` item turned out to be the smaller half. Profiling a `map`
inside a sub with L lexicals showed that the time went into
`Env::note_frame_write`. That function asks whether a by-name write (binding the
topic, for one) is already in the frame-write log, and the log was a `Vec`
seeded with every key of the collapsed frame tier. So every topic bind scanned
all L names. The log is now `FrameWriteLog` (`src/frame_write_log.rs`): the
list, plus a hash index once it holds more than 16 names.

`map callback vs frame locals` (L = 1000 → 2000): ratio 1.03. With L = 10 /
2000 / 4000, 20 maps of 5000 items took 0.71 / 0.74 / 0.67 s (debug). Before,
the same runs took 0.67 / 2.57 / 5.30 s.

## Declaring a chain of classes is no longer cubic

`finalize_class_registration` computed the new class's C3 linearization only to
validate it, then threw it away. Nothing cached a plain `class B is A {}`'s
MRO, so each declaration re-walked the whole ancestor chain. On top of that,
the C3 merge itself was O(n²): it did a `remove(0)` per head and rescanned
every tail. Now the validated linearization is stored in `ClassDef::mro`
(except for a `__hoisted` shell). The merge (`Registry::c3_merge`) uses a
cursor per sequence and a count of how many tails hold each name, O(n·k).

Chains of 100 / 200 / 400 / 800 classes took 0.12 / 0.20 / 0.50 / 1.47 s to
declare and use. Before: 0.20 / 1.32 / 15.0 s, with 800 not attempted (debug).
The `declaring a chain of NN classes` case shows 2.49 against the healthy 4
(O(d) per class).

Pins: `t/vm/scope/temp-snapshot-depth.t`, `t/oo/class/c3-mro-long-chain.t`,
and unit tests in `src/op_scan_index.rs` and `src/frame_write_log.rs`.
