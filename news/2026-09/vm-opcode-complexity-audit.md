# Every VM opcode now documents its complexity; per-op frame-size costs are tracked

This is the last of four complexity audits. The first three covered `nqp::`
ops, `Str` methods and `Array`/`List` operations. This one annotates the
bytecode VM:

- all 370 `OpCode::` arms of `exec_one_dispatch` (`src/vm/vm_exec_dispatch.rs`)
  now have a `// Cost:` line;
- so does the handler function where each deficit's root cause lives;
- so does the run loop (`src/vm/vm_run_loop.rs`), which adds O(1) per
  dispatched op.

Every `OpCode` variant has an arm, so none was missed. The rule that every
built-in method, routine, `nqp::` op and opcode carries such a line is now
in `CLAUDE.md` (Conventions) and `docs/complexity-annotations.md`.
`grep -rn 'Rakudo: O(\|MoarVM: O(' src/` lists 279 known deficit lines
across the four audits.

## scripts/vm-complexity-check.sh

The new `scripts/vm-complexity-check.sh` differs from the other three
scripts. It keeps a fixed timed body and doubles something the opcode
should not care about:

- the number of locals in the frame;
- env entries;
- package size;
- MRO depth;
- call depth;
- operand size.

It generates the big frames and class chains itself.

Several operand-free controls stay flat across those same frames, which
shows the growth belongs to specific ops: `$x == 42`, a sub call, and
`@a[5]++` against array size.

## What the audit found

The dominant finding is a family of ops whose cost depends on the size of
the surrounding program, not on their operands.

- **#9169, whole-frame locals ↔ env synchronisation.** `say`/`print`,
  `~~`, `does`/`but`, `andthen` with a user `.defined`, a user `sink`,
  stores to outer variables, `&f` and `eager` each synchronise every frame
  local with env. They cost O(L) per execution. Examples:
  - 20000 × `$x ~~ Int` takes 0.68 s next to 250 locals and 1.19 s next
    to 500.
  - 20000 × `$s += (my $z = $_)` takes 0.38 s at 2000 env entries and
    0.72 s at 4000.
- **#9170, scope entry/exit and closure creation.** Bare blocks with a
  `my`, `"a{ $x }b"` interpolation blocks, `-> { }` closures, `gather`,
  `my sub` and block-level `use` walk the whole env or snapshot the routine
  registry on every execution. Example: 5000 × `"a{ $t }b"` takes 0.44 s
  at 1000 declarations and 0.85 s at 2000, while `"a$t b"` takes 0.005 s.
- **#9171, lookups by name.** Several ops find a local by a linear name
  scan, or build a map of the whole env to read one pseudo-stash entry.
  - A sub with K `my` declarations costs O(K²) per call.
  - A `P::<$x>` read costs about 300 µs.
- **#9172, four single sites.**
  - `===` walks both operands, even though its final test is a pointer
    compare.
  - A resumable `CATCH` clones the enclosing code unit each time its
    region is entered.
  - `die` builds the backtrace eagerly, so a throw costs O(call depth).
  - Method calls walk the MRO twice per call.
- **#9173, minor bounds.** `temp` deep-copies, `ResetStateLocals`,
  `goto`'s label scan, `run_reuse`, and class-MRO computation.
- **Existing issues.** Boolifying a map/grep Seq forces all of it (#9158).
  A computed-target store into an `is Array` instance copies the storage
  (#9157).

The audit also found four correctness bugs, filed as `todo:ticket`:

- **#9165:** `eager gather` inside a loop loses writes to outer locals.
- **#9166:** `"@*ARGS[0]"` is not interpolated.
- **#9167:** a lexical named `$__t0` reads back the wrong value.
- **#9168:** `.self[1] = 1` on an `is Array` instance dies.
