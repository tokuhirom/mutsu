# ADR-17681: A unit restarted from its CHECK-rewritten tree keeps the first compile's compile-time state

- **Status**: Accepted (2026-11-13)
- **Date**: 2026-11-13
- **Issue**: [#17681](https://github.com/tokuhirom/mutsu/issues/17681) (CHECK phasers run again
  over the rewritten tree) and [#17676](https://github.com/tokuhirom/mutsu/issues/17676) (BEGIN
  blocks run again); both found in Red's t/106.
- **Related**: #17501 / #17244 (a CHECK phaser that rewrites `$*CU` makes the unit continue from
  the rewritten tree), ADR-0134 (BEGIN prologue ordering)

## 1. Context

A CHECK phaser may rewrite `$*CU` (Red's `red-compile` walks the tree and replaces nodes). mutsu
runs a unit's compile-time statements (the BEGIN prologue and the CHECK phasers) as bytecode at
the head of the program; when the `RunCheckCode` opcode finds the tree changed since the end of the
BEGIN prologue, it lowers the rewritten tree and runs it in place of the compiled program (#17501).

That restart went through the ordinary `EVAL`-of-a-RakuAST path, which compiles and runs the tree
as a fresh compilation unit. So the unit's BEGIN prologue and CHECK phasers ran a **second** time,
now over the tree they had rewritten:

- `BEGIN say "begin"` printed twice (#17676).
- Red's t/106 runs `red-compile` in a CHECK phaser; on the second run it saw its own rewritten
  callbacks, found no `compiled` event for a regex callback, and died with "Regex callback was not
  compiled" (#17681).

Rakudo compiles the rewritten tree once; its BEGIN/CHECK-time code does not run again, because the
compile-time values already live in the tree's nodes and in the unit's lexicals.

An earlier attempt (#17675, reverted in aeb9afcc8) skipped only the CHECK phasers on the restart.
That broke `t/rakuast/rakuast-resolver-scope-stack.t`: its CHECK phaser pushes into a top-level
`my @seen` declared without an initializer, and the restart's fresh `my @seen` lost those values,
so the mainline assertions over `@seen` saw an empty array.

## 2. Options

1. **Lower the rewritten tree and run only its runtime remainder in the first compile's frame.**
   Conceptually closest to rakudo, but the restart's result replaces the mainline (END/LEAVE
   transfer, `mainline_superseded`), machinery built around running the tree as its own unit;
   running a second code object inside the live mainline frame is a much larger change.
2. **Run the rewritten tree as an EVAL unit, but in a "restart" mode that keeps the first compile's
   compile-time state:** skip its statement-form BEGIN/CHECK phaser bodies (they already ran), and
   have its top-level `my` declarations without an initializer adopt the first compile's bindings
   of the same name rather than reset them.
3. **Carry a recorded BEGIN prologue and its results into the restart.** The issue's first
   direction, but it requires the tree to remember which statements are the prologue and to replay
   their declared bindings; more machinery than the observable behaviour needs.

## 3. Decision

Option 2.

- **A statement-form BEGIN/CHECK phaser body is skipped on the restart.** `CheckPhaserStart`
  carries a `skip_on_restart` flag, set only by `compile_check_phaser` (the statement-form
  primitive, whose body leaves no value on the stack). When the flag is set and the unit compiling
  now is a rewritten restart (`unit_source_text::active_unit_is_rewritten`), the VM jumps to the
  matching `CheckPhaserEnd`, leaving the stack untouched. A value-form BEGIN
  (`my $x = BEGIN ...`, compiled through `BeginOnceExpr`) and a constant initializer's region
  (also a `CheckPhaserStart`, but with the flag false) are **not** skipped: their value is needed
  on the stack.
- **A top-level `my` declaration without an initializer is dropped on the restart**
  (`adopt_rewritten_restart_toplevel_decls`), so its uses resolve to the first compile's binding of
  the same name — whatever its BEGIN/CHECK phasers wrote into it. Only a plain, un-typed `my` is
  dropped; `our`/`state`/`constant`/`:=`-bound/dynamic/typed declarations keep their own handling,
  and a declaration *with* an initializer still runs its initializer (run-time code) and writes
  through to the adopted binding.
- **The first compile's top-level locals are published into `env` before the restart.** A top-level
  scalar lexical lives in the mainline frame's local slots, which the restart's separate code
  object cannot reach by name; `RunCheckCode` calls `sync_env_from_locals` before lowering the
  rewritten tree, so the restart's free-variable reads of the adopted names resolve to the first
  compile's values. A container (`@`/`%`) already lives in `env`.

The restart already did not re-run the add-check-code closures (`take_check_code` returns empty for
a `rewritten` unit) or rebuild `$*CU` (`refresh_cu_resolution` skips it), so it does not rewrite the
tree a second time.

## 4. Consequences

- A unit whose CHECK phaser rewrites `$*CU` runs its BEGIN and CHECK phasers once, with their
  lexical effects preserved: Red's t/106 now matches rakudo, and `BEGIN say "begin"` prints once.
- `t/rakuast/rakuast-resolver-scope-stack.t` keeps passing: its `my @seen` adopts the first
  compile's populated array.
- The `use` lines and type/role declarations of the rewritten tree still run on the restart, as
  before; they are idempotent on the already-loaded modules and types
  (`suppress_cross_eval_class_redeclaration_check`). Only the BEGIN/CHECK phaser *bodies* are
  skipped. This keeps the change bounded; a restart that must not re-run a module's import side
  effects is out of scope here.

## 5. Alternatives rejected

- **Skipping only CHECK (the #17675 attempt).** Loses a top-level `my`'s CHECK-written value, which
  is why it was reverted; the adopt rule is what makes skipping safe.
- **Skipping the whole BEGIN prologue, `use` lines included.** The runtime code needs the imports
  and types; dropping them would require carrying the prologue's results into the restart
  (option 3), more than the observable behaviour needs today.

## 6. Open questions

- A value-form BEGIN and a `my $x = BEGIN ...` constant still re-evaluate on the restart (their
  per-site memo is keyed to the first code object, not the restart's). This matches the old
  double-run for those forms and is not observably wrong for the suites in scope; a unit that both
  rewrites its tree and relies on a value-form BEGIN's single evaluation would expose it.
