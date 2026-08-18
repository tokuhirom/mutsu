# A bare list re-assignment's discarded rvalue leaked a stale `ContainerRef` into the shared env, corrupting the next closure that happened to reuse the same names

Root-caused and fixed
`todo/deep/sunk-list-reassign-leaks-containerref-into-shared-env.md`, found
continuing `todo/deep/vendor-real-test-module.md`'s `t/` residue sweep
(`t/warn-resumes-at-the-raise-site.t` test 8), and directly following the
2-call sibling bug
(`news/2026-08/control-warn-resume-caller-var-name-collision.md`).

## The bug

```raku
sub f(&code) {
    my ($x, $y, $z) = False, '', False;
    code();
    $z = True;
    CONTROL { when CX::Warn { $x = True; $y = .message; .resume } }
    ($x, $y, $z);
}

my ($x, $y, $z) = f({ warn "boom1" });
say "1: x=$x y=$y z=$z";   # x=True (correct)

($x, $y, $z) = f({ warn "boom2" });
say "2: x=$x y=$y z=$z";   # x=True (correct)

($x, $y, $z) = f({ warn "boom3" });
say "3: x=$x y=$y z=$z";   # x=False (wrong -- raku says True)
```

The two-call fix above resolved the 2-call minimal repro, but a *third* call
sharing the same caller variable names still lost the `CONTROL` handler's
write. Reproducing it still needed the real vendored `Test.rakumod`
(`MUTSU_REAL_TEST=1`) loaded.

## Root cause

Two independent mechanisms combine:

1. A bare (sunk) statement-level list re-assignment — `($x, $y, $z) =
   f(...);`, as opposed to a `my ($x, $y, $z) = ...;` **declaration** — is an
   *expression* in Raku, and its own value is the LHS list read back (needed
   for chained assignment, `($a,$b) = ($c,$d) = ...`). mutsu compiled that
   rvalue unconditionally: `WrapVarRef` tags each target, then `MakeArray`
   boxes it into a shared `ContainerRef` cell and writes that cell into the
   flat, cross-frame `env` store (list-element container aliasing). For a
   bare statement this value is immediately discarded by the following
   `SinkPop` — but the `env` write already happened and is not undone.
2. `capture_closure_env` (`vm_register_ops.rs`), under
   `reflective_name_access_possible()` (the broad-capture mode a program
   enters once it uses reflective/by-name features — real `Test.rakumod`'s
   own machinery among them), snapshots a closure literal's **entire** flat
   env at creation time rather than just its actual free variables. So
   `{ warn "boom3" }` — a block that never references `x`/`y`/`z` at all —
   still captured them, because they happened to be present in the ambient
   env at the moment the block literal was evaluated. When a captured entry
   is a `ContainerRef`, the closure-entry merge
   (`call_compiled_closure_with_topic`, `vm_closure_dispatch.rs`)
   deliberately *overwrites* the callee's own value with it (correct for a
   genuinely-referenced shared cell, e.g. box-on-capture) — but here the
   "genuinely referenced" premise doesn't hold, so it overwrote `f`'s own
   freshly-declared `x`/`y`/`z` with the stale cell from mainline's *previous*
   reassignment, moments after `f`'s own `my (...)` declaration had correctly
   initialized them.

Traced with `rust-gdb -batch` breakpoints (conditional on the declaring
statement's local slot index, then backtraces off `Env::insert`/`insert_sym`
hits) rather than rebuilding with debug prints: the sequence for call 3 was
declare-reset `x`/`y`/`z` to their correct values, then immediately
overwritten again from inside `call_compiled_closure_with_topic`'s captured-
`ContainerRef` merge, before the `warn` inside the block ever ran.

The first call's own statement is a `my` **declaration**, which has no such
trailing aliased-list construction (a fresh declaration's own statement value
is just the plain RHS array). The second call's statement is the first
**reassignment**, so it is the one that leaves the stale cell in `env`; the
third call is the first to run a closure literal while that cell is still
live — which is why the corruption only starts on the third call, not the
first two.

## Fix

The rvalue of a bare, sunk list-reassignment statement is unreachable — the
`SinkPop` right after `compile_stmt`'s `Stmt::Expr` arm discards it
unconditionally — so building the real aliased list (and thereby polluting
`env`) is pure waste as well as the cause of the corruption. `Stmt::Expr` now
recognizes the exact shape (a top-level `__mutsu_assign_callable_lvalue` call
over existing-variable targets, the parser's lowering of `($a,$b) = ...`) and
sets a one-shot compiler flag (`Compiler::sunk_list_assign_result`); the
list-assign compilation consumes it immediately, before compiling the RHS or
anything else, so a *chained* list assignment used as this one's RHS
(`($a,$b) = (($c,$d) = ...)`) is unaffected — only the outermost call's own
result construction is skipped, replaced with a cheap `Nil` placeholder
instead of the real `WrapVarRef`+`MakeArray` sequence.

Verified against `raku` for the non-sunk cases (consumed as a `say(...)`
argument, chained assignment) to confirm the aliasing behavior is preserved
when the result genuinely is used. Pin:
`t/sunk-list-reassign-does-not-leak-containerref.t` (the 3-call repro,
spawned as a subprocess with `MUTSU_REAL_TEST=1`, same shape as the 2-call
sibling pin). Full local `t/` suite (3244 files, 30044 tests) clean;
`cargo clippy -- -D warnings` clean.

`t/warn-resumes-at-the-raise-site.t` now passes 8/8 under `MUTSU_REAL_TEST=1`.
