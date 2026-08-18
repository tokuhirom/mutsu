# A statement-level list re-assignment's discarded alias-array leaves a stale `ContainerRef` in the shared env slot, corrupting the next call's resumable-warn read

Found investigating `todo/deep/vendor-real-test-module.md`'s `t/` residue,
continuing from the (fixed) `control-warn-resume-caller-var-name-collision`
bug (`news/2026-08/control-warn-resume-caller-var-name-collision.md`). That
fix resolves the 2-call minimal repro, but `t/warn-resumes-at-the-raise-site.t`
test 8 still fails under `MUTSU_REAL_TEST=1`. **Correction (2026-08-19):**
this ticket originally suspected interpreted method dispatch
(`CustomNumeric.Numeric`) as the trigger — that was wrong, see below. The
actual root cause is unrelated to method dispatch and reproduces with three
plain `warn` calls.

## Repro (no method dispatch involved)

```raku
use Test;

sub f(&code) {
    my ($x, $y, $z) = False, '', False;
    code();
    $z = True;
    CONTROL { when CX::Warn { $x = True; $y = .message; .resume } }
    ($x, $y, $z);
}

my ($x, $y, $z) = f({ warn "boom1" });
say "1: x=$x y=$y z=$z";

($x, $y, $z) = f({ warn "boom2" });
say "2: x=$x y=$y z=$z";

($x, $y, $z) = f({ warn "boom3" });
say "3: x=$x y=$y z=$z";
```

```
$ MUTSU_REAL_TEST=1 target/debug/mutsu repro.raku
1: x=True y=boom1 z=True
2: x=True y=boom2 z=True
3: x=False y= z=True
```

`raku` prints `x=True` on every call. Only the third call (and every call
after it — a 4th/5th call reproduces the same wrong output, it does not
"recover") is wrong. Confirmed this is unrelated to the originally-suspected
`CustomNumeric.Numeric` method dispatch: replacing call 3 with a third plain
`{ warn "boom3" }`, or using the `did`/`msg`/`reached` names from
`t/warn-resumes-at-the-raise-site.t` instead of `x`/`y`/`z`, reproduces
identically. It is specifically the **third call onward**, not "any call
after a method-dispatch coercion".

## Root cause (traced with `rust-gdb`, `MUTSU_JIT=off`)

1. `f`'s own return statement `($x, $y, $z);` compiles to
   `GetLocal+WrapVarRef` for each of `x`/`y`/`z`, then `MakeArray(3)`.
   `exec_make_array_op` (`vm_data_ops.rs:14-30`) sees each `WrapVarRef`-tagged
   element and calls `capture_var_cell_inner`, which boxes the named
   variable into a shared `ContainerRef` cell — correct Raku semantics: a
   returned list of bare variables is an *aliased* list, so writing through
   an element must be visible in the original variable. `capture_var_cell_inner`
   (`vm_data_ops.rs:319`, via `set_env_with_main_alias_sym`) writes this
   `ContainerRef` cell into `env` under the variable's bare name — `f`'s own
   `x`, in `f`'s dynamic scope, but env is a single flat namespace, so this is
   the *same* env key mainline's own `$x` uses.
2. Mainline's `($x, $y, $z) = f(...);` — a **re-assignment**, not a `my`
   declaration — is compiled to: the actual element-wise assignment
   (`AssignExprLocal` ×3, which correctly deref's the returned `ContainerRef`s
   via `DecontListElems` before storing, so mainline's own locals/env end up
   with plain values), **followed by** a second, structurally identical
   `GetLocal+WrapVarRef ×3, MakeArray(3), SinkPop` sequence — because the
   *reassignment expression itself* evaluates to the aliased list of its LHS
   targets (needed e.g. for chained assignment), even when that value is
   immediately discarded in statement (sink) context. This second
   `MakeArray` call *also* runs through `exec_make_array_op`'s `WrapVarRef`
   branch, and boxes mainline's own `x`/`y`/`z` into fresh `ContainerRef`
   cells — again written into the same flat `env["x"]`/`env["y"]`/`env["z"]`
   — immediately re-corrupting what the assignment had just cleaned up, even
   though the array is about to be thrown away (`SinkPop`).

   A `my (...) = ...;` **declaration** statement does not have this trailing
   sequence (its own statement-value is just the plain RHS array, no aliasing
   implied for a fresh declaration) — which is why the corruption only
   appears starting from the *first reassignment* (the transition from call 2
   to call 3 in the repro above): call 1's statement is a `my` declare (no
   trailing box), call 2's statement is a reassignment (leaves a stale
   `ContainerRef` in `env["x"/"y"/"z"]` after it completes), and call 3 is the
   first call to observe that stale cell.
3. `f`'s own next declare (`my ($x, $y, $z) = False, '', False;`, at the top
   of call 3) has an existing guard for exactly this shape
   (`vm_var_assign_set_local.rs:538-551`, "Replace stale `ContainerRef` in env
   with `Nil` so a new `my $var` doesn't inherit a binding from an earlier
   scope") — confirmed firing via a `rust-gdb` breakpoint on that line,
   **only** on call 3's declare (not calls 1 or 2). It resets the env entry to
   `Nil`, not to the freshly declared `False` — and something further
   downstream (not yet traced past this point) leaves the env entry in a
   state that still is not `False`/`''`/`False` by the time
   `try_resume_safe_control_inline` reads it to seed `handler_locals` for the
   `warn` raised inside `code()` a few ops later. That seed is what actually
   feeds the `CONTROL` block, so the `when CX::Warn` body's `$x = True; ...`
   writes land on top of a wrong base, and/or the block's success path
   compares against a wrong `seeded` snapshot (see the — since-superseded —
   "What was ruled out" section of this ticket's first draft: `seed ==
   post-control`, i.e. the write never lands at all).

## What is not yet known

- The exact mechanism between the `Nil`-reset (`vm_var_assign_set_local.rs:550`)
  and `try_resume_safe_control_inline`'s seed read that leaves the seed at a
  non-declared value rather than `False`/`''`/`False`. Next step: breakpoint
  on every write to `env["x"]` between those two points specifically for call
  3 (the `Symbol::intern("x").0`-keyed conditional breakpoint on
  `src/env.rs:645`/`637` worked reliably for this in the investigation so
  far — plain string equality conditions in `rust-gdb` are unreliable, silently
  mis-evaluate, and must not be trusted; compare on `Symbol.0`, a `u32`,
  instead).
- Whether the right fix is (a) not writing the boxed `ContainerRef` into the
  shared `env` at all when `exec_make_array_op`'s result is about to be
  discarded (`SinkPop` immediately follows) — the compiler would need to skip
  emitting the trailing alias-array construction entirely for a sunk
  reassignment expression, which seems the more principled fix and avoids the
  wasted allocation too — or (b) hardening the various env-seeding sites
  (`try_resume_safe_control_inline` and potentially others) to treat a
  `ContainerRef`/`Nil` residue the same way the existing declare-time guard
  does. (a) likely has a much broader blast radius (any sunk multi-assign
  expression, not just ones feeding a resumable warn) and is probably the
  general fix; (b) is narrower but leaves the underlying env pollution for
  other consumers to trip over.
- Whether this same mechanism corrupts *any* other env-by-name reader across
  a reassignment statement (not just `try_resume_safe_control_inline`) — a
  reasonable next probe: does a plain (non-`warn`) cross-frame by-name env
  read (e.g. `CALLER::`, a dynamic variable, `$*x`) also see stale
  `ContainerRef`/`Nil` after a sunk reassignment shares its name with a
  callee's own local?

## What it blocks

`t/warn-resumes-at-the-raise-site.t` test 8 under `MUTSU_REAL_TEST=1`
(`todo/deep/vendor-real-test-module.md`'s `t/` residue). The file's other 7
assertions pass; this is the last blocker for that file. Test 8 specifically
is the *third* `caught-by-control` invocation in that file (`warn` "boom",
`Int.Numeric`, `CustomNumeric.Numeric` — three calls, matching this ticket's
"third call onward" shape), which is why it looked method-dispatch-related
at first: it is simply the first `t/` test file with three sequential
resumable-warn calls sharing caller variable names.
