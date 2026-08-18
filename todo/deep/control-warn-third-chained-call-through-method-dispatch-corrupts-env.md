# A 3rd chained resumable-warn call, dispatched through interpreted method dispatch, sees a corrupted pre-CONTROL local snapshot

Found investigating `todo/deep/vendor-real-test-module.md`'s `t/` residue,
continuing from
`todo/deep/control-warn-resume-list-assign-first-target-stale-on-repeat-call.md`
(fixed by `news/2026-08/control-warn-resume-caller-var-name-collision.md`).
That fix resolves the 2-call minimal repro, but `t/warn-resumes-at-the-raise-site.t`
test 8 still fails under `MUTSU_REAL_TEST=1` — a distinct bug, upstream of the
one just fixed.

## Repro

```raku
use Test;

sub caught-by-control(&code) {
    my ($did, $msg, $reached) = False, '', False;
    code();
    $reached = True;
    CONTROL { when CX::Warn { $did = True; $msg = .message; .resume } }
    ($did, $msg, $reached);
}

my ($did, $msg, $reached) = caught-by-control { warn "boom" };
say "1: did=$did msg=$msg reached=$reached";

($did, $msg, $reached) = caught-by-control { Int.Numeric };
say "2: did=$did msg=$msg reached=$reached";

my class CustomNumeric does Numeric { method new { 42 } }
($did, $msg, $reached) = caught-by-control { CustomNumeric.Numeric };
say "3: did=$did msg=$msg reached=$reached";
```

```
$ MUTSU_REAL_TEST=1 target/debug/mutsu repro.raku
1: did=True msg=boom reached=True
2: did=True msg=Use of uninitialized value of type Int in numeric context reached=True
3: did=False msg= reached=True
```

`raku` prints `did=True` and the coercion message on all three calls. Only
the third call is wrong, and only after this fix landed — before it, calls 2
*and* 3 were both wrong (same symptom, presumably the same underlying
identity-skip false positive for call 2, but call 3 has its own, separate
cause that survives the fix).

## What was ruled out / narrowed down

- **`try_resume_safe_control_inline` genuinely runs for call 3** — confirmed
  via `rust-gdb` breakpoints on `builtins_control_flow.rs:419` (entry) and
  the `control_handlers`/`control_handler_depth` push/pop in
  `vm_try_catch_ops.rs:100`/`131`: depth goes `0 -> 1 -> 0` around each of the
  three calls, and `try_resume_safe_control_inline` is entered all three
  times with `resume_safe: true` and a live `handler`.
- **Not the unwinding fallback path.** Breakpoints on
  `builtins_control_flow.rs:378` (`Err(RuntimeError::warn_signal(...))`,
  the "no inline-eligible handler" fallback) and
  `vm_try_catch_ops.rs:319` (the OTHER, unwinding-based
  push/resume site inside `exec_try_catch_op_inner`'s `Err` arms) never fire
  for any of the three calls — every call resolves entirely inline.
- **The `CONTROL` block's `when CX::Warn` clause never matches for call 3.**
  Breakpoints on `builtins_control_flow.rs:466`
  (`let saved_locals = std::mem::replace(&mut self.locals, handler_locals);`,
  right after `handler_locals` is built from `env` — i.e. the *seed*) and
  `:483` (right after `run_range` executes the CONTROL block, i.e. the
  *post-control* state) show, for call 3's `handler_locals` vector (slots
  `[&code, @__destructure_tmp__, did, msg, reached]`):

  ```
  seed:         [.., did=<not False's bits>, msg=<not ''s bits>, reached=<not False's bits>]
  post-control: [.., did=<UNCHANGED>,        msg=<UNCHANGED>,    reached=<UNCHANGED>]
  ```

  Every slot is bit-identical before and after `run_range` runs the CONTROL
  block — not "changed back to the same value" (that would be the bug this
  ticket's parent just fixed) but literally never written at all. And
  critically, the **seed values themselves are not this call's own declared
  defaults** (`False`, `''`, `False`) — they are something else entirely (the
  bit patterns look like heap-pointer-shaped `Value`s, e.g. `Str`/boxed
  values, not the small inline `Bool` constants seen in calls 1 and 2's own
  seeds).
- Calls 1 and 2's `seed`/`post-control` pairs look exactly as expected
  (`False/''/False` seed, `True/<message>/False` post-control) — this is
  specific to call 3.

## What is not yet known

- **Why `handler_locals` (built by `try_resume_safe_control_inline` reading
  `env` by name — see the fixed ticket for that mechanism) does not reflect
  call 3's own fresh `my ($did, $msg, $reached) = False, '', False;`
  declare**, which runs unconditionally at the top of `caught-by-control`
  before `code()` is ever called. Something between that declare and the
  warn raise inside `CustomNumeric.Numeric` overwrites `env["did"]` (and
  `msg`/`reached`) with an unrelated value. The likely culprit, not yet
  confirmed: `CustomNumeric.Numeric`'s dispatch
  (`methods_call_dispatch.rs`, the `.Numeric`/`.Real` default-method arm for
  a role-composed type object) calls `self.call_method_with_values(target,
  "new", vec![])` to compute the resume value *before* raising the warning —
  a real nested method call (`method new { 42 }`) that pushes and pops its
  own call frame. If that call's OWN env-restore path has a bug analogous to
  the one just fixed in `call_compiled_closure_with_topic` (or a different
  one that overwrites rather than skips), it could clobber `env["did"]`
  between the declare and the raise.
- Whether the fix belongs in `methods_call_dispatch.rs`/whatever backs
  `call_method_with_values` for a user-defined `method new`, or in
  `raise_resumable_warning`/`try_resume_safe_control_inline` itself (e.g. it
  might need to read the caller's frame more precisely than a flat `env`
  lookup by name once more than one call has intervened since the
  installing frame's own declare).
- Whether this is really about method dispatch specifically, or about
  *three* resumable-warn dispatches chaining (i.e. does a 3rd `warn`-based
  call, with no method dispatch at all, also fail once enough state has
  accumulated?) — not yet tested. A quick next step: replace call 3's
  `{ CustomNumeric.Numeric }` with a third `{ warn "boom3" }` and see if it
  still reproduces (if not, method dispatch is confirmed as the necessary
  ingredient, not just "the third call").

## What it blocks

`t/warn-resumes-at-the-raise-site.t` test 8 under `MUTSU_REAL_TEST=1`
(`todo/deep/vendor-real-test-module.md`'s `t/` residue). The file's other 7
assertions pass; this is the last blocker for that file.
