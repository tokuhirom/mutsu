# A resume-safe CONTROL handler's write is dropped when the caller reuses the same variable name across two calls

Root-caused and fixed
`todo/deep/control-warn-resume-list-assign-first-target-stale-on-repeat-call.md`,
found continuing `todo/deep/vendor-real-test-module.md`'s `t/` residue sweep
(`t/warn-resumes-at-the-raise-site.t`).

## The bug

```raku
sub f(&code) {
    my ($x, $y, $z) = False, '', False;
    code();
    $z = True;
    CONTROL { when CX::Warn { $x = True; $y = .message; .resume } }
    ($x, $y, $z);
}

my ($x, $y, $z) = f({ warn "boom" });
say "first: x=$x y=$y z=$z";        # x=True (correct)

($x, $y, $z) = f({ warn "boom2" });
say "second: x=$x y=$y z=$z";       # x=False (wrong -- raku says True)
```

Only `$x` — and only from the *second* call on — reverted to its
function-entry default, even though the `CONTROL` block visibly ran (`$y`
picked up the new message, `.resume` did resume execution, `$z`'s write
survived). Reproducing it needed the real vendored `Test.rakumod`
(`MUTSU_REAL_TEST=1`) loaded; a large synthetic module with many declared
subs did not trigger it, which is what made this look Test-module-specific
at first.

## Root cause

`warn`'s resume-safe `CONTROL` handler runs *inline*, at the raise site, deep
inside the closure's call stack (`try_resume_safe_control_inline`,
`builtins_control_flow.rs`). Since `self.locals` at that point belongs to the
deep frame, not `f`'s own, the handler writes the mutated lexical (`x`)
straight into the flat, cross-frame-visible `env` store — the write has no
call opcode to mark it, so `Interpreter::inline_control_env_writes` (a
counter) is bumped to tell the closure's own return path "something changed,
run the caller-writeback scan even though you look like a leaf".

That return path (`call_compiled_closure_with_topic`,
`vm_closure_dispatch.rs`) restores `self.env` from `frame.saved_env` (a
snapshot from *before* the call) and then selectively re-applies names the
call actually changed. One of its guards exists specifically to avoid leaking
an unrelated same-named caller lexical into the closure's own captured
binding: if a name is in the closure's blanket capture snapshot
(`data.env`, taken at closure-creation time) AND the live value still equals
that capture-time value, the write is treated as "the closure's own untouched
capture, not a caller mutation" and skipped.

That heuristic is a false positive here: on the second call, the *caller's*
`$x` already holds `True` (the first call's result) at the moment the second
closure (`{ warn "boom2" }`) is created, so the closure's blanket env
snapshot happens to capture `x => True` too — purely by coincidence, since
the closure's body never references `x` at all. The `CONTROL` handler then
writes `x = True` again (the same value), so the identity check sees "no
change" and drops the write, leaving `restored_env`'s `x` at whatever `f`'s
own declare step had set it to (`False`) moments earlier.

## The fix

`inline_control_env_writes` changed from a counter to a log of the `Symbol`s
actually written (`Vec<Symbol>`, appended in
`try_resume_safe_control_inline`). The closure-return writeback scan now
exempts any name in that log — written by an ancestor frame's `CONTROL`
handler during *this* call — from the "unchanged capture, skip" heuristic,
since that write is known to be a genuine mutation regardless of whether it
happens to coincide with the capture-time value.

Pin: `t/control-warn-resume-caller-var-name-collision.t` (spawns a subprocess
with `MUTSU_REAL_TEST=1`, since the synthetic-module trigger above did not
reproduce it — the real vendored module is the smallest known repro). Full
local `t/` suite and `cargo clippy -- -D warnings` clean.

## What is still open

`t/warn-resumes-at-the-raise-site.t` test 8 (a role-composed `Numeric` type
object's default `.Numeric`, dispatched through `methods_call_dispatch.rs`'s
interpreted path rather than a native op) still fails under
`MUTSU_REAL_TEST=1` after this fix — a *different* mechanism drops the
`CONTROL` handler's write entirely (the handler's own `seeded`/pre-control
snapshot reads a value that is not this call's declared default at all, so
the bug is upstream of the closure-return path this fix touches). Filed
separately:
`todo/deep/control-warn-third-chained-call-through-method-dispatch-corrupts-env.md`.
