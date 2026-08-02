# A resumable warning raised from a non-call opcode runs its CONTROL handler twice and loses its writes

`raise_resumable_warning` → `try_resume_safe_control_inline` is the mechanism
that lets a `warn` raised deep in a call chain reach a
`CONTROL { when CX::Warn { … .resume } }` handler and then *resume* at the raise
site. It works when the raise happens inside a **call** — a native method, a
builtin, a routine — which is how
`news/2026-08/a-warning-resumes-at-its-raise-site.md` fixed `Int.Numeric`.

It does **not** work when the raise happens inside a plain arithmetic opcode.

## Minimal repro

```raku
sub g {
    my ($d, $m) = False;
    { "x" x Int }();
    CONTROL { when CX::Warn { say "HANDLER"; $d = True; $m = .message; .resume } }
    say "d=$d m=$m";
}
g();
```

Swap `"x" x Int` for `Int.Numeric` and it prints `HANDLER` once and
`d=True m=Use of uninitialized value of type Int in numeric context`.

As written — with the warning routed through `raise_resumable_warning` from
`exec_string_repeat_op` (`src/vm/vm_arith_int_ops.rs`) — it prints:

```
HANDLER
HANDLER
d=False m=
```

**The handler body runs twice and neither of its writes reaches `g`'s frame.**
A `gdb` breakpoint on the raise site confirms the *raise* happens exactly once,
so the second `HANDLER` is the CONTROL block's own bytecode range being executed
a second time as `g`'s frame continues — the inline run does not leave the frame
in the state the normal "skip over the CONTROL declaration" path expects.

## Why it is not just a missing call

`try_resume_safe_control_inline` rebuilds the installing frame's locals from
`env`, runs the handler's bytecode range, then flushes changed slots back to
`env` and records them in `pending_rw_writeback_sources` for
`apply_pending_rw_writeback`. That drain runs at *call* boundaries. A raise from
an arithmetic opcode has no call boundary between the raise and the installing
frame, and the operand stack is mid-instruction (the op has popped its operands
but not pushed its result) while the handler's bytecode runs on that same stack.

## Status

**Not applied.** The change was written and reverted: with it, the warning is
*silently swallowed* (nothing on stderr, handler writes lost), which is strictly
worse than today's behaviour, where `exec_string_repeat_op` calls
`write_warn_to_stderr` directly — the message is printed but no `CONTROL`
handler ever sees it.

## What it blocks

`roast/S03-operators/repeat.t` test 56, under the real `Test::Util`:

```raku
warns-like { 'x' x Int }, *.contains('uninitialized' & 'numeric'),
    'using an unitialized value in repeat count throws';
```

It is one of the last two files in
`todo/tickets/retire-native-test-util-overrides.md`. There are two other
`write_warn_to_stderr` sites with the same shape (the `xx` twin at
`vm_arith_int_ops.rs`, and the pair in `runtime/builtins_operators_repeat.rs`),
so a fix covers all four.

## Where to start

Find why the CONTROL range executes a second time — instrument
`try_resume_safe_control_inline`'s `run_range(&code, control_begin, end, &fns)`
and compare the `ip` the enclosing frame resumes at against the non-inline path.
The fix is probably to make the *op-level* raise site behave like a call
boundary (drain `pending_rw_writeback_sources` and re-establish the frame's
`ip`/stack invariants), which is the same "op-level warn sites need a call-like
boundary" problem the `raise_resumable_warning` doc comment describes.
