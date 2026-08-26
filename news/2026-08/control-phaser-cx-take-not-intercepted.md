# `take` now raises a CONTROL-catchable `CX::Take`

From the doc-diff harness on `Language/phasers.rakudoc:401`:

```raku
say elems gather {
    CONTROL {
        when CX::Warn { say "WARNING!!! $_"; .resume }
        when CX::Take { say "Don't take my stuff"; .resume }
        when CX::Done { say "Done"; .resume }
    }
    warn 'people take stuff here';
    take 'keys';
    done;
}
```

`raku` prints the warning line, `Don't take my stuff`, `Done`, and `0`. mutsu
printed the warning and `Done` but skipped the `CX::Take` arm entirely and
answered `1` — the `take` slipped past the phaser and landed in the gather
buffer.

## Root cause

In Raku a `take` **always** throws a `CX::Take` control exception; `gather` is
merely its outermost handler. Any lexically nearer `CONTROL` block sees the
signal first. mutsu's `exec_take_op` short-circuited that: with an enclosing
gather on the stack it appended straight into the gather buffer
(`take_value(val)`) and only raised `RuntimeError::take_signal` when there was no
gather at all. There was therefore nothing for `when CX::Take` to catch. A
second, smaller gap: even in the gather-less case, the `OpCode::Take` arm never
recorded a `resume_ip`, so a `CONTROL` block that did catch the signal and called
`.resume` silently abandoned the rest of the block instead of continuing.

## Measured semantics (raku v2026.06)

| case | result |
| --- | --- |
| `CONTROL` with a `when CX::Take` that `.resume`s | handler runs per take; the value is **discarded**; execution continues after the take |
| `CONTROL` with a `when CX::Take` that does **not** resume | handler runs once; the CONTROL-installing block is abandoned |
| `CONTROL` that can only match something else (`when CX::Warn`) | declines; the take proceeds into the gather normally |
| `CX::Take`'s payload | none — `.raku` is `CX::Take.new`; the taken value is not exposed |

## Fix

`OpCode::TryCatch` gained a compile-time `control_handles_take` flag, computed by
a new `Compiler::control_block_handles_take`: true when the CONTROL block has an
arm that can match a `CX::Take` — an explicit `when CX::Take` or a catch-all
`default`. It is carried into the runtime's `ControlHandlerEntry` as
`handles_take`. `exec_take_op` consults the innermost active handler and raises
`take_signal(val)` only when that handler can actually match; everything else
keeps the direct fast path, so the common `gather` + unrelated
`CATCH`/`CONTROL` combination is untouched (verified: it still collects `[1, 2]`).

Because the flag guarantees the handler *will* match, the existing CONTROL
machinery in `exec_try_catch_op_inner` then covers all three outcomes for free:
`.resume` jumps to the newly-recorded `resume_ip` (the statement after the take,
with the value dropped), a handled-but-not-resumed signal ends the block, and a
signal from a deeper frame (`sub t { CONTROL {...}; take 5 }` called by a
`gather`) unwinds to the handler's own frame.

All four measured cases, plus the doc's original snippet, now match `raku`
exactly. Pinned by `t/control-constructs-in-expression-position.t`.
