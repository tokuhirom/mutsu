# ADR-0072: A resumable exception runs its `CATCH` handler at the throw point, not after unwinding

- Status: Accepted (Slice 1 implemented; Slices 2-3 open — see "Implementation status")
- Date: 2026-09-07
- Supersedes: none
- Related: ADR-0001 (Rust-stack-recursive VM), `docs/adr/0052` (a `when` clause produces its value on the stack)

## Context

`.resume` on a caught exception is supposed to hand control back to the statement
*after* the `die`, inside the dying routine's own frame, with every frame between
the `die` and the handler still live. mutsu only did this when the `die` and the
`CATCH` were in the **same** `CompiledCode`.

### What was measured (2026-09-07, `raku` vs `target/debug/mutsu`)

The ticket (`todo/deep/resume-does-not-return-to-die-call-site-in-nested-sub.md`)
claimed the existing mechanism was "built for CONTROL exceptions (`warn`), not for
an arbitrary `die`". Two of its three premises are wrong. The measured table:

| # | Shape | raku | mutsu (before) |
|---|---|---|---|
| 01 | `.resume` on a `warn`, 1 frame up | resumes | **resumes** |
| 14 | `.resume` on a `warn`, 2 frames up | resumes | **resumes** |
| 02 | `die` + `CATCH` in the same bare block | resumes | **resumes** |
| 17 | `die` + `CATCH` in the same *sub* body | resumes | **resumes** |
| 15 | `CATCH` that does not resume | block abandoned | **matches** |
| 11 | `fail` (no `CATCH` fires) | returns a `Failure` | **matches** |
| 03 | `die` 1 sub-frame below the `CATCH` | resumes | stops |
| 04 | `die` 2 sub-frames below | resumes | stops |
| 05 | `die` 3 sub-frames below | resumes | stops |
| 06 | `die` in a `for` loop inside a sub | resumes each iteration | stops |
| 07 | `die` in a nested `if`/block inside a sub | resumes | stops |
| 08 | `.resume` inside a `try {}` | `try` yields its tail value | yields `Any` |
| 09 | value of a resumed `die` expression | `Any` | never reached |
| 10 | user `Exception` subclass `.throw` | resumes | stops |
| 12 | `.resume` called twice in one handler | first wins, handler exits | stops |
| 13 | resume an exception an inner `CATCH` rethrew | resumes at the original `die` | stops |
| 16 | handler reads a `$*DYN` set inside the dying sub | sees the **inner** value | saw the outer value |
| 19 | handler vs. the dying sub's `LEAVE` phaser | handler first, then `LEAVE` | `LEAVE` first |

So: `die` + `.resume` already worked, and `warn` already worked *across frames*.
The one axis that fails is **an exception raised in a frame below the one that
installed the handler**. Rows 16 and 19 are the load-bearing measurements — they
say *why*.

### Why it fails

mutsu records a resume point as `resume_ip: Option<(code_fingerprint, ip)>`
(`vm/vm_run_loop.rs`), and `take_resume_ip_for(code)` hands it back **only** when
the fingerprint matches the currently-running `CompiledCode`. A point recorded in a
callee frame is deliberately discarded, because resuming there would jump to an
arbitrary op index in the wrong frame's op array.

That guard is correct and cannot be relaxed: mutsu's VM recurses on the **Rust**
stack for every call (`call_compiled_function_fast` runs the callee's ops in its
own `while` loop, `exec_one` re-enters for nested regions). By the time the `Err`
reaches the region that owns the `CATCH`, every Rust frame between the `die` and
the handler has already been popped by `?`. The continuation the ticket asks for
does not exist any more — there is nothing left to resume *into*.

Rows 16 and 19 show what rakudo actually does, and it is not "save a continuation":
rakudo runs the handler **in the dynamic scope of the throw, before unwinding**.
The handler sees the dying sub's `$*WHERE`, and it runs *before* that sub's `LEAVE`
phaser. `.resume` is then not a jump at all — it is the handler simply returning,
and the `die` evaluating to `Any`.

mutsu already implements exactly that shape for CONTROL exceptions:
`try_resume_safe_control_inline` (`runtime/builtins_control_flow.rs`) runs a
resume-safe `CONTROL` block **inline at the `warn` raise site**, reconstructing the
installing frame's locals from `env` by name, and returns the value the suspended
`warn` evaluates to. That is why rows 01 and 14 pass. Nothing about that mechanism
is specific to `warn`.

## Decision

**A `CATCH` handler that can resume runs at the throw point, in the dynamic scope
of the `die`, before any unwinding.** `.resume` is the handler returning normally;
the throw expression yields `Any` and the dying frame continues with all its Rust
frames intact. A handler that runs inline and does *not* resume still has to
transfer control to the end of its own block, so the throw site returns the
exception **tagged** with the owning region's token and the handler's verdict; the
owning `TryCatch` applies that verdict without re-running the handler body.

This generalizes the proven CONTROL mechanism to `CATCH` rather than inventing a
second one.

### Gate: resume-*capability*, decided at compile time

Registration and inline execution happen only for a `CATCH` block that lexically
contains a `.resume` call, detected by scanning the emitted `catch_start ..
control_start` op range for a `CallMethod`/`CallMethodMut` named `resume`
(`OpCode::TryCatch::catch_resume_capable`). Every other `CATCH` keeps today's
unwinding path bit-for-bit.

The gate is deliberately *capability*, not the CONTROL path's much stricter
*safety* (`control_block_is_resume_safe`, which demands that the block
*unconditionally* resumes). The verdict tag above handles the "ran inline but did
not resume" branch, so the analysis does not have to prove anything — it only has
to be conservative in one direction, and "no `.resume` token in the range" is a
sound proof that the block cannot resume.

### Blocking markers, so no handler is ever skipped

A region that would *stop* an exception from reaching an outer `CATCH` — one with
its own `CATCH` block, or a genuine `try` — pushes a cheap marker entry even when
it is not resume-capable. The throw site only takes the inline path when
`catch_handlers.last()` is itself resume-capable. Without the markers, an inner
non-resuming `CATCH` would be silently skipped in favour of an outer resuming one
(measured row 13), which is a worse answer than not resuming at all.

## Alternatives considered

### A. A saved resume point per frame (what the ticket proposed)

Generalize `resume_ip` into a stack of `(frame, ip, locals)` triples, and on
`.resume` re-enter each saved frame in turn.

Rejected. The frames are Rust frames. Re-entering `bad-sub` at op 3 with its
locals restored is not enough: the *caller* of `bad-sub` is `call_compiled_function_fast`'s
`while` loop, which has already returned; its `saved_stack_depth`, its routine
frame, its `let` marks and its pragma state are gone. Reconstructing them means
giving the VM a flat, heap-allocated frame array and a trampolined dispatch loop —
i.e. a level-2 VM redesign of the kind ADR-0001 §7 explicitly rejects absent a
measured need. It would also be *strictly more* machinery for a *strictly worse*
answer, since it still gets rows 16 and 19 wrong: a saved-continuation resume runs
the handler after unwinding, so the handler cannot see the throw's dynamic scope
and cannot precede the dying frame's `LEAVE`.

### B. Run every `CATCH` inline, ungated

Architecturally the most correct (it fixes rows 16 and 19 for *all* handlers, not
just resuming ones), and it is what rakudo does.

Rejected **for now**, on blast radius, not on principle. `die` is one of the most
heavily exercised paths in the interpreter (`dies-ok`, `lives-ok`, `throws-like`,
every `Failure`, every internal error routed to a `CATCH`), and moving *when* the
handler body runs changes the observable environment for all of them at once. The
gated form gets the whole measured resume table with no effect on any handler that
cannot resume. Ungating is Slice 3 below, and the phaser-ordering rows are the
reason to eventually do it.

### C. Intercept at the bytecode loop instead of at the throw opcodes

Hook `run_range` right after `exec_one` returns `Err` — the deepest point at which
all Rust frames are intact — so *every* error (including internal ones like
division by zero) becomes resumable with one hook.

Rejected. The resumed opcode's stack effect is unknown: after resuming a failed
mid-expression op we would not know how many values it should have pushed. The
throw opcodes (`Die`, `CallFunc("die")`, `CallMethod("throw")`) each have a
statically known effect — push exactly zero or one value — which is what makes the
inline path safe there and unsafe in general.

## Consequences

- `.resume` works from arbitrary call depth, through loops, nested blocks and
  `try`, for `die`, `.throw` and user `Exception` subclasses.
- The handler for a resume-capable `CATCH` now runs in the dynamic scope of the
  throw, which is what rakudo does (row 16) and which also fixes the phaser
  ordering (row 19) for those handlers.
- A resume-capable `CATCH` block's own locals are reconstructed from `env` by name
  for the inline run, and mutated slots are flushed back — the same trade-off (and
  the same helper) the CONTROL path has carried since the cross-frame resumable
  `warn` work. A handler lexical that lives only in a slot and never reaches `env`
  is therefore not visible to an inline run.
- A throw raised while the installing region's *own* code object is executing is
  deliberately left on the pre-existing frame-local `resume_ip` path. That path
  already resumes it correctly, and the inline path would swap `self.locals` for
  an `env` reconstruction of the very same frame and then drop the handler's
  writes to the live slots on restore (`t/resume.t` and
  `t/topic-quoted-method-call.t` catch exactly this).
- Entering a resume-capable region clones its `CompiledCode` into an `Arc`, the
  same cost the `resume_safe` CONTROL path has always paid. It is per *region
  entry*, so a resume-capable `CATCH` inside a hot loop would pay it per
  iteration. Acceptable while the construct is rare; if a real workload shows up,
  the fix is to cache one `Arc` per code object rather than to ungate less.
- `.resume` reached from a *nested VM run* (`EVAL`, a `dies-ok { }` block, the
  `map`/`grep` eager loops) is not inline-resumable: `with_nested_registers`
  clears `catch_handlers` for the nested run, exactly as it already clears
  `resume_ip`. Those shapes keep today's behaviour.

## Implementation status

- **Slice 1 (implemented, 2026-09-07)** — the mechanism above:
  `OpCode::TryCatch::catch_resume_capable`, the `catch_handlers` stack, the
  `catch_inline_verdict` error tag, and the inline runner
  (`runtime/catch_inline.rs`) hooked at `OpCode::Die`, `builtin_die` and
  `Exception.throw`. Pinned by `t/exception-resume-cross-frame.t`, which covers
  every row of the table above, including the rows that already passed.
- **Slice 2 (open)** — row 13: an exception an inner, non-resume-capable `CATCH`
  rethrew cannot be inline-resumed by an outer one, because the inner marker
  blocks the inline path (correctly — see "Blocking markers"). Doing this properly
  means running the *chain* of handlers inline, innermost first, which is
  Alternative B restricted to rethrow.
- **Slice 3 (open)** — ungate: run every `CATCH` inline (Alternative B), which
  closes rows 16 and 19 for non-resuming handlers too. Trigger: a roast/battery
  failure that turns on handler-vs-`LEAVE` ordering, or on a `CATCH` observing the
  throw's dynamic scope.
