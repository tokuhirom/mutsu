# Method calls never push a caller-env frame, breaking CALLER::/callframe()/PROCESS::/DYNAMIC:: reads from inside any method body

Reclassified from `todo/tickets/log-timeline-task-event-recording-empty.md`
(originally filed as a narrow `Log::Timeline`/`given`/role-composed-method
gap) after bisecting the real trigger. The original ticket's framing was
wrong: `given` and role composition are both red herrings. The actual bug is
much bigger and has nothing to do with either.

## Root cause

`push_caller_env`/`push_caller_env_with_code` (`src/runtime/runtime_caller_env.rs`)
is what pushes the calling frame's env onto `self.caller_env_stack`, which
every caller-frame-observing mechanism depends on to walk up the call chain:
`CALLER::`, `callframe()`/`callframes()`, and the `PROCESS::`/`DYNAMIC::`
pseudo-stashes (via `dynamic_pseudo_stash_entries`, `src/runtime/runtime_caller_env.rs:214`,
which explicitly documents needing "the whole caller chain plus the current
frame").

A **sub**/closure call correctly pushes this frame (confirmed: a `PROCESS::`
write at the top level, read from inside 2-3 levels of nested plain `sub`
calls, works correctly both before and after investigating this ticket).

A **method** call never does, on any of its dispatch paths
(`vm_call_method_compiled_cache.rs`, `vm_call_method_compiled_interpret.rs`,
`vm_call_method_compiled_mut.rs`, `vm_method_dispatch.rs`,
`vm_dispatch_helpers.rs` — none of these files reference
`push_caller_env`/`uses_callframe` at all, confirmed by grep). This is
independent of whether the method is compiled/fast-pathed or not, and
independent of role composition — a **flat, top-level, non-role, non-given**
repro already reproduces:

```raku
class C { method reader() { say PROCESS::<$X> } }
PROCESS::<$X> = 42;
C.new.reader();
# raku: 42   mutsu: Nil
```

Two other caller-frame-observing mechanisms confirmed independently broken
the same way, ruling out anything PROCESS::-specific:

```raku
# CALLER:: reading a caller's declared dynamic
class C { method reader() { say CALLER::<$*y> } }
sub outer() { my $*y = 42; C.new.reader(); }
outer();
# raku: 42   mutsu: Nil

# callframe() line number
class C { method reader() { say callframe(1).line } }
sub outer() { C.new.reader(); }   # line 3
outer();
# raku: 3 (the outer() call site)   mutsu: 4 (wrong line entirely)
```

There is an existing, principled mechanism for exactly this: `CompiledCode::uses_callframe`
(`src/opcode.rs`, doc comment at the field) is a compile-time-detected flag —
"a `callframe`/`callframes` call, or a `CALLER::` pseudo-package read/write
op... must be invoked through a frame-pushing call path... so the fast/light
frameless paths exclude it" — but this flag is **only consulted by the
sub/closure call-eligibility checks** (`src/vm/vm_call_eligibility.rs`,
`src/vm/vm_call_fast.rs`), never by any method-dispatch path. Extending the
`emit()`-time detection to also catch `OpCode::GetPseudoStash("PROCESS::" |
"DYNAMIC::")` (tried during this investigation) had **zero effect** — every
sub-call repro already worked before that change (confirming subs already
always push a caller frame, unconditionally, regardless of `uses_callframe`),
and every method-call repro was unaffected after it (confirming the flag is
simply never read on that path). That speculative change was reverted rather
than landed inert.

## Why this is large

- The actual fix needs the method-dispatch call paths (at least 5 distinct
  files/functions, several of them the hottest, most performance-sensitive
  code in the interpreter — every method call in every program goes through
  one of them) to *also* push a caller-env frame, gated the same way subs
  presumably are (or via the same `uses_callframe`-style opt-in, extended to
  cover method bodies too) — without regressing method-call performance for
  the overwhelming majority of methods that never observe their caller.
- Needs to first establish HOW/WHERE sub calls already get this for free
  (not yet located precisely in this investigation — grep confirmed
  `push_caller_env` is called from `vm_closure_dispatch.rs`,
  `vm_call_named_inner.rs`, and `resolution_call_sub.rs`, but which of these
  is actually exercised by the working sub-chain repro above, and whether
  that mechanism can be reused as-is for methods or needs its own
  method-shaped variant, is unresolved).
- `vm_call_light.rs` has an existing, explicit "skip `push_caller_env` for
  speed" fast path for *subs* already (see its own comment at line ~237);
  understanding why that doesn't already break the working sub-chain repro
  (some subs presumably don't take that path, or something else compensates)
  is a prerequisite for reasoning about whether the analogous method fast
  paths can safely gain the same opt-in without the same risk.
- High verification burden: `CALLER::`/`callframe()`/`PROCESS::`/`DYNAMIC::`
  are all used across the existing `t/`/roast suite in various sub/block
  contexts already (many passing tests today); a broad method-dispatch
  change needs the full local + roast sweep, not just the method-shaped
  repros above.

## Affected files

- `src/vm/vm_call_method_compiled_cache.rs`
- `src/vm/vm_call_method_compiled_interpret.rs`
- `src/vm/vm_call_method_compiled_mut.rs`
- `src/vm/vm_method_dispatch.rs`
- `src/vm/vm_dispatch_helpers.rs`
- `src/runtime/runtime_caller_env.rs` (the push/pop/walk machinery to reuse)
- `src/opcode.rs` (`uses_callframe`, if the fix follows that opt-in pattern)

## What it blocks

`Log::Timeline`'s `t/logging.rakutest` tests 10-30 (a real, useful bundled
battery — its `.log`/`.start`/`.end` methods, composed from a parametric
role, read `PROCESS::<$LOG-TIMELINE-OUTPUT>` to decide where to record, and
never see a `given`-scoped or even top-level write made by the caller). The
module's own no-op-when-unset fallback means this fails silently rather than
crashing.

## Repro

```sh
cargo build
timeout 10 target/debug/mutsu -e '
class C { method reader() { say PROCESS::<$X> } }
PROCESS::<$X> = 42;
C.new.reader();
'
# raku: 42   mutsu: Nil
```
