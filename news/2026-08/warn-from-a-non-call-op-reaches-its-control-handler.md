# A resumable warning raised from a non-call opcode reaches its CONTROL handler

`raise_resumable_warning` → `try_resume_safe_control_inline` lets a `warn`
raised deep in a call chain reach a `CONTROL { when CX::Warn { … .resume } }`
handler and then resume at the raise site. It worked when the raise happened
inside a **call** (a native method, a builtin, a routine) — that is how
`news/2026-08/a-warning-resumes-at-its-raise-site.md` fixed `Int.Numeric`.

It did not work when the raise happened inside a plain arithmetic opcode.
`"x" x Int` and `1 xx Int` called `write_warn_to_stderr` directly, so no handler
ever saw the warning, and `Test::Util`'s `warns-like` reported `$did-warn` as
`False` (`roast/S03-operators/repeat.t` test 56).

## Two things had to change

**Route the raise through the resumable mechanism.** The four repeat-count warn
sites (`exec_string_repeat_op`, `exec_list_repeat_op`, and the `x` / `xx` arms
of the reduce path in `runtime/builtins_operators_repeat.rs`) now share
`Interpreter::warn_uninitialized_repeat_count`, which calls
`raise_resumable_warning` and then runs the caller-writeback drain a call
boundary would have run.

**Stop a leaf closure from discarding the handler's writes.** That alone still
lost `$did-warn`. `call_compiled_closure_with_topic` skips its caller-writeback
env scan when the closure changed no free variable, has no rw parameters, no
env-write opcode and **makes no calls at all** — without a call, the reasoning
goes, nothing outward can have been mutated. An inline CONTROL handler is
precisely the counterexample: `{ 'x' x Int }` makes no calls, the warning comes
out of an arithmetic opcode, and the handler writes the *installing* frame's
lexicals into this frame's env with no call boundary to mark the mutation.

`Interpreter::inline_control_env_writes` is a counter bumped whenever
`try_resume_safe_control_inline` flushes a handler-mutated name to env. Each
closure frame snapshots it on entry and forces the scan when it moved.
`{ Int.Numeric }` had never shown the bug only because a method call sets
`cc.has_calls`, which forced the scan anyway.

## The "handler runs twice" symptom was a red herring

The original record of this bug reported that the handler body ran twice and
that this was a second, separate defect. It is not: `raku` runs it twice on the
same repro. The repro's trailing `say "d=$d m=$m"` interpolates an *undefined*
`$m`, which is itself a warning, so the second `HANDLER` line is that second
warning's handler run. It disappears under `raku` only because the first handler
run had already given `$m` a value. Reproducing against `raku` before believing
the shape of a failure would have saved the detour.

Pin: `t/warn-from-a-non-call-op-reaches-control.t` (every assertion also passes
under `raku`). This unblocked
`news/2026-08/retired-native-test-util-overrides.md`.
