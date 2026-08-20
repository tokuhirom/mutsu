# `EVAL ..., context => $frame` — a `return` in the snippet must target the *context* frame

**Status (2026-08-20): still open, and the design is now recorded as
[ADR-0037](../../docs/adr/0037-eval-context-frame-owns-the-return-target.md).**
Read the ADR for the full analysis, the raku measurements, the decision, and
the five-slice implementation plan. This file keeps only the finding and the
two corrections the re-verification pass made to it.

`EVAL`'s `context` argument names the frame the snippet is compiled in. mutsu
takes the *package* from it (`news/2026-08/eval-context-argument.md`) but
nothing else, so control flow still resolves against the frame that called
`EVAL`. A `return` in the snippet therefore returns from whatever routine
happens to enclose the `EVAL` call, instead of behaving as it would at the
context frame.

## Correction 1 — the original repro passes on `main` for the wrong reason

The repro this file was filed with used `sub thrower($code)`. A sub **with a
positional parameter** is dispatched through
`call_compiled_function_positional_light`, which pushes no `RoutineFrame` at
all, so `enclosing_routine_exists()` answers `false` inside it and the
snippet's `return` throws — the right answer, produced by an unrelated bug.
Use the 0-arg form, which still reproduces the documented wrong output:

```raku
use MONKEY-SEE-NO-EVAL;
sub thrower() {
    my $ctx = CALLER::;            # names the mainline
    my $r = 'no-throw';
    { EVAL 'return 1', context => $ctx; CATCH { default { $r = .^name } } }
    say "thrower saw: $r";
    return 'thrower-end';
}
say thrower();
say "still alive";
```

```
raku                                    mutsu
thrower saw: X::ControlFlow::Return     1
thrower-end                             still alive
still alive
```

**Any fix validated against a param-bearing repro validates against noise.**

## Correction 2 — a second, independent defect found while verifying

`push_routine_with_location` is called from only two sites
(`src/vm/vm_call_fast.rs:168`, `src/vm/vm_call_named_inner.rs:90`). The two
**light** sub dispatch paths (`src/vm/vm_call_light.rs`,
`vm_call_light_typed.rs`) push no routine frame, so the whole `routine_stack`
is unfaithful inside any sub that qualifies for them. Measured on `main`
(`raku` answers `1` for every row):

| declaration | path | mutsu, body `{ EVAL 'return 1'; return 2 }` |
| --- | --- | --- |
| `sub zero()` | fast (pushes) | `1` |
| `sub pos1($x)` | positional-light | `X::ControlFlow::Return` |
| `sub named1(:$x)` | light | `X::ControlFlow::Return` |
| `sub arr(@x)` / `opt($x?)` / `slurp(*@x)` | full named (pushes) | `1` |

So *un*contextualised `EVAL 'return …'` is already wrong inside any
param-bearing sub. `caller_frame_package()`, `executing_source_file()` and
backtrace rendering read the same stack and share the gap. ADR-0037 Slice 1
fixes this and is a prerequisite for the context mechanism, since the context
identity is captured from the same stack.

## Correction 3 — the ticket's proposed fix does not match raku

The original proposal (convert an escaping CX::Return at the top-level run
loop when no routine exists at or below the context depth) is rejected in
ADR-0037 §3(a): raku raises the exception **at the `return`**, so a `CATCH`
lexically around the `EVAL` catches it and the enclosing routine runs to
completion. It also cannot express the live-routine-context case, where raku's
`return` unwinds *past* the `EVAL` caller to the frame the context names.

## Already fixed, do not re-investigate

The "related, separable half" this file recorded — `EVAL 'gather { return 1 }'`
not throwing — is fixed (`news/2026-08/eval-statement-sink-forces-lazy-result.md`).

## What it blocks

`t/throws-like-gather-sink.t`'s remaining subtests and part of
`t/emit-done-controlflow.t`, both under `MUTSU_REAL_TEST=1` — rakudo's real
`Test.rakumod` `throws-like` is exactly this shape
(`my $caller-context = $*THROWS-LIKE-CONTEXT // CALLER::;` then
`EVAL $code, context => $caller-context` inside a `subtest`), so every
`throws-like '<code that returns>', X::ControlFlow::Return` reports the code as
not having died. See `todo/deep/vendor-real-test-module.md`.
