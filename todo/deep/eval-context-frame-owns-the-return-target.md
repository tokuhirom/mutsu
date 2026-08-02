# `EVAL ..., context => $frame` — a `return` in the snippet must target the *context* frame

`EVAL`'s `context` argument names the frame the snippet is compiled in. mutsu
takes the *package* from it (`news/2026-08/eval-context-argument.md`) but nothing
else, so control flow still resolves against the frame that called `EVAL`. A
`return` in the snippet therefore returns from whatever routine happens to
enclose the `EVAL` call, instead of raising `X::ControlFlow::Return` when the
context frame has no enclosing routine.

## Minimal repro

```raku
use MONKEY-SEE-NO-EVAL;

sub thrower($code) {
    my $ctx = CALLER::;
    my $r = 'no-throw';
    {
        EVAL $code, context => $ctx;
        CATCH { default { $r = .^name } }
    }
    say "thrower saw: $r";
    return 'thrower-end';
}
say thrower('return 1');
say "still alive";
```

```
raku                       mutsu
thrower saw: X::ControlFlow::Return    1                      <- returned from thrower
thrower-end                            thrower saw: no-throw
still alive                            thrower-end
                                       still alive
```

`thrower` is called from the mainline, so `$ctx` names the mainline — which is no
routine — and raku raises `X::ControlFlow::Return`, which the surrounding `CATCH`
handles. mutsu propagates a raw CX::Return signal that unwinds out of `thrower`
instead, and the snippet's value (`1`) becomes `thrower`'s return value.

Note the *un*-contextualised behaviour is already correct and must stay:

```raku
sub f() { EVAL 'return 1'; return 2 }
say f();     # 1 in both raku and mutsu
```

so this cannot be fixed by unconditionally converting an escaping return at the
EVAL boundary.

## Why it is not a small change

The context value is a `Stash` instance carrying only a name and an origin
package (`Interpreter::eval_context_package`, `src/runtime/accessors_stash.rs`).
Getting the return target right needs the *frame*, not the package:

1. `CALLER::` must record which routine frame it was taken from — the routine
   stack depth at capture time — alongside the origin package it already records.
2. `builtin_eval` must carry that depth into the EVAL'd unit.
3. The escaping-return path must consult it: an escaping CX::Return converts to
   `X::ControlFlow::Return(out-of-dynamic-scope)` when no *non-block* routine
   frame exists at or below the context depth, and otherwise propagates as today.
   The equivalent rule for the uncontextualised case already lives in
   `vm_run_loop.rs` (`e.is_return() && self.routine_stack().is_empty() &&
   self.nested_run_depth == 0`); this generalises "empty" to "empty below the
   context frame". Block frames must not count — `exec_try_catch_op` pushes one
   for every bare `{ ...; CATCH { } }`.

A related, separable half: `EVAL 'gather { return 1 }'` does not throw at all in
mutsu, because the snippet's tail `gather` is returned unforced and its `return`
never runs. raku sinks it inside the EVAL and raises `X::ControlFlow::Return`.

## What it blocks

`t/throws-like-gather-sink.t` (all three `return` subtests) and part of
`t/emit-done-controlflow.t` under `MUTSU_REAL_TEST=1` — the real
`Test.rakumod`'s `throws-like` is exactly this shape:

```raku
my $caller-context = $*THROWS-LIKE-CONTEXT // CALLER::;
subtest {
    ...
    EVAL $code, context => $caller-context;
    ...
    CATCH { default { pass $msg; ... } }
}
```

so every `throws-like '<code that returns>', X::ControlFlow::Return` reports the
code as not having died. See `todo/tickets/vendor-real-test-module.md`.
