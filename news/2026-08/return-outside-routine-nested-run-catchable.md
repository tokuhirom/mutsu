# A `return` escaping through EVAL/a nested run is catchable again — and the real root cause was a compiler bug, not the `nested_run_depth` guard

`todo/deep/return-outside-routine-uncatchable-inside-nested-run.md` recorded that
`try { EVAL(q[gather { return 1}]); CATCH { default { .message.say } } }; say "reached"` printed
"reached" in real `raku` but aborted the whole program in mutsu — the `X::ControlFlow::Return`
escaped uncaught past the `CATCH`. The ticket's own diagnosis pinned this on
`vm/vm_run_loop.rs`'s `run()` only converting an escaped raw `CX::Return` signal into a catchable
typed exception when `self.nested_run_depth == 0`, and suggested the fix needed to walk the actual
VM call-frame stack instead of that blunt depth counter.

That diagnosis turned out to be a red herring. Bisecting the compiled bytecode
(`mutsu --dump-bytecode`) showed the `return` was never even reaching `vm_run_loop.rs`'s
conversion check unconverted — it was being raised, correctly typed as `X::ControlFlow::Return`,
**outside** the `TryCatch` opcode's own protected instruction range entirely. The actual bug was in
`compiler/helpers_control_flow.rs`'s `compile_try_region`: a block's last real statement is only
sunk in place (as opposed to being kept as the block's value) when a `CATCH`/`CONTROL` phaser is
*textually the last thing in the block* — Raku treats a phaser declaration as occupying a slot in
the statement sequence for tail-position purposes, even though it doesn't run in textual order.
Verified against `raku`:

```
sub f { 42; CATCH { default { } } }; say f();   # Nil (phaser after: 42 is sunk)
sub f { CATCH { default { } }; 42 }; say f();   # 42  (phaser before: 42 is the tail)
```

mutsu's compiler always kept the pre-CATCH statement's value on the stack regardless of the
phaser's position, deferring its sink past the `TryCatch` opcode's protected range whenever the
phaser textually followed it (the common `try { WORK(); CATCH {...} }` idiom). So an exception
raised while forcing that deferred value — e.g. sinking an un-forced lazy `gather` returned by
`EVAL(...)` — escaped past the very `CATCH` meant to catch it. `compile_try_region` now tracks the
phaser's textual position (ignoring interstitial `SetLine` markers) and only discards the tail
value when the phaser genuinely follows it, matching Rakudo exactly, without touching
`vm_run_loop.rs`'s `nested_run_depth`/`routine_stack` guard at all — that guard's own history (a
previous version of this exact check caused a real regression, commit `547422bab`) is left intact.

A second, related gap was found and fixed while building the ticket's requested "both directions"
repro matrix: `test_fn_lives_ok`/`test_fn_dies_ok` (`src/runtime/test_functions/eval_exception.rs`)
treated ANY `Err` from running their block as "the block died", including a live `return` signal
meant for a routine outside the assertion's own nested-run boundary (e.g. `lives-ok { $cb() }`
where `$cb` was captured by, and its `return` targets, the sub that also called `lives-ok`). Now a
live `is_return()` signal propagates transparently instead of being misreported as a failed
assertion, matching Rakudo (where such a `lives-ok` call is never even recorded — the enclosing sub
returns first). `last`/`next`/`redo` were deliberately left out of that fix — they lack an
equivalent compile-time "no enclosing loop" check the way `return`'s
`OpCode::ReturnFromNonRoutine` does, so blindly propagating them risked masking a genuinely-dead
`lives-ok { last }` (no loop anywhere) as a silent abort instead of a reported failure; see
`todo/tickets/lives-ok-dies-ok-last-next-redo-not-propagated.md` for that follow-up.

Pinned by `t/try-catch-tail-statement-sink.t` (11 assertions covering both phaser-before and
phaser-after tail position, `try`/`do`/sub/CONTROL/LEAVE contexts, and the ticket's exact repro)
and `t/lives-ok-dies-ok-return-propagates.t` (the `lives-ok`/`dies-ok` transparent-propagation
fix). `t/tap-callback-nonlocal-return.t` (the Cro streaming-parser motivation for the
`nested_run_depth` guard) and `roast/S32-exceptions/misc.t` (already whitelisted) both continue to
pass unchanged.
