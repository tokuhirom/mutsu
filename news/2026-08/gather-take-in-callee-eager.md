# A take inside a called routine no longer corrupts lazy-gather suspension

The lazy-gather pull driver (`force_lazy_list_vm_n_inner`) runs the gather
body's compiled code with a take limit and suspends by unwinding a signal
from `take_value` when the limit is reached, snapshotting the body's ip,
stack, and locals as coroutine state. That design assumes the `take` fires in
the body's own frame. When the body *calls* a routine and the take fires
inside the callee (`gather trip(5)` where `trip` takes inside its `for`
loop), the signal unwound the callee frames and left the saved ip pointing at
the body's call op with its arguments already drained — resuming then
underflowed the VM stack ("Interpreter stack underflow in CallFunc") for a
compiled callee, or silently lost every element after the first for an
interpreter-arm callee.

`take_value` now compares the live call-frame depth against the depth the
pull driver recorded at entry (`lazy_pull_entry_call_depth`, saved/restored
per pull so nested pulls compare against their own entry). A take arriving
from a nested call frame skips the suspension and keeps collecting eagerly:
the pull over-produces but stays correct, and a take at the body's own depth
still suspends lazily as before (pinned by `t/gather-take-in-callee.t`,
including an infinite own-frame gather).

Found by the ADR-0019 C6e-2a gate widening: `roast/integration/
advent2012-day04.t`'s `triplets(\N)` used to reach the interpreter arm, whose
wrong-but-quiet variant of this bug passed the test only because that gather
takes exactly once. Routing it through the compiled entry exposed the crash —
but the bug was reachable on `main` with a plain `$n` parameter all along.
One adjacent pre-existing wrongness remains and is ticketed:
`todo/tickets/do-for-over-lazy-gather-drops-first-value.md`.
