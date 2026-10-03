# A closure call no longer drops a dynamic-variable write that lands on its capture-time value

Template::HAML's `- tab-down 2` had no effect after `- tab-up 2` (but worked for
`tab-up 2` / `tab-down 1`). Each HAML statement runs as a block, and the helper it
calls writes `$*HAML-TAB-OFFSET`. When a block returns, the closure-call writeback
skips a captured name the body never references if its value is still the one the
block captured at creation — the guard that keeps a closure's own lexical capture
from leaking into a caller's unrelated same-named lexical. A dynamic variable is
not a lexical capture, though: the body resolves it through the live caller
chain. So `my $b = -> { dn 2 }` created while `$*OFF` was 0, called after `up 2`
made it 2, set it back to 0 — the capture-time value — and the write was thrown
away.

Dynamic-variable names are now exempt from that identity skip, so the callee's
write always reaches the declaring caller. This makes Template::HAML's
`t/0390-tab-up-down.rakutest` and `t/0780-direct-emit-parity.rakutest` pass
(#10996). Regression test: `t/routines/closure/closure-dynamic-var-restored-writeback.t`.
