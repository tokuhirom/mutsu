# `Supply.Seq` taps a `supply { whenever ... }` body

`.Seq` on an on-demand `supply { ... }` ran the body's callback directly, bypassing the
`whenever` machinery, so `supply { whenever $src { ... } }.Seq` never subscribed (the
socket-accept handler in `roast/6.c/MISC/bug-coverage-stress.t` printed nothing). It now goes
through the same tap path as `.list`. `roast/6.c/MISC/bug-coverage-stress.t` is whitelisted.
