# A sunk statement call asks whether its callee is a block only when it matters

`f($x);` as a statement (`SinkPopCall`) decided up front whether the callee
was a code variable holding a bare block, since such a block hands back the
container of its last statement and a Failure stored in it must not be sunk
(`my &c = { $z = f() }; c();` throws nothing, where a sub's result would).
The decision is a by-name lookup of `&f` across the frame, the declaration
scope, env and the module and package lexicals, plus a bare-call eligibility
test that `format!`s and interns `&f` -- about 1.9k instructions on every
statement call, although only a sunk unhandled Failure or unsuccessful Proc
ever reads the answer.

The answer is now computed only when the sunk value would explode. A loop
calling a one-line helper as a statement: 10.3k -> 8.1k instructions per
iteration (callgrind, profiling build). Refs #15737.
