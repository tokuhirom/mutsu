# `nqp::p6setfirstflag` and `nqp::p6takefirstflag`

Rakudo keeps a `FIRST` flag on each code object. `nqp::p6setfirstflag($do)` sets it, given a
code object's bytecode handle (`nqp::getattr(&f, Code, '$!do')`, not `&f`). After that,
`nqp::p6takefirstflag()` inside that code reads the flag once and clears it. Both ops now
work in mutsu (#11505, ADR-11505 §2.5), which takes the Rakudo `p6*` category to 25 / 26.

The flag belongs to one closure clone: setting it on one clone of a pointy block leaves the
other clones unflagged. mutsu keys it the way it already keys `state` variables, by the
clone's identity, so `&f`, its `$!do` and `&?BLOCK` all reach the same flag.

`p6takefirstflag()` must know which code object it runs in. The parse-time pass that places
`nqp::p6stateinit()` now handles both ops (`parser/p6_frame_state.rs`), using Rakudo's
inlining rules. A statement-level `if` body reads its routine's flag, a closure reads its
own, and the mainline always answers 0.

`p6staticouter` is the last op of the category. ADR-11505 §2.6 now records how Rakudo
behaves for it and the planned implementation.
