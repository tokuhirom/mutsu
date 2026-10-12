# A subscript argument to a frame-lexical sub no longer walks every routine

A call like `rotl($A[2], 10)`, where `rotl` is a `my sub` the compiler keeps
frame-lexical (ADR-0113), first asks whether the callee binds that argument to
the caller's container (`is rw`, `\x`). The question went to the routine
registry, which never holds a frame-lexical routine, and then to a by-name
scope resolution that ended in a scan over every registered routine's key --
on every call. With `use Test` loaded that scan was most of the cost of
Digest::RIPEMD's `rmd160`, whose 80-round reduce calls `rotl` twice per round.

The gate now asks the chunk's own frame-lexical routine first, the same one
the call itself dispatches to. That is also a correctness fix: the by-name
question found nothing, so `sub f($x is rw) { ... }; f(@a[1])` inside a
routine died with "expects a writable container" instead of writing the
element.

Digest 1.1.0's `t/ripemd.t` on a release build went from 2m10s to 1m37s,
against rakudo's 54s on the same box (2.54x to 1.79x; #11686).
