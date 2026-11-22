# TRIR inlines small statically linked callees

ADR-8673 stage 2 (D2): when a TRIR routine calls another TRIR routine whose
body fits a 24-op budget and that it links statically, the compiler splices
the callee's ops into the caller instead of emitting a `CallTr`. The callee's
slots are renumbered into fresh caller slots. A native `is rw` parameter
becomes a direct reference to the caller's slot, and any other parameter
becomes a read-only copy. Its `return`s become jumps to the region's join.

The region keeps what an observer could see of a real call:

- `InlineEnter` records the callee's routine entry lazily, the way stage 1
  defers a real call's entry, and switches the package.
- A `.wrap` installed after linking makes the region fall back to the call.
- A `fail` inside the region becomes the callee's Failure value at the join.
- A call the callee makes re-seeds its free variables (`InlineReseed`).

`MUTSU_TRIR_INLINE=0` turns the splice off. It is part of the precomp cache
key, so a warm cache honours it. `MUTSU_VM_STATS` reports `inlined=`.

Fewer deferred entries are pushed during a call as well. Data `nqp::` ops on
plain-data operands, `nqp::create` of a core collection type and
`nqp::p6scalarwithvalue` no longer count as observers. A generic call that
stays on its linked path no longer counts either.

`t/vm/codegen/adr8673-trir-inline.t` runs one fixture with inlining on, with
it off and with TRIR off, and pins the transcript against rakudo's.
