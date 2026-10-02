# A module's top-level routines no longer leave registration markers in the importer's env

Every `sub` registration records a *registration clone id* — what a `state`
variable's scope, a non-local `return` and a `wrap` chain key on — under an
internal `__mutsu_callable_id::Pkg::name` env entry. Because a module's
mainline runs in the env of the frame that loads it, every routine of every
loaded module left one such entry in the importing program's frame env. After
`use Cro::HTTP2::RequestParser` that was 197 of the 824 entries each frame env
carried, and each copy-on-write deep copy of a frame env copied all of them.

A registration that a module's mainline makes directly now goes to a
per-interpreter table instead; nested registrations (subs inside routines,
blocks and loops) keep their lexical env marker, and readers consult the env
first and the table second. A `Promise(supply { whenever … })` loop under that
`use` deep-copies 22% fewer env entries per iteration (24,293 → 18,955).

This is the first slice of ADR-0084 (#7817); the type/package names and enum
values that make up most of the rest of the env are next.
