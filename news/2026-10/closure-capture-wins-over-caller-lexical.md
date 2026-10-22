# A compiled closure's captured lexical wins over a same-named caller binding

`call_compiled_closure` merged a closure's captured environment under its
caller's: a caller lexical that merely shared a name shadowed the closure's own
binding, so lexical scoping degraded into dynamic scoping wherever the escape
analysis had left a capture unboxed. The whole plain-lexical capture now
overwrites the caller's binding (ADR-0055 slice 2; dynamics, `self`, the topic
and `__mutsu_*` metadata keep their live resolution, and a closure whose outer
is the live program scope still reads it through the static link).

Two closure sources relied on the old caller priority and were fixed at their
source: the `substr-rw`/`subbuf-rw` Proxy no longer carries a snapshot of the
whole scope (it names its source variable and reads it live), and an
escaping closure created inside `EVAL` boxes the caller scalars it captures
into shared cells, so the caller's later writes stay visible to it (#7546).
