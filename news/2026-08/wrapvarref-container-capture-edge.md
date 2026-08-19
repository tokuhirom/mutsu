# A captured outer scalar keeps its container across every closure kind, not just a directly nested named sub

`todo/deep/captured-outer-pair-container-alias.md` started as "a Pair built
from a captured outer scalar snapshots its value instead of retaining the
scalar's container" and was re-measured to be much broader: `\($v)` Capture
construction and `Pair.new`, built inside a pointy block, an anonymous
`sub {}`, a bare block, or a class method, all showed the same snapshot
instead of aliasing the source variable's container. The existing fix (a
`GetGlobal`-immediately-followed-by-`WrapVarRef` peephole scan) only ever
detected a directly nested named sub as the reader.

[ADR-0032](../../docs/adr/0032-wrapvarref-container-capture-across-closure-boundaries.md)
generalized the mechanism: `Compiler::emit_wrap_var_ref` now records a
container-capture edge (`container_ref_capture_syms`) at the point it is
emitted, for any name that is not a local of the emitting frame — a single
rule that covers every `WrapVarRef` consumer (`key => $v`, `Pair.new`,
`\($v)`, list-literal elements, meta-identity operands) regardless of what
kind of nested code is doing the reading. A new shared
`Compiler::bubble_container_ref_capture_syms` helper propagates that fact to
whichever ancestor frame actually declares the name — even when the owning
frame is two or more levels up — through the existing decl-site boxing gate
(`needs_cell_ref_capture_slots`, renamed from `needs_cell_named_sub_ref_slots`
to match its generalized contributors). The old peephole scan and the
`ContainerizePair` pop-back hack it forced are gone.

Two false-positive sources surfaced only during implementation, both fixed
before landing: a bareword call argument (`isa-ok($pair, Pair)`) is not a
variable read, so the general-purpose `is rw`/`:=`-bind-source call-arg
tagging must NOT feed the new mechanism at all (not even for its genuine
`Expr::Var` arguments — that call site fires for every plain positional
argument, not only an actual `is rw` one, and boxing a free-variable argument
passed to an ordinary function broke attribute-hash access through it); and
a for-loop's own parameter (`for %h.pairs -> $pair {...}`) deliberately gets
no local slot, so a body read of it looked exactly like a captured outer and
had to be excluded the same way `compute_free_vars` already excludes it from
`free_var_syms`.

New pin: `t/closure-container-capture-alias.t`, covering the pointy-block,
anon-sub, bare-block, class-method, escaping-closure, and Capture shapes the
old mechanism missed, plus a shadow-safety negative control. One probe from
the ADR's measured table (`.VAR.WHICH` identity across a closure boundary)
turned out to be an unrelated, pre-existing bug — `.VAR`'s reflection-object
identity comes from a separate name-keyed env cache with no cross-frame
writeback of its own, confirmed independent by testing the pre-existing
named-sub mechanism against the same shape — filed separately as
`todo/tickets/var-which-identity-across-closure-boundary.md`.
