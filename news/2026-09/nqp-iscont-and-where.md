# `nqp::iscont` and `nqp::where`

Both ops were unsupported (#9346). Scalar::Util's `readonly` is
`nqp::hllbool(nqp::not_i(nqp::iscont(a)))` over a raw parameter, and its
`refaddr` is `nqp::where(a)`.

`nqp::where` compiles to the `.WHERE` method call: rakudo's `Mu.WHERE` *is*
`nqp::where(self)`, so keeping one identity scheme is the whole point.

`nqp::iscont` asks about the operand's container, which the `nqp::` layer never
sees -- `call_nqp_op` decontainerizes every operand at its boundary. So the
compiler hands the op the operand's `.VAR` (the one place mutsu already answers
"which container is this" for a variable, an element or a parameter), and the
new `iscont` op only classifies what that yields: a `Scalar`, a native array's
`*PosRef` element descriptor or a `Proxy` is a container, anything else is a bare
value.

Two more things had to line up for it to agree with rakudo:

- TRIR, the typed fast path, compiles `nqp::` calls from the AST itself and
  passes bare values, so `sub f($p) { nqp::iscont($p) }` answered 0 whenever it
  took that path. TRIR now declines `nqp::iscont` and leaves the routine to the
  bytecode path.
- `.VAR` on a raw (`\p`) parameter consulted its name-keyed descriptor cache
  before asking whether the current binding owns a container at all, so after
  `h($x)` a later `h(1)` still reported the first call's `Scalar`. The
  sigilless-readonly probe now runs before that cache, like the other
  "is there a container right now" probes already did.

Pinned by `t/vm/nqp-iscont-where.t`; the assertion in
`t/concurrency/thread-lock/nqp-istrue-hllize-lock-ops.t` that pinned
`nqp::iscont` as an explicit "Unsupported" gap now checks the op instead.
Scalar::Util goes from 7/9 to 8/9 files: `refaddr` passes, and `readonly`'s last
failure is a raw parameter losing its container when the sub was imported
through `sub EXPORT`, filed as #9410.
