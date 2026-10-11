# nqp::bindpos binds a variable's container; unassigned `$!reified` slots are null

`nqp::bindpos($list, $i, $var)` now hands the op the variable's shared cell, so a later
assignment to `$var` shows through the slot, in a raw list and in an Array's `$!reified`
storage alike. A slot of `$!reified` that was never assigned (`@b[3] = 1` leaves 0..2
open) answers null to `nqp::atpos`/`nqp::isnull` (#12786).
