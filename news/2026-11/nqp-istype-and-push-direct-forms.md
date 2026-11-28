# `nqp::istype` and `nqp::push_s` have a direct form in the VM

Every generic `nqp::` op runs the same wrapper: an argument vector, Proxy fetches, a REPR
check, and a name-keyed walk of the op tables (`call_nqp_op` and its siblings match the op's
name against hundreds of string arms). For `nqp::istype` that wrapper costs more than the type
check itself. JSON::Fast's encoder asks `istype` up to fifteen times for each value it writes,
and pushes three or more strings onto its output with `nqp::push_s`.

Both now have a direct form beside the existing pure-op, identity and list fast paths
(`exec_nqp_istype_op`, `exec_nqp_push_typed_op`). It runs for operands that need no
normalization (no `VarRef`, no `Proxy`). Each form shares its body with the table arm:
`nqp_istype_answer` for `istype`, `nqp_push_typed` for `push_s` / `push_i` / `push_n`.
`push_s` of a plain Str now shares the string rather than copying its text.

`bench-json-fast-encode` (`scripts/bench-det.sh`, `+jit`, local): 944.0M -> 860.5M
instructions (-8.8%) and 440,451 -> 350,012 allocations (-20.5%). The wall-clock section did
not move outside the local noise (~0.10-0.13 s in both builds).
