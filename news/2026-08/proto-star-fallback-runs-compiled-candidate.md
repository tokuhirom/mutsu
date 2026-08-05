# The proto {*} fallback runs the compiled candidate body

ADR-0019 C6d-3, Phase-C half (#5947). The interpreter proto-dispatch entry
`call_proto_dispatch` — the fallback `vm_call_proto_dispatch` takes for a
candidate whose body trips the OTF gate (a class declaration or a `start`
call in the body), for empty-sig-with-args, and for no-candidate errors,
plus the direct `{*}` entry when the proto *body* itself runs through the
interpreter carrier — held its own copy of the retired `call_function_def`
shape: env save, parameter binding, a `run_block(&def.body)` body run (a
fresh compile of the candidate's AST per call), and a hand-rolled rw/env
writeback. The site was dead across the whole `t/` suite in the C6d survey,
so it also had no pin.

The proto-sub arm now delegates the candidate run to `call_routine_def`,
keeping only what is proto-specific: the remaining-candidate collection and
multi-dispatch frame (for `nextsame`/`callsame`), the samewith context, and
the `X::Multi::NoMatch` error construction. The compiled entry enforces
`empty_sig`, binds parameters including rw writeback, pushes the routine
frame, and performs the caller-env writeback merge; an explicit `return`
comes back already unwrapped by `finalize_return_with_spec`, so a surviving
Err-with-return_value is a non-local return and now propagates instead of
being swallowed into `Ok`.

Pinned by `t/proto-dispatch-interpreter-path.t` (13 assertions, every
expectation taken from `raku` first; a breakpoint confirmed the deferred
candidates actually reach the rewired arm). The `is rw` writeback through a
non-trivial proto body remains a pre-existing gap — verified unchanged by
A/B on this rewire — tracked in
`todo/tickets/rw-writeback-through-nontrivial-proto-body-is-lost.md`.

The other C6d-3 site, `types/roles.rs:run_role_submethod`, is dead across
the suite and its `def` is a `MethodDef`, so it is reassigned to Phase D's
class/role work, as the checklist anticipated.
