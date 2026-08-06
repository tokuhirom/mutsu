# A non-trivial proto body now resolves its own nested subs against its own table

`vm_try_run_nontrivial_proto_body` (`src/vm/vm_call_func_ops.rs`) OTF-compiles
a proto with a real body (`proto foo($x) { ...; {*} }`, as opposed to a
bodyless `proto foo($x) {*}`) and runs it as compiled bytecode. It passed the
*caller's* `compiled_fns` table straight through to
`call_compiled_function_named` instead of the freshly compiled proto body's
own (`cf.compiled_fns`) — the same "wrong nested-sub table" shape that was
fixed for `call_shared_state_body` while investigating the ADR-0019 C6e-3c
blocker (`todo/deep/c6e-legacy-body-drop-blocked-by-gate-rejected-shapes.md`).

If a non-trivial proto body itself declares a nested `my sub`/`proto`/`multi`,
that nested declaration's `RegisterDecl` opcode would resolve against the
wrong table whenever the proto body's own table diverges from the caller's
(the imported-module case, per the `call_shared_state_body` investigation).
Fixed with the same one-line pattern:
`let fns = cf.compiled_fns.as_deref().unwrap_or(compiled_fns);` before the
`call_compiled_function_named` call.

Pinned by `t/nontrivial-proto-body-own-nested-sub-table.t` against a new
fixture module (`t/lib/NontrivialProtoNestedSub.rakumod`) with a non-trivial
proto body that runs a nested `my sub` before `{*}`.
