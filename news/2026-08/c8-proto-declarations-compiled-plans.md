# ADR-0019 C8: proto declarations register from typed plans

C8 asked to migrate `RegisterProtoSub` and `RegisterProtoToken` off the
generic `stmt_pool`, and to compile the `{*}` dispatch instead of rewriting
the proto's AST body on every call. Both opcodes were removed outright: a
`proto sub`/`proto method` now lowers to a typed `CompiledProtoDeclPlan`
(mirroring `CompiledSubDeclPlan`/`CompiledRoleDeclPlan`) and a `proto
token`/`proto rule` LTM marker lowers to a `CompiledDeclPlanRef::ProtoToken`
carrying just its name inline — both register through the same `RegisterDecl`
opcode sub/class/role declarations already use, so proto declarations now
also benefit from the conservative env-sync gate and `has_inner_subs`
detection that key off `RegisterDecl`, closing a latent gap where a proto
nested inside a routine body was invisible to both.

The compile-time half: a non-trivial proto body (anything beyond an empty
body or a bare `{*}`) has its `{*}` placeholder rewritten to a
`__PROTO_DISPATCH__()` call once, at declaration compile time, then compiled
through the same `compile_sub_body` ordinary subs use. The VM's
`vm_try_run_nontrivial_proto_body` now runs that pre-compiled bytecode
directly when the resolved proto carries it, instead of re-rewriting and
OTF-compiling the AST on every call — the OTF-compile-and-cache path stays
only as a defensive fallback for a hand-built `FunctionDef` that never went
through plan registration.

Following `CompiledRoleDeclPlan`'s own precedent (which still carries
`legacy_body`), `CompiledProtoDeclPlan` keeps a `legacy_body: Vec<Stmt>` field
for now: a registered proto's `FunctionDef` still needs its raw body for the
pure-interpreter fallback reached from the user-operator dispatch path
(`call_proto_function`, `builtins_operators_fallback.rs`) and for judging
triviality (`vm_resolve_trivial_proto_candidate`). Dropping it is a later
box, matching how C6's own `legacy_body` removal was split out from C1-C5.

Pinned by two new compiler unit tests
(`nontrivial_proto_declarations_compile_their_dispatch_body`,
`trivial_proto_declarations_compile_no_dispatch_body`) plus the full existing
proto/multi test surface (`t/proto-*.t`, `t/our-proto-*.t`,
`t/multi-signature-alternates.t`, and the whitelisted
`roast/S05-grammar/proto*.t` / `roast/S06-multi/*.t` / `roast/S12-methods/multi.t`
files). Full `t/` (27,761 tests) and the roast whitelist pass unchanged.
