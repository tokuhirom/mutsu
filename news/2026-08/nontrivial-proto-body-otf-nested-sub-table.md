# A non-trivial proto body's nested sub now resolves its own compiled table

`vm_try_run_nontrivial_proto_body` (the OTF path for a `proto foo($x) { ...;
{*} }` whose body is more than a bare `{*}`) compiled the proto body and ran
it via `call_compiled_function_named`, but passed the *caller's*
`compiled_fns` table instead of the proto body's own
(`CompiledFunction::compiled_fns`, the ADR-0019 C6e-3c nested-sub table
carrier). A nested declaration inside such a proto body — a `my sub helper`,
another nested `multi`, or a nested `proto {*}` — then failed to resolve its
own `RegisterDecl` compiled key against that mismatched table.

For a proto declared and called in the same compilation unit this was
harmless, since the caller's table already contains everything. It only
diverges for a module-imported proto, where the call site's table and the
proto body's own table are genuinely different objects. Confirmed with a
`rust-gdb` breakpoint on `exec_register_sub_op`
(`src/vm/vm_register_sub_ops.rs`): `primary_compiled` resolved to `None` for
the nested sub before the fix, `Some` after.

Behavior was already correct — the registration path fell back to the
plan's still-present `legacy_body` AST — so this was invisible from the
outside; it only mattered for ADR-0019 C6e-3c's goal of eventually dropping
that field. Fixed with the same one-line pattern already used for
`call_shared_state_body`: prefer `cf.compiled_fns` over the caller's table
when it is available.

This closes the follow-up ticket filed alongside the `call_shared_state_body`
fix (PR #6001) that first found this bug shape. Pinned by
`t/nontrivial-proto-body-nested-sub-compiled.t`.
