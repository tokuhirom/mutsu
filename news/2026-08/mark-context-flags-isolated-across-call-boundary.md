# Mark-context VM flags no longer leak across a live call boundary

`MarkBindContext` and its sibling one-shot VM flags (`scalar_bind_context`,
`param_raw_bind_context`, `bound_decont_active`, `rebind_context`,
`constant_context`, `array_share_context`/`array_share_source`,
`explicit_initializer_context`, `vardecl_context`) are compiler-emitted
markers set immediately before a `:=` bind target's own store op
(`SetLocal`/`SetGlobal`), meant to be consumed by that very next store. When a
real function or method CALL sat between the mark and its consumer —
`@!other := make();` compiles to `MarkBindContext; ...; CallFuncNamed; ...;
SetGlobal` — the callee's own body used to run with the flag still set, so
ANY vardecl/store inside the callee (e.g. `my uint8 @state = 0..255;`) was
wrongly treated as a bind target too, skipping the Range-to-array
materialization a typed native array needs. This left a bare immutable
`Range` where a mutable typed array was expected and blocked `Crypt::RC4`'s
own dist test suite (`Cannot modify an immutable Range`) on a shape as simple
as:

```raku
class Foo {
    has uint8 @!other;
    method go() { @!other := make(); }
}
sub make() {
    my uint8 @state = 0..5;
    @state[2] = 99;   # died here
    @state;
}
```

`vm_run_loop.rs`'s nested-run boundary (`EVAL`, `dies-ok`/`lives-ok` blocks)
already isolated this exact flag family around its `f(self)` call, but that
mechanism only runs for a nested Rust-level `run()` invocation — not for an
ordinary compiled function/method call, which pushes call frames in-place in
a flat bytecode dispatch loop.

Fixed by adding a `MarkContextGuard` RAII type
(`src/vm/vm_call_state_guard.rs`) that saves, clears, and restores the whole
flag family on `Drop` — so it isolates correctly on every exit path
(explicit return, `fail`, error, and even a Rust panic unwind) without a
matching manual restore at each one. The guard is installed at the top of
every call-dispatch function found to run a callee's compiled body without
going through `vm_run_loop.rs`'s existing boundary:
`call_compiled_function_light_spec`, `call_compiled_function_positional_light`,
`call_compiled_function_fast`, `call_compiled_function_named_inner`,
`call_compiled_closure_with_topic` (covering `call_compiled_closure`),
`call_compiled_method`, `call_compiled_method_fast` (method dispatch — not
named in the original investigation, found by testing the same bug shape
through a method-call boundary), the two lazy-list/`gather` body runners
(`force_lazy_list_vm_inner`, `force_lazy_list_vm_n_inner`), the `xx`-repeat
thunk driver (`vm_xx_repeat_thunk`), and the `Lock::Async.protect` inline
block runner (`exec_protect_block_inline`).

The whole flag family was isolated (not just `bind_context`, the one flag
proven to leak) to match `vm_run_loop.rs`'s existing precedent and guard
against the next instance of this bug class — every flag's write site was
audited first (`git grep '\.<flag> = true'`) to confirm each is a simple
one-shot marker consumed by the very next store op, with no call boundary
relying on any of them propagating across a call.

Regression test: `t/bind-through-call-boundary-vardecl-leak.t` (sub-call,
method-call, and multi-level call-chain boundaries, plus a check that the
outer bind itself still functions correctly).
