# Investigated: does the method-call attribute-shaped-source leak also affect plain sub calls? No.

`call_compiled_method`/`call_compiled_method_fast`'s exit-time `rw_writeback`
loop (`src/vm/vm_method_dispatch.rs`) used to insert every `(source_name,
val)` pair from `rw_bindings` into the caller's merged env verbatim —
including entries where `source_name` is an ATTRIBUTE-TWIGIL-SHAPED
pseudo-key (`"%!plugin-config"`, `"!x"`, ...), produced when a named
`:$scalar` parameter is bound from an `@`/`%` source that is itself an
attribute expression (`:%!attr`, `:$.attr`). That pseudo-key is not a genuine
caller lexical; planting it let `reconcile_attrs`' `:=`-recovery candidate
scan (run at a *different* method's exit) mistake it for a real `:=` binding
and silently overwrite an unrelated instance's own same-named attribute
(Cro::HTTP::Router's `http-router-plugin.rakutest` "Local configuration in
included route handler not affected by outer"). Fixed by skipping
attribute-twigil-shaped `source_name`s in that one loop.

`src/runtime/types/mod.rs`'s `apply_rw_bindings_to_env` is the analogous
exit-time writeback for regular SUB/FUNCTION calls (used by
`dispatch_proto.rs`, `dispatch.rs`, `resolution_call_sub.rs`,
`builtins_operators_fallback.rs` — not the method-call path fixed above), and
its non-slurpy branch has the same shape of unconditional insert by
`source_name`. This was left unverified when the method-path fix landed.

## Investigation

Built a sub-call analogue of `t/method-rw-writeback-attr-source-no-leak.t`:
a plain `sub f(:$plugin-config) { $plugin-config }` called as
`f(:%!plugin-config)` from inside a method (`Owner.spawn-all`, looping over
several `Handler` instances that each subsequently call their own
`.copy-with()`, which does `self.bless(:$!plugin-config)` and so exercises
`reconcile_attrs`). It does not reproduce — every handler keeps its own
attribute (byte-identical to `raku`).

Traced why with `rust-gdb` breakpoints (not `eprintln!` — see the repo's
debugging guidelines): `apply_rw_bindings_to_env`'s `rw_bindings` parameter
is empty on every call for this shape. `bind_function_args_values` does
reach its "Slice 2d named follow-up" branch
(`named_scalar_container_share_eligible(pd)` is true), but the
`arg_sources` it reads via `take_pending_call_arg_sources()` is `None` —
despite the bytecode compiling the correct
`"plugin-config=%!plugin-config"` encoding into `arg_sources_idx`, and
`exec_call_func_op` still holding that `Some(...)` value a few lines before
dispatch. Something in `dispatch_func_call_inner`'s call-eligibility
fast-path selection (`vm_call_func_ops.rs`) — the exact branch wasn't fully
isolated; `call_shares_container_into_named_scalar_param`, the guard meant to
exclude this shape from the light-call fast path, was never even entered —
consumes or bypasses the pending `arg_sources` before
`apply_rw_bindings_to_env` would see it, so the vulnerable insert never
executes for a plain sub call.

Separately confirmed the *feature* itself (named scalar param sharing an
`@`/`%` caller container) does work correctly for a plain lexical source
(`f(:plugin-config(@a))` where the sub body does `$plugin-config.push(...)`)
— so this isn't a case of the whole mechanism being unreachable, just this
one attribute-sourced-named-arg shape happening not to reach it via the
fast-path dispatch a sub call takes (methods apparently don't have — or
don't take — the same fast-path shortcut).

## Conclusion

The corruption risk is method-call-specific, as already fixed. Pinned with
`t/sub-rw-writeback-attr-source-no-leak.t`, the sub-call twin of the
existing method-path regression test, so a future change to the sub-call
fast paths that starts populating `rw_bindings` for this shape would be
caught — at which point `apply_rw_bindings_to_env` would need the same
attribute-twigil guard `vm_method_dispatch.rs`'s `rw_writeback` loop already
has.
