# `apply_rw_bindings_to_env` (sub/function `is rw` writeback) may have the same attribute-shaped-source leak as the method path, unverified

## Context

`src/vm/vm_method_dispatch.rs`'s `call_compiled_method`/`call_compiled_method_fast`
had an exit-time `rw_writeback` loop that inserted every `(source_name, val)`
pair from `rw_bindings` into the caller's merged env verbatim — including
entries where `source_name` is an ATTRIBUTE-TWIGIL-SHAPED pseudo-key
(`"%!plugin-config"`, `"!x"`, ...), produced when a named `:$scalar`
parameter is bound from an `@`/`%` source that is itself an attribute
expression (`:%!attr`, `:$.attr` — the "Slice 2d" named-scalar-aliases-
container-source rule in `bind_function_args_values`). That pseudo-key is
not a genuine caller lexical; planting it let `reconcile_attrs`' `:=`-
recovery candidate scan (run at a *different* method's exit) mistake it for
a real `:=` binding and silently overwrite an unrelated instance's own
same-named attribute — Cro::HTTP::Router's `http-router-plugin.rakutest`
"Local configuration in included route handler not affected by outer".
Fixed by skipping attribute-twigil-shaped `source_name`s in that ONE loop
(see `news/2026-08/rw-writeback-attr-shaped-source-leak.md`).

## Unverified, out of scope for that fix

`src/runtime/types/mod.rs`'s `apply_rw_bindings_to_env` is the ANALOGOUS
exit-time writeback for regular SUB/FUNCTION calls (used by
`dispatch_proto.rs`, `dispatch.rs`, `resolution_call_sub.rs`,
`builtins_operators_fallback.rs` — NOT the method-call path fixed above).
Its non-slurpy branch (`target_env.insert(source_name.clone(), updated)`)
has the *same shape* of unconditional insert by `source_name`, so a sub
call of the form

```raku
sub f(:$plugin-config) { ... }
class C {
    has %!plugin-config;
    method go() { f(:%!plugin-config) }
}
```

may plant `"%!plugin-config"` into `go`'s caller env the same way, with the
same downstream risk if a *different* instance's method (sharing that env)
gets its own same-bare-name attribute corrupted by `reconcile_attrs`.

This was NOT verified — no repro was constructed, and the method-path fix
was scoped narrowly (single function, single call site) to match the
original ticket and avoid touching an unrelated dispatch path without
evidence. `apply_rw_bindings_to_env`'s slurpy-element branches
(`target_env.insert(source_name.clone(), Value::array_with_kind(...))` /
the scalar slurpy-element case) may have the same issue too.

## Suggested attack

1. Try to reproduce with a sub-call analogue of
   `t/method-rw-writeback-attr-source-no-leak.t` (swap the `copy-with`
   method for a plain `sub`, called with `:%!attr`/`:$.attr` from inside a
   method).
2. If it reproduces, apply the same guard used in
   `vm_method_dispatch.rs`'s `rw_writeback` loop: skip
   `target_env.insert(source_name.clone(), ...)` when `source_name` is
   attribute-twigil-shaped (reuse or duplicate
   `is_attr_twigil_shaped` from `vm_method_dispatch.rs`, or hoist it
   somewhere both files can share it).
3. If it does NOT reproduce (e.g. sub dispatch never shares the caller's
   env the same way method dispatch does), close this ticket with a note
   explaining why the method-only shape is the only vulnerable one.
