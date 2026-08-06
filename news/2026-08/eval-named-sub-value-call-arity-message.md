# A dynamically-called EVAL'd named sub reports a runtime arity error, not a compile-time one

Found via `Template::Mojo` 0.2.2 (`t/00-basic.rakutest` test 16,
`todo/tickets/template-mojo-residual-failures.md`), whose generated template
`sub` is built as source text, `EVAL`'d, stored in a lexical, and invoked
through that lexical:

```raku
sub build() {
    EVAL 'sub t { $^a + $^b }';
}
my &f = build();
f(23);
```

raku: `Too few positionals passed; expected 2 arguments but got 1` — a plain
runtime message, since a value call through `&f` is never
compile-time-diagnosable. mutsu instead reported `Calling t(Int) will never
work with declared signature ()` — the phrasing reserved for a
statically-resolved bare call — with an empty `()` signature to boot, since
placeholder-derived params never populate `param_defs`.

Two independent bugs, both in the path a named sub with only implicit
`^`-twigil placeholder params (`$^a`, `$^b`) takes when it has no cached
compiled bytecode (the `EVAL`'d case) and is called through a value rather
than a static name:

1. `src/vm/vm_dispatch_helpers.rs`'s "compile on-the-fly" branch of
   `vm_call_on_value` never set `suppress_binding_error_enhance`, unlike its
   sibling `compiled_routine` branch just above it — even though the same
   "a value call is never compile-time-diagnosable" reasoning applies. Fixed
   by setting the flag there too.
2. The legacy placeholder binder this call falls into
   (`bind_function_args_values` in `src/runtime/types/binding_signature.rs`)
   had its own message bug once the wrapper was gone: its too-few message
   read "Missing required implicit placeholder parameter $^b" instead of
   raku's `Too few positionals passed; expected N arguments but got M`. Fixed
   to match.

raku also rejects too-many positionals for this shape (mutsu still silently
absorbs them into `@_`), but that half was deliberately left as-is — see
`todo/tickets/template-mojo-residual-failures.md` for why a general fix was
tried and reverted (it regressed `t/placeholder.t`'s `mixed-placeholder`
test, which relies on exactly this leniency for a placeholder sub whose body
also references bare `@_`/`%_`).

Regression test: `t/eval-named-sub-placeholder-arity.t`.
