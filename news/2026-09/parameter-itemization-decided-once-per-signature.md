# Parameter itemization is decided once per signature, not once per bind

Binding an argument to a plain `$` parameter itemizes it -- Raku's binder puts
the value in a Scalar container, so `f([1, 2])` binds `$v` as `$[1, 2]`, one
element in list context. Sigilless (`\v`), `is raw` and `is rw` parameters bind
the raw value, and `@` / `%` / `&` parameters bind the container itself.

That decision depends only on the parameter's *declaration*. Yet
`itemize_plain_scalar_param` re-derived it on every bind of every call, and the
derivation is not cheap: two scans of the parameter's `traits: Vec<String>` with
string compares (`== "invocant"`, then `== "raw" || == "rw"`), a multi-character
`starts_with`, and then `itemize_scalar_store`'s own name guard (`== "_"`,
`starts_with('&')`, `starts_with("__mutsu")`) on top. `perf` on `bench-fib` put
`itemize_plain_scalar_param` at 1.9% and `itemize_scalar_store` at 1.5%.

A routine's signature is fixed, so the answer is now settled once. Following the
same shape as `param_fast_types` from
`news/2026-09/light-call-type-checks-answer-from-a-precomputed-tag.md`:

- `Interpreter::param_binds_itemized_scalar(pd)` is the predicate, now written
  in one place and folding in the name half of `itemize_scalar_store`'s guard so
  it answers the whole question.
- `CompiledFunction::param_itemize_on_bind` holds the per-parameter answer,
  filled by `precompute_param_name_syms` alongside the type tags.
- `itemize_scalar_store` splits into `name_is_itemize_exempt` (the name half)
  and `itemize_scalar_store_value` (the value half), so a caller holding the
  precomputed flag calls straight into the value half.
- `Interpreter::bind_itemize_param(cf, i, val)` is the one place the three light
  call sites go through, with a fallback to the per-bind derivation for a
  hand-built chunk that never ran the precompute.

Measured on a release build with a temporary same-binary env switch, pinned to
one core: `bench-tak` retired instructions **-2.28%**, `bench-fib` **-1.60%**,
`method-call` +0.05% and `bench-class` +0.03% (neither takes this path).

`t/param-itemize-on-bind.t` pins the outcome for each declaration shape -- plain
`$` with an Array, a Hash and a scalar, two parameters itemized independently,
`is raw`, `is rw`, sigilless, `@`, and `&` -- all verified against `raku`, so
the precomputed flag cannot drift from the predicate it was derived from.
