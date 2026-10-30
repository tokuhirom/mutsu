# Multi sub calls with named arguments hit the resolve cache

`f(1, :flag($t))` on a `multi sub` re-ran the full candidate walk on every call
because `multi_arg_type_keys` refused any named (`Pair`) argument, so the
type-keyed winner cache (`func_multi_resolve_cache`) was bypassed: a call with a
named argument cost 72.8k Ir against 6.7k for the same multi called with
positionals only. `resolve_function_multi_cached_sym` now keys a named argument
by a reserved marker, its name and its value's own type key (the keying the
method side got in #15422); the value-dependence gates (`where`, literal,
subset, coercion, `is rw`, sub-signature) are unchanged, so a family with such a
candidate still resolves per call.

Debug-build callgrind (instruction counts, per call): 809.6k -> 496.5k Ir for
`f(1, :flag($t))` over two candidates. The remaining named-argument cost is the
binder (`bind_function_args_values_inner`), not dispatch. See #15235.
