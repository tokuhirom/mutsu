# A definite constant return (`--> Nil`, `--> True`) no longer forces the full call path

A routine whose signature names a definite return *value* rather than a return
*type* -- `--> Nil`, `--> True`, `--> False`, `--> 42` -- was always declined by
the positional-light call path. Its eligibility gate admitted a return spec only
when `is_fast_type_name` recognised it as a type, and `Nil`/`True`/`False` are
not type names it knows. Every call therefore re-resolved the routine by name
and deep-copied the env: on the #9074 micro-benchmark (20,000 calls of
`sub callee(str $t, int $p is rw --> Nil) { }`) that was 20,001
`function-full-resolve`s and 20,002 `env_deep_copies`, against 2 and 1 for the
same signature with no return spec.

A definite spec is not a type check at all: the body's value is sunk and thrown
away and the constant is returned. `CompiledFunction` now carries that constant
precomputed (`return_definite_const`, set only for the literal spellings `Nil`,
`True`, `False` and an integer, which the general path's
`is_definite_return_spec` classifies as definite before any type-registry
lookup), the eligibility gate admits it, and the light path applies the same
rules as `finalize_return_with_spec`'s definite arm after the body: a `fail`
bypasses it, an explicit `return` must carry no value, and a natural completion
sinks the body's value (so a trailing lazy `.map` still runs) before the
constant is returned. The benchmark now shows 2 full resolves and 1 deep copy,
the same as the untyped signature.

JSON::Fast declares `--> Nil` on `nom-ws`/`nom-comment` and `--> True`/`--> False`
on `parse-true`/`parse-false`, which is what surfaced this (#8673).

Pin: `t/vm/frames/light-call-definite-return.t`.
