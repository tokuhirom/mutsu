# A typed or defaulted named parameter, or a return type, no longer leaves the light call path

A routine that declared a named parameter with a type (`Bool :$flag`), a constant default
(`:$n = 42`), or a fast return type (`--> Int`) was kept off the named light call and ran the
whole general binder: `sub a3(Int $x, Bool :$flag = True --> Int)` cost 38.5k Ir per call
against 3.8k for the positional `sub h0(Int $x --> Int)`.

The light named bind now uses the same `NamedFast` plan the compiled-method fast path has
for exactly that shape (fast type name, constant default already checked against it): a
supplied value is type-checked by name, an omitted one binds the default, and the return
spec is finished by `light_finish_return`, which is the block the positional-light path ran
inline, now shared. Anything else (`where`, non-constant defaults, native types, sub-signatures)
keeps the general binder.

On the `profiling` build (callgrind slope): `a3(1, :flag($t))` 38.5k -> 6.1k Ir,
`a4(1, :flag($t))` (no return type) 33.4k -> 6.1k, an `Int :$n` named argument 30.7k -> 4.8k.
