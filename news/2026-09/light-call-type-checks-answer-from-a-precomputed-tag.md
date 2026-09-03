# The light call path stopped re-reading type-constraint spellings per call

Only eight type names get a routine onto the light / positional-light call
paths at all (`is_fast_type_name`: `Int`, `Str`, `Num`, `Bool`, `Rat`, `Any`,
`Mu`, `Cool`) -- yet the per-call check took the constraint as a `&str` and
matched it: a length dispatch plus a byte compare against five three-letter
candidates, on top of inspecting the value up to three times (once for the mixin
probe, once for the type-object probe, once for the type match itself). The
declared *return* type asked the same question again, through four separate
value inspections. `perf` put `fast_type_check` at 4.2% of `bench-fib`.

The classification is fixed the moment a signature is known, so it is now made
once. `CompiledFunction` gained `param_fast_types` (a `FastParamCheck` per
parameter) and `return_fast_type`, both filled by
`precompute_param_name_syms` -- the routine every construction site already
calls once its signature is final, which is precisely why the tags live there
rather than in a sibling method a new site could forget. A `FastParamCheck` is
either `Unconstrained` or `Fast { kind, name_sym }`, where `kind` is the new
`FastParamType` discriminant (`Wild` covering `Any`/`Mu`/`Cool`) and `name_sym`
is the constraint name pre-interned, so a bare type object argument
(`sub f(Int $a); f(Int)`) compares two `Symbol`s instead of resolving one to a
`&str`. `FastParamType::of` is now the single source of truth that
`is_fast_type_name` delegates to.

The per-call checks (`fast_type_check_tagged`,
`light_return_type_check_tagged`) dispatch on the *value*'s shape exactly once
and answer from the tag. The by-name forms stay for the paths without a
precomputed tag -- a hand-built chunk whose `param_fast_types` is empty falls
back to them, parameter by parameter. The return check keeps its deliberate
asymmetry with the parameter check: a returned type object must match the
declared return type by name even when that type is a wildcard.

Measured on a release build with a temporary same-binary env switch, pinned to
one core: `bench-fib` retired instructions **-3.12%**, cycles -3.4%. The
benchmarks with untyped signatures (`bench-tak`, `method-call`, `bench-class`,
`bench-ctor`, `poly-call`, `bench-mandelbrot`) are all within +/-0.1%, as
expected -- an unconstrained parameter did no work before and does none now.

`t/light-call-type-check-tags.t` pins the tagged check against the by-name one
across every shape it distinguishes: the five concrete types, the three
wildcards, bare type objects, allomorphs (`<42>` satisfying both `Int` and
`Str`), `Nil` passing any return type, the return-type asymmetry, the failing
cases, and a mixed signature where each parameter carries its own tag. Verified
against `raku`.
