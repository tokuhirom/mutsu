# The signature half of the positional-light call check is decided once per routine

`is_positional_light_call_eligible` ran on every call that took the light
path from a call site without a name-keyed cache (a routine imported by a
`use`, a code-variable call): a dozen flag tests and a walk of every parameter
(`named`, `slurpy`, `where`, traits, type constraint, twigil ...) of the
callee, each time. Everything in it that reads only the routine (flags,
return type, parameter shapes) now lives in `light_signature_static_ok` and
is memoized in `CompiledFunction::light_signature_cache`; what looks at the
call's own arguments (a `Proxy`, arity against defaults, `is rw` and
aggregate arguments) is still checked per call. A `where`-verified multi
winner asks the uncached form, since the `where` clause is what differs.
Measured on a loop calling an imported `sub inc($x)`: 5,845 -> 5,688 Ir per
call (profiling build, callgrind second run); a call of a routine declared in
the program is unchanged. Part of #14371.
