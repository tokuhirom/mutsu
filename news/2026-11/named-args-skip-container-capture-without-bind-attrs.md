# Named arguments skip the container capture until a `is built(:bind)` attribute exists

#16002 made every syntactic named argument (`x => $var`) retain the caller's container so an
`is built(:bind)` attribute can bind it. That capture (a shared cell, its descriptor name, a
constraint registration and the pair rebuild in `read_named_args`) cost ~9 allocations per
variable argument on every named-argument call, whether or not any class could use it. A
process-wide monotonic flag, set where a class or role registers an attribute declared
`is built(:bind)`, now gates it: until one exists, `WrapVarRef` before a `MakeNamedArg` is skipped
and the op binds the value. `method-call` (callgrind, profiling build, same box): 563.7M -> 521.7M
instructions and 288.5k -> 198.5k allocations (pre-#16002: 510.0M / 178.2k); the rest is #16048's
`pending_call_arg_sources` clone.
