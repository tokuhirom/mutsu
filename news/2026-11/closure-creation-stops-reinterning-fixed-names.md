# Closure creation stops re-interning three fixed names

Every closure created inside a method looked up the declared-method-capture marker
through `Symbol::intern("...")`, every closure capture re-interned its env's `?FILE`
string, and `value_type_name` of a `Sub` interned `__mutsu_callable_type` on each call.
Each is a ~400 Ir hash probe on a per-closure path. The marker and the callable-type
key are now well-known symbols, and the `?FILE` symbol is memoized per thread (a string
compare against the last file seen).

Profiling build, warm, slope 100 -> 400 iterations: a method that builds a closure
(`method m { my $y = 1; my $b = { $y }; $b }`) 41.0k -> 40.2k Ir per call (-2%);
`$l.protect: { $x++ }` 23.1k -> 22.9k (-1%). Small by itself: closure creation is
still ~9k Ir (`capture_closure_env` 5.4k of it, spread over allocation and hashing), the
dominant cost of #15530.
