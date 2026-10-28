# Instance type checks and itemization probe attributes by pre-interned Symbol

An `Instance` argument paid several string-keyed attribute probes on its way through a method call.
`attributes.contains_key("__mutsu_array_storage")` and its Hash / Baggy siblings (102 call sites,
asked of every instance a scalar store itemizes) and `is_bootstrapattr` (the first step of every
`type_matches_value`) each resolved their literal through `Symbol::lookup`, a thread-local hash of
the whole string, ~120 Ir per probe. The four literals are now well-known symbols (`symbol::wk`), so
each probe is a plain symbol-keyed lookup.

Two per-method-call costs that scaled with program size rather than the call:

- `has_class_scoped_subs` built two `format!("::{name}")` strings per class that declares a sub, on
  every method call; it now compares suffixes in place, with no allocation.
- `package_has_deferred_use_imports` hashed the class name to look it up even when no package had
  received a deferred import; it returns early on an empty set.

Measured with callgrind on a 40,000-iteration loop, profiling build, the previous branch binary vs
this one: `method t4(A $o)` called with an instance argument 835.3M -> 804.4M Ir (-3.7%, ~770 Ir per
call); `method t1(Int $a)` 659.4M -> 651.5M (-1.2%). The `trie-create` goal in #15235 is not met by
this slice.
