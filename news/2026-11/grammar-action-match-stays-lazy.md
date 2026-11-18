# A grammar action's `$/` stays lazy through its call

A capture node reduced during `Grammar.parse(:actions(...))` reaches its action
as a lazy `Match`: its attribute map is built only when something asks for it.
Calling the action asked for it every time, through four probes that only
needed to know the value's kind:

- the fast binder's `Any` check of the `$/` parameter (`light_arg_is_any`),
  which fell through to the BOOTSTRAPATTR probe;
- the parameter's scalar-store itemization (`itemize_scalar_store_value`);
- the method gate's "does a Seq bind into an `@` parameter" scan
  (`method_binds_seq_into_array_param`), which viewed the arguments before
  looking at the signature;
- `make`'s lookup of the frame's `$/` (`deref_container`).

Each now answers a lazy `Match` from its tag (it is an `Any`, not a container,
not a BOOTSTRAPATTR, stored as it is) or asks the signature first, so a leaf
action no longer materializes its `$/`. A one-action parse went from 43.9k to
37.4k instructions per action (callgrind slope, profiling build).
