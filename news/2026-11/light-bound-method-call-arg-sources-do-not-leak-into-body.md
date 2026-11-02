# A light-bound method no longer leaks its call's arg-source names into its body

The method light bind (`call_compiled_method_in_unit`) only peeked the call
site's `pending_call_arg_sources` and never consumed them, so they stayed
installed while the method body ran. The first bind the body made with the same
arity took them for its own: `CArray[uint8]` calls `^parameterize($how, $type)`,
and the `["off", Nil]` of `self.rs($off, 1)` matched its two arguments, so the
caller's `$off` was bound as a shared cell in the env that the caller's slot never
adopted. That tripped the ADR-0097 section 15 env/slot invariant (a debug-build
panic; in release a stale slot). The light bind now takes the names once it is
done with them (#16037). DAWG `t/03-binary.rakutest` passes 12/12.
