# `class_does_role` is memoized per registry write generation

The `Sequence` delegate (`try_sequence_role_delegate`) asks `class_does_role(class, "Sequence")`
before it will let a method call through, three times per method call on an instance, and each ask
rebuilt the class's whole composed-role closure (`collect_roles_for_class`) to answer no. The
answer reads the registry and the builtin catalog only, so it is now memoized per
`(class, role)` for one `registry_write_gen` (the memo every registry-derived cache here uses),
with a debug assertion that a hit equals the recomputed answer.

Callgrind slope (profiling build, second run, 500 -> 1500 reads of `$x = $c[$_ % 500]` on a
`CArray[num64]`): 100.2k -> 98.8k Ir per read (same session, same build otherwise), i.e. under the
100k close condition of #16373 (850.9k before #16445).
