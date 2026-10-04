# The `nqp::` serialization-context ops

mutsu now implements twelve of the serialization-context `nqp::` ops (#11504):
`createsc`, `scgethandle`, `scsetdesc`, `scgetdesc`, `scsetobj`, `scsetcode`,
`scobjcount`, `scgetobjidx`, `setobjsc`, `getobjsc`, `pushcompsc` and
`popcompsc`.

A serialization context is a run-time structure, so these ops build and query
it the way MoarVM does, every behaviour measured against rakudo:

- an SC is identified by its handle, so `createsc` with a known handle answers
  the same SC, and the registry is shared by every thread;
- `scsetobj` fills a root slot (growing the list with nulls) but does not make
  the SC the object's owner; `setobjsc` / `getobjsc` record and read the owner;
- `scgetobjidx` answers an owned object's last stored index, and otherwise
  the first root slot holding the object;
- the compiling-SC stack is per thread.

`serialize` and `deserialize` are recorded under "Not applicable" in
`docs/nqp-op-coverage.md`: they read and write MoarVM's binary precompilation
format, and mutsu does not precompile to a serialized object graph. No
ecosystem distribution reaches either at run time. The coverage table now
stands at 507 of 575 ops.
