# A type minted on a worker thread reaches its sibling threads

A type created with `Metamodel::ClassHOW.new_type` on one `start` worker, after its sibling
workers had already started, was unknown to those siblings. This held even when the program
synchronized the mint with a `Lock` and shared the type through an object: calling `.new` on the
cached type died with `Could not find symbol '&ResultSeq' in 'GLOBAL::Shared'`, or with
`No such method 'new'` for an unqualified name. Every thread runs on its own copy-on-write
snapshot of the declaration registry, and that snapshot is taken at spawn.

ADR-17697 is now accepted and implemented. Once `compose` finishes on a minted type, and a thread
exists, the minting thread publishes the type's registry entries to a process-wide table. The
bundle also carries the entries of the parents and roles the type was built from. A thread that
meets the name and does not have it copies those entries into its own registry. This happens on
method resolution, `.new`, type matching and smartmatch, and adoption runs no user code. The copy
destructures the registry exhaustively, so a registry field added later cannot be silently left
out.

Red's t/104 "Concurrent first access publishes one class per model" subtest now passes, as it
does on rakudo (#17697).
