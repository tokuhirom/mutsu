# Storing into a captured hash no longer seeds and replays the whole store

A `my sub` or closure that stores into a hash of its enclosing routine
(`%memo{$needle} = [$from, $at]`) sees the variable only through the shared
cell the capture boxed it into. The plain-hash store lanes look for a bare
hash under the name, so they declined the cell. The store then took the
general path: the cell's hash was seeded into env, the whole store cascade ran
on it, and the result was written back through the cell. That cost about 27k
instructions per store, against about 1.8k for the same store into a local
hash (#18055).

The cell is the variable's container, and its hash is the store's target. When
that hash is plain (no element type, no `is default`, not a `Map` or object
hash, not readonly) and the cell is its only holder, the store is now one insert
into it, made in place under the cell's lock. The cell is resolved exactly as
the general path resolves it. A bound or `Proxy` element still goes to the
general store, and so do a hash shared with another holder and a
cross-thread-shared variable.

MCP::Server::Tool::Web's tokenizer runs this store on every memo miss of its
`find` helper.
