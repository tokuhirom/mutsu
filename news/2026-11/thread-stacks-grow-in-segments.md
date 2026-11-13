# Under `ulimit -v`, worker threads start small and grow their stack as they recurse

mutsu's `await` keeps its OS thread busy, so every `start` block parked in
`await` holds a thread and that thread's native stack. When the address space
is limited (`ulimit -v`, as in the ecosystem sandbox), each of those stacks
counts against the limit in full. Before this change, the first dozen workers
took 256 MiB each and later ones stepped down to smaller fixed sizes. A
release build ran out at about 700 parked `start` blocks under a 6 GB limit.
The fixed sizes also meant that a worker created late could recurse only a
few hundred levels deep.

A worker thread under an address-space limit now starts on a small stack: 2 MiB
in a release build, 16 MiB in a debug one. When a deep call runs low on stack,
the thread continues it on a freshly mapped 64 MiB segment and returns to the
old stack afterwards. Every place that already checked for "Too deep
recursion" does this:

- the VM's call boundaries;
- the value renderers;
- the parser;
- the RakuAST walks.

So a parked worker costs only its small stack. Under a 6 GB limit a release
build now parks 2000 `start` blocks, where it used to manage about 700. Every worker can still recurse
as deep as the main thread, and a runaway recursion still ends in the same
catchable error. Without an address-space limit nothing changes: workers keep
their 256 MiB stack.

The decision is ADR-17392. It replaces the fixed stack sizes of ADR-0123 and
reverses ADR-0100's earlier rejection of growable stacks (#17392).
