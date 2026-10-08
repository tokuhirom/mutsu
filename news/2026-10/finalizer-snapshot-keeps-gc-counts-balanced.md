# A DESTROY snapshot taken at cycle reclaim no longer underflows GC counts

The nightly `gc-stress-tap` run
([#12306](https://github.com/tokuhirom/mutsu/issues/12306)) failed on two Cro
tests with `Gc::drop strong-count underflow` on a pool thread, reproducible with
`MUTSU_GC=on MUTSU_GC_EVERY_CANDIDATE=1024`. It was a deterministic collector
bug, not a flake.

When the collector reclaims a garbage `Instance` whose class has a `DESTROY`,
the finalizer snapshots the attribute map. Trial deletion had counted every edge
inside a uniquely-owned `Arc` wrapper (a `Scalar` or `Capture` box) as the
instance's own and `reclaim` drops those edges inertly, but the snapshot clone
shares the wrapper instead of its `Gc` handles. The wrapper outlived the
reclaim holding a handle whose count had already been deducted, so a node still
held elsewhere was driven below zero when the snapshot was dropped. `reclaim`
now compares the node's children before and after the finalizer and gives each
vanished (now snapshot-owned) edge its count back (`Trace::finalize_clones_edges`).

Pin: `finalizer_snapshot_of_an_inline_wrapper_edge_keeps_counts_balanced` in
`src/gc/collect.rs`; the two Cro files pass under the stress environment.
