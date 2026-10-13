# Cell stores and clone-reads take the container stripe

A routine called with a shared array (`rd(@x)`) while other threads pushed to or spliced the
same array could crash the main thread with `Gc::drop strong-count underflow`. When the call
returned, the parameter's value was written back into the caller's container cell under the
cell's own mutex. The worker's push held the container's ADR-0068 stripe instead, so the two
never excluded each other, and both released the replaced node.

Every whole-value store into a container cell now goes through `ContainerCell::store`, and
every clone out of one through `ContainerCell::load`. Both take the stripe as well as the
mutex. Writers through a cell also re-check, under the stripes, which node the cell holds.
A new harness, `t/concurrency/concurrent-param-array-reader-families.t`, runs seven reader
families against four kinds of concurrent mutation. It found five more unguarded readers:
stringification, the `@a[$i]` fast path, and three spots in `for`-loop iteration. All of
them now read under the stripe or from a snapshot (ADR-0068 §18, #13016).
