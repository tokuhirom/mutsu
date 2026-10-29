# Fix a Gc strong-count underflow abort for role mixins held by DESTROY cycles

A cyclic object with a `DESTROY` submethod that held a role mixin (`5 but R`)
aborted the process at exit with `Gc::drop strong-count underflow` (found
through Red's transaction guard, #15441). The collector's `DESTROY` snapshot
keeps the mixin's `MixinOverrides` node alive past the reclaim, but the node's
role-cell handle had been counted down by trial deletion and its target freed
with the garbage, so the later drop underflowed. `MixinOverrides::drop_gc_edges`
now swaps the role cell for a fresh empty one instead of clearing it in place.
