# Slot-frame attribute reads and warm accessor sites read the layout slot directly

Two reads on the method-call path did general lookups for an answer they had already cached.

- **`$!x` in a slot-frame method body.** The read went through `read_self_attr_cell_slot`. That clones `self` out of its slot, asks whether the innermost method class is a role, and unwraps mixins and containers before it consults the site's cached `(layout, slot)`. A slot-frame method is never a role's and its receiver is an ordinary instance, so the read now takes the cached slot straight off the receiver in slot 0. Any other layout, or an instance with undeclared attributes, falls back to the general read.
- **`$o.x` at a warm accessor site** (`vm_method_site_ic`). Each hit asked the accessor lane again: a method-cache generation check and a hash probe keyed by `(layout, method)`. The site now keeps the lane's `(layout, entry)` after its first hit and reads that slot. The lane's own checks still run on every read: the layout id, undeclared attributes, and an aggregate's element type and `is rw`.

Two smaller per-call costs went with them:

- a planned call's arguments are now moved into the slot frame's locals instead of cloned and dropped;
- the site cache no longer clones its argument-key vector on every hit, nor runs a Seq-reification probe that a plan's scalar-only arguments can never need.

Callgrind slope per loop iteration, profiling build:

| input | before | after |
| --- | ---: | ---: |
| `$sum += $p1.distance-to($p2)` (method-call's method) | 14.9k Ir | 13.4k Ir (-10%) |
