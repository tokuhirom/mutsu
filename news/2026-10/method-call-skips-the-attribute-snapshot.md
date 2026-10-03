# A method call no longer clones the receiver's attributes

Every compiled method call took a whole-map snapshot of the receiver's
attributes (`InstanceAttrs::to_map`) before entering the binder, and dropped
it again on return — about 500 instructions per call on a one-attribute class,
more on a class with many. Only the full binder reads the attributes as a map;
the fast path, which nearly every call takes, reads them straight from the live
attribute cell. Callers now hand the binder a `CallAttrs` (the live cell, an
existing map, or nothing) and the snapshot is taken only when the full binder
actually runs.

Two smaller leftovers of the same kind went with it: the method-exit `:=`
attribute reconcile looked up the attribute cell before learning that the frame
holds no `ContainerRef` (the common case, where it needs no cell at all), and
the fast-path prologue walked the receiver for the role and instance cells
twice.

callgrind, instructions per call with the empty loop subtracted (#8880's
repros, warm second run):

| call | before | after |
| --- | ---: | ---: |
| `$o.m()` | 13,890 | 12,872 (-7.3%) |
| `$o.m($x)` | 19,451 | 18,438 (-5.2%) |
