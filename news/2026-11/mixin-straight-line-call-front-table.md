# A repeated straight-line role method call on a mixin skips the role walk

After `run_straight_line_method` (the previous slice) took the method frame
away, most of a `CArray` element access was the walk that finds the method:
sorting the mixin's roles, copying its role parameters into the env and back,
deriving the winner-memo key from the role definition. A front table now
remembers, per composition node, method, receiver definedness and argument
type keys, the straight-line method the walk ended in. A hit goes straight to
the lane. An entry is valid only while the registry has not been written
(`registry_write_generation`, which a CArray access does not move) and keeps
its composition node alive, so the node's address cannot be reused. The lane
re-checks the argument shapes itself and declines to the general path.

Callgrind slope on a profiling build (second run, N 500 -> 1500), per element:
`$c[$_] = 1.5e0` on `CArray[num64]` 40.3k -> **30.0k Ir**; fill + `.Array`
99.3k -> **77.8k Ir** (136.4k before the lane).

Refs #16215.
