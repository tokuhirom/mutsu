# Chained container descent reads each hop under its guard

An indexed store through a chain of container references (an alias bound to a
scalar that holds an Array) walks the chain hop by hop to find the slot it
writes. Only the first hop was read through the caller's pointer; every later
hop used to be read through a pointer escaped from the previous cell's lock,
after that lock was released. ThreadSanitizer reported it as `ContainerCell::store`
and `celled_array_extend` writes racing `descend_container_ref_tracked` reads in
`t/concurrency/concurrent-param-array-reader-families.t` (#18667).

`descend_container_ref_tracked` now takes each cell's guard to read the next hop
and escapes only the final slot, so a re-assignment of an intermediate scalar
can no longer free the value a descent is still reading.
