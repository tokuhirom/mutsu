# A cross-thread element store allocates two fewer `String`s

`@a[$i] = $v` on an array shared between threads goes through `shared_array_elem_set` (and the hash
twin): a read-modify-write of the name-keyed `__mutsu_atomic_arr::` entry. Every store built the entry's
key `String` again for `insert` and another for the `shared_vars_dirty` set, although both already held
the name from the second store on (#13936: `bench-threads` allocations 662k -> 822k since the store started
keeping the element record).

The entry is now replaced in place (`get_mut`) when it exists, and `ThreadSharing::note_dirty` looks the
name up before building the owned string.

callgrind (profiling build), two threads storing into a `:=`-bound shared array: 10.0 -> 8.0 allocations
per store (the repro's `__rdl_alloc` count differenced between 400 and 1,200 stores), 23.4k -> 22.9k
instructions per store. The element store still clones the whole `ArrayData` (and its `initialized` set) per
store; replacing that is the name-keyed lane's retirement (#8069 section 4.2).
