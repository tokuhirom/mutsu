# ContainerRef reads take the cell stripe (use-after-free under threads)

Many VM and runtime paths cloned a shared `ContainerRef` cell's value under the cell
mutex alone (`cell.lock().unwrap_or_else(..).clone()`). A writer holding the ADR-0068
stripe could drop the old node at that moment, so the reader took a refcount on freed
memory: the SIGSEGV in `exec_index_autovivify_lazy_op_raw_list_elem` seen on the Monad
concurrent-types test (#17149). All such sites now use `ContainerCell::load`, which
takes the stripe as well (the fix #13016 made for one site).

The remaining crash had a second cause: assigning a Map to a variable that held a Map
(`$cache = Map.new(..)`) went through `cell_store_preserving_container_identity`, whose
in-place hash reassign rewrote the *published* Map node, freeing its table under a
lock-free reader on another thread. A Map is immutable, so the cell now just takes the
new value; the bind of a Map element also binds the value instead of promoting the slot
in place.
