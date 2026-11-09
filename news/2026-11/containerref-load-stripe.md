# ContainerRef reads take the cell stripe (use-after-free under threads)

Many VM and runtime paths cloned a shared `ContainerRef` cell's value under the cell
mutex alone (`cell.lock().unwrap_or_else(..).clone()`). A writer holding the ADR-0068
stripe could drop the old node at that moment, so the reader took a refcount on freed
memory: the SIGSEGV in `exec_index_autovivify_lazy_op_raw_list_elem` seen on the Monad
concurrent-types test (#17149). All such sites now use `ContainerCell::load`, which
takes the stripe as well (the fix #13016 made for one site).
