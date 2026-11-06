# A spinning reader of a captured `atomicint` sees another thread's store

`my atomicint $a = 0; start { $a ⚛= 1 }; while $a == 0 { sleep 0.01 }` never ended: the atomic branch
of `GetLocal` overwrote the variable's slot with the plain value it had just fetched, which dropped the
shared cell the closure's capture had boxed. The next read boxed a fresh cell the thread's store never
reached. The slot now keeps a cell it holds (`src/vm/vm_var_assign_local_get.rs`). This is the
`atomicint` documentation's "spin until another thread sets it" example. Test:
`t/concurrency/thread-lock/atomicint-start-store-visible-to-spinning-reader.t`.
