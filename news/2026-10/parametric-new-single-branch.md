# Parametric Array/Hash `.new` has one implementation

`dispatch_new_unallocated` kept a second, older copy of the `Array[T].new` / `Hash[K,V].new`
construction below the `native_ctor` table. The table's `Array`/`Hash` entries already handle type
arguments (`try_native_array_construct` / `try_native_hash_construct`, which also the VM uses), so
the copy was reachable only when the table was bypassed and had drifted from it. It is removed,
along with the now-unused `make_shaped_array`. ADR-11276 §9.51; part of #12423.
