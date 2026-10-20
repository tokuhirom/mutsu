# A lazy array's deleted indices belong to the array

`@a[2]:delete` on a lazy array (`my @a = 1, 2, 4 ... Inf`) keeps the array
lazy, and `@a[2]:exists` then reports `False`. mutsu used to record the hole
in an env key derived from the variable's name, `__mutsu_deleted_index::@a`.
A `my` redeclaration had to clear that key on every declaration, so that a
fresh array of the same name would not inherit the hole.

The record now lives on the `LazyList` itself (`deleted_indices`). The
rebuild after the delete (`restore_lazy_array_slot`) takes it from the reified
prefix's holes, which a reified array already keeps in its embedded
`initialized` set. Every holder of the array sees the same holes, a fresh
array starts without any, and the namespace, its `BindingDesc` field and the
per-declaration clear are gone (ADR-0097 slice 4, #8087).
