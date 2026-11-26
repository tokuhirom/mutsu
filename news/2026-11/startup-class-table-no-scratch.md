# Built-in classes go straight into the registry, saving ~118 start-up page faults

`build_builtin_registry` is the step of `Interpreter::new` that builds the ~450 built-in
classes. It used to collect them in a scratch `FxHashMap<String, ClassDef>` sized for 512
entries. A `ClassDef` is ~530 bytes, so that table was ~570 KB of buckets, and every process
touched it once at start-up. Each class was then moved into an `Arc` for the registry's sharded
table, and the scratch table was freed. Each newly touched page of heap costs one page fault.

`BuiltinClassTable` (`src/runtime/runtime_init_class_table.rs`) now inserts each class directly
in the registry's own form, `ShardedMap<String, CowTable<ClassDef>>`. The deferred declaration
of built-in exception attributes (#18654, #18956) used to be a separate pass over the table. It
now happens as each class is inserted.

Measured on a CI-style release build, `bench-startup` with `MUTSU_JIT=off`, 2×40 interleaved
runs against `main`:

- Minor faults went from ~1,225 to ~1,107.
- Median wall time fell 5-8%.
- Instructions went from 7.91M to 7.76M.

This is the second slice of #19197. The first slice was the start-up text layout.
