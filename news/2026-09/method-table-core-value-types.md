# `.^method_table` of a core value type is no longer empty

`Int.^method_table` was an empty hash, while `Int.^methods(:local)` listed 32
methods. `class_method_table` built the table only from a registry `ClassDef`.
Among the core classes, only those registered in `runtime_init.rs` (Proc::Async,
Lock, IO::Path, ...) have one. Int, Str, Num, Array, Hash and the other value
types fell through to an empty table. Anything that walks the table saw
nothing, for example Test::Mock's `mocked(Int, ...)` or a generic delegation
builder (#9388).

With no `ClassDef`, the table is now built from the built-in method catalog,
`registry().builtin_method_names`. That is the same source
`.^methods(:local)` reads through `collect_builtin_type_methods`, so the two
introspection routes cannot disagree. Entries are native `Method` objects
and can be called: `Int.^method_table<abs>(-3)` is 3.

Types the catalog does not model yet are still empty on both routes, for
example `Date` and `Mu`. rakudo lists 44 and 65 methods there.

The regression test is `t/oo/method/method-table-core-types.t`.
