# Per-call method-table scans replaced by O(1) lookups

`io_path_subclass_cool_receiver` (run for every Cool-only method call on a non-`IO::Path`
instance such as `Match`) asked `builtin_type_method_names("IO::Path").contains(..)`, which
filtered the whole native-method row table and allocated a `Vec` on each call. It now uses
`builtin_type_method_listed`, a single hash probe, and `.can`'s native check does the same.
`introspectable_names_for_owner` reads a once-built per-owner row index, which also shortens the
18 enumerations done while seeding the registry at startup (#13807).
