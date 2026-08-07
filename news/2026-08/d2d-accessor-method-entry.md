# ADR-0019 D2d (partial): `has_public_accessor` probes the canonical method table

`has_public_accessor` decided whether `$obj.name` should resolve to an
auto-generated `has $.name` accessor by walking the class's MRO and, at each
level, linearly scanning that class's `ClassAttributeDef` vector for a
matching name — a scan on the per-call method-dispatch path (it backs the VM's
`try_fast_accessor_read` fast path and the interpreter's instance-method
dispatch fallback, among other callers). Meanwhile `MethodEntry`, the
canonical `(owner, method)` table Phase B built for user/native method
candidates, had no way to represent "this class also has an accessor here" —
so accessor presence and method presence lived in two unrelated data
structures despite both answering the same kind of query.

`MethodEntry` gains an `accessor: Option<bool>` field (`None` = this class
does not declare an attribute of that name; `Some(is_public)` when it does).
`Registry::sync_user_method_entries` — already the single place that
publishes a class's `ClassDef::methods` into the table on every registration
mutation — now does the same for `ClassDef::attributes` in the same pass,
under the same generation bump. `has_public_accessor` is now a per-MRO-level
table probe (`Registry::accessor_is_public`) instead of a vector scan.

This is a first slice, not the whole of D2d. `resolve_user_method_or_accessor`
(the method-vs-accessor-vs-role-method race used by the mutation/write path
and the compiled-dispatch re-check) and the `.^methods`/`.^can`/`.^attributes`
introspection synthesis sites (`methods_classhow_method_obj.rs`,
`methods_classhow_attribute.rs`) still do their own independent MRO×attribute
scans. They carry meaningfully more logic than a boolean presence check
(role-attribute composition, method-vs-accessor shadow ordering per level,
full `Attribute` meta-object construction for `.^attributes`), so migrating
them was left for a follow-up slice rather than risked in the same PR as the
`MethodEntry` shape change.

Verified with `cargo test --lib` (672 tests), the local attribute/accessor/
private-method `prove` surface (56 files, 417 tests), and the full `t/` suite
via `make test` (PASS) — all passing unchanged.
