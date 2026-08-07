# ADR-0019 D2d: resolve_user_method_or_accessor becomes a MethodEntry table probe

`resolve_user_method_or_accessor` decides, per MRO level, whether an explicit
user method or an auto-generated attribute accessor answers a given method
name — the core of Raku's "explicit method beats accessor beats role method"
priority rule used across method dispatch. It previously read
`ClassDef::methods`/`ClassDef::attributes` directly: a `HashMap` lookup
followed by an inline linear filter over the returned `Vec<MethodDef>`
(reimplementing `is_private`/`is_my`/`role_origin` filtering already done once
by `sync_user_method_entries` when it populates the canonical `MethodEntry`
table), plus a linear scan of the attribute vector for the public-accessor
check.

Both scans are now table probes against `Registry::method_entries`, the same
canonical `(owner, name)` table `has_public_accessor` was migrated onto in the
first D2d slice. A new `Registry::user_method_local_role_presence` returns the
`(has_local_method, has_role_method)` pair directly without cloning the
candidate list — `resolve_user_method_or_accessor` sits on the per-call method
dispatch path, so avoiding the `Vec<MethodDef>` clone that the existing
`user_method_overloads` helper does matters here. The public-accessor check
reuses `accessor_is_public` as-is.

Only the `registry.classes.get(cn)` branch of the function moved. The sibling
branch — a punned role used directly as a parent class — still reads
`RoleDef` fields directly, since general roles are not guaranteed to have a
synced `method_entries` row the way a class always does (only punned-role
instantiation syncs one today), and `native_methods.contains(...)` (a
separate `HashSet`) is untouched as out of scope for this table.

Verified with the full `t/` suite and every `S12-attributes`/`S14-roles`
roast-whitelisted file (36 files, 938 tests), all green with no output
changes — this is a pure mechanism unification, not a behavior change.

Remaining in D2d: the `.^methods`/`.^can`/`.^attributes` synthesis sites
(`methods_classhow_method_obj.rs`, `methods_classhow_attribute.rs`), which
carry meaningfully more logic (full `Attribute` meta-object construction) than
a boolean presence probe.
