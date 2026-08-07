# ADR-0019 D2d closed: `.^methods`/`.^can`/`.^attributes` synthesis intentionally left unmigrated

D2d ("publish generated accessors through the canonical table") is now marked
done in the ADR. Its final piece — whether to also migrate the
`.^methods`/`.^can`/`.^attributes` meta-object synthesis
(`methods_classhow_method_obj.rs`'s `collect_class_methods`/
`class_method_table`/`collect_can_methods`, `methods_classhow_attribute.rs`'s
`collect_attribute_objects`) onto the `MethodEntry` table — was investigated
and deliberately left alone, with the reasoning recorded in the ADR rather
than silently dropped.

The two D2d slices that did land (`has_public_accessor`,
`resolve_user_method_or_accessor`) were both **single-key point lookups**
`(owner, method_name)` sitting on the per-method-dispatch-call hot path,
where `MethodEntry`'s `(owner, name)` keying is a direct win over a linear
`Vec` scan. The remaining introspection sites are structurally different:
they **enumerate every method or attribute a class declares** to build full
`Method`/`Attribute` meta-objects (params, body, signature, custom trait
state). `method_entries` has no owner-keyed enumeration index — the one
precedent for scanning it by owner (`Registry::builtin_method_names`) already
pays a full-map scan to do so, for the same structural reason.

More importantly: unlike D2b's four independently-drifted `Stmt::HasDecl`
destructuring sites (the kind of bug D2b actually fixed),
`ClassDef::methods`/`ClassDef::attributes` and
`MethodEntry.user_candidates`/`.accessor` are already a single source of
truth kept in lockstep by `sync_user_method_entries` at every mutation site.
Reading one over the other here would be a lateral move, not a mechanism
unification — it doesn't remove any duplicated logic or close any
dual-store risk, so it doesn't clear the bar CLAUDE.md sets for a
worthwhile architectural change ("gain" = moving toward a sounder
architecture, not code-shuffling for its own sake).

See `docs/adr/0019-compiled-declarations-and-unified-method-dispatch.md`'s
D2d entry for the full reasoning.
