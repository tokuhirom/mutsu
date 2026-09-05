use super::*;

fn dummy_method_def() -> MethodDef {
    MethodDef {
        lexical_package: "GLOBAL".to_string(),
        params: Vec::new(),
        param_defs: Vec::new(),
        body: std::sync::Arc::new(Vec::new()),
        is_rw: false,
        is_raw: false,
        is_private: false,
        is_multi: false,
        is_my: false,
        role_origin: None,
        original_role: None,
        return_type: None,
        compiled_code: None,
        compiled_fns: None,
        delegation: None,
        is_default: false,
        deprecated_message: None,
        is_submethod: false,
        captured_env: None,
        source_file: None,
        role_param_bindings: None,
    }
}

#[test]
fn set_user_methods_adds_replaces_and_drops_empty_rows() {
    let mut registry = Registry::default();
    let owner = Symbol::intern("Foo");
    let name = Symbol::intern("bar");
    registry.set_user_methods(owner, name, vec![dummy_method_def()]);
    assert_eq!(registry.owner_method_names("Foo"), vec![name]);

    registry.set_user_methods(owner, name, vec![dummy_method_def(), dummy_method_def()]);
    assert_eq!(
        registry.method_entries[&MethodEntryKey { owner, name }]
            .user_candidates
            .len(),
        2
    );

    registry.set_user_methods(owner, name, Vec::new());
    assert!(registry.owner_method_names("Foo").is_empty());
    assert!(
        !registry
            .method_entries
            .contains_key(&MethodEntryKey { owner, name })
    );
}

#[test]
fn push_user_method_appends_a_multi_candidate() {
    let mut registry = Registry::default();
    let owner = Symbol::intern("Foo");
    let name = Symbol::intern("bar");
    registry.push_user_method(owner, name, dummy_method_def());
    registry.push_user_method(owner, name, dummy_method_def());
    assert_eq!(
        registry.method_entries[&MethodEntryKey { owner, name }]
            .user_candidates
            .len(),
        2
    );
    assert_eq!(registry.owner_method_names("Foo"), vec![name]);
}

#[test]
fn retain_user_methods_drops_the_row_once_the_last_candidate_is_filtered_out() {
    let mut registry = Registry::default();
    let owner = Symbol::intern("Foo");
    let name = Symbol::intern("bar");
    registry.set_user_methods(owner, name, vec![dummy_method_def()]);
    registry.retain_user_methods(owner, name, |_| false);
    assert!(registry.owner_method_names("Foo").is_empty());
    assert!(
        !registry
            .method_entries
            .contains_key(&MethodEntryKey { owner, name })
    );
}

#[test]
fn remove_user_methods_is_set_user_methods_with_an_empty_vec() {
    let mut registry = Registry::default();
    let owner = Symbol::intern("Foo");
    let name = Symbol::intern("bar");
    registry.set_user_methods(owner, name, vec![dummy_method_def()]);
    registry.remove_user_methods(owner, name);
    assert!(registry.owner_method_names("Foo").is_empty());
}

#[test]
fn clear_user_methods_for_owner_wipes_user_rows_but_spares_builtin_columns() {
    let mut registry = Registry::default();
    registry.seed_builtin_method_entries();
    let owner = Symbol::intern("Str");
    let name = Symbol::intern("chars");
    registry.set_user_methods(owner, name, vec![dummy_method_def()]);
    registry.clear_user_methods_for_owner(owner);
    assert!(registry.owner_method_names("Str").is_empty());
    let entry = &registry.method_entries[&MethodEntryKey { owner, name }];
    assert!(entry.builtin.is_some());
    assert!(entry.user_candidates.is_empty());
}

#[test]
fn rename_method_owner_moves_every_row() {
    let mut registry = Registry::default();
    let old = Symbol::intern("OldName");
    let new = Symbol::intern("NewName");
    let name = Symbol::intern("greet");
    registry.set_user_methods(old, name, vec![dummy_method_def()]);
    registry.rename_method_owner(old, new);
    assert!(registry.owner_method_names("OldName").is_empty());
    assert_eq!(registry.owner_method_names("NewName"), vec![name]);
}

#[test]
fn map_user_methods_in_place_mutates_every_candidate_without_touching_the_index() {
    let mut registry = Registry::default();
    let owner = Symbol::intern("Foo");
    let name = Symbol::intern("bar");
    registry.set_user_methods(owner, name, vec![dummy_method_def()]);
    registry.map_user_methods_in_place(owner, |def| def.is_rw = true);
    assert!(registry.method_entries[&MethodEntryKey { owner, name }].user_candidates[0].is_rw);
    assert_eq!(registry.owner_method_names("Foo"), vec![name]);
}

#[test]
fn user_method_rows_round_trip_through_restore() {
    let mut registry = Registry::default();
    let owner = Symbol::intern("Foo");
    let a = Symbol::intern("a");
    let b = Symbol::intern("b");
    registry.set_user_methods(owner, a, vec![dummy_method_def()]);
    registry.set_user_methods(owner, b, vec![dummy_method_def(), dummy_method_def()]);
    let rows = registry.user_method_rows_for_owner(owner);
    assert_eq!(rows.len(), 2);

    registry.clear_user_methods_for_owner(owner);
    assert!(registry.owner_method_names("Foo").is_empty());

    registry.restore_user_method_rows(owner, rows);
    let mut names = registry.owner_method_names("Foo");
    names.sort_by_key(Symbol::resolve);
    assert_eq!(names, vec![a, b]);
    assert_eq!(
        registry.method_entries[&MethodEntryKey { owner, name: b }]
            .user_candidates
            .len(),
        2
    );
}

#[test]
fn sync_accessor_entries_derives_from_attributes_and_clears_stale_rows() {
    let mut registry = Registry::default();
    let owner = Symbol::intern("Point");
    let mut class = ClassDef::default();
    class.attributes.push(ClassAttributeDef {
        name: "x".to_string(),
        is_public: true,
        default: None,
        is_rw: false,
        is_required: None,
        sigil: '$',
        where_constraint: None,
        declared_shape: None,
    });
    registry.classes.insert("Point".to_string(), class);
    registry.sync_accessor_entries(owner);
    assert_eq!(
        registry.method_entries[&MethodEntryKey {
            owner,
            name: Symbol::intern("x"),
        }]
            .accessor,
        Some(true)
    );

    // Redeclare with the attribute gone; the stale accessor row must be
    // cleared even though it isn't covered by `owner_method_names` (that
    // index tracks the user-method column only).
    registry
        .classes
        .insert("Point".to_string(), ClassDef::default());
    registry.sync_accessor_entries(owner);
    assert!(!registry.method_entries.contains_key(&MethodEntryKey {
        owner,
        name: Symbol::intern("x"),
    }));
}
