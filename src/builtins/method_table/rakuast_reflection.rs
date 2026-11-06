//! Reflection combines model constructors with the registered declaring rows.

/// Existing model extensions whose declaration belongs to mutsu, rather
/// than to the Rakudo snapshot. ADR-10723 section 6.92 records hash captures.
// Cost: O(1), one fixed owner/name pair.
pub(crate) fn model_extension(owner: &str, name: &str) -> bool {
    (owner == "RakuAST::Regex::NamedCapture" && name == "hash") || role_provided(owner, name)
}

/// `properties` / `reducer-name` of the owners that get them from the
/// `RakuAST::OperatorProperties` role: rakudo declares them on the role, not
/// in the class's own method table, so `.^methods(:local)` does not list
/// them; mutsu serves them from per-class rows.
// Cost: O(1), fixed owner/name pairs.
fn role_provided(owner: &str, name: &str) -> bool {
    matches!(name, "properties" | "reducer-name")
        && matches!(
            owner,
            "RakuAST::Postfix"
                | "RakuAST::Postcircumfix::ArrayIndex"
                | "RakuAST::Postcircumfix::HashIndex"
                | "RakuAST::Postcircumfix::LiteralHashIndex"
        )
}

// Cost: O(c + log p + f log f), c = model classes, p = row owners, f = local methods.
pub(crate) fn local_method_names(class_name: &str) -> Option<Vec<&'static str>> {
    let mut names = crate::rakuast::constructor_method_names(class_name)?;
    names.extend(
        super::owner_row_names(class_name)
            .into_iter()
            .filter(|name| !role_provided(class_name, name)),
    );
    names.sort_unstable();
    names.dedup();
    Some(names)
}

/// The public methods a RakuAST model class exposes *including* the ones it
/// inherits, for `.^methods` (whose default, per
/// `Type/Metamodel/MethodContainer.rakudoc`, is "methods of the class and its
/// parents, stopping at Cool/Any/Mu" — `:local` is the narrower set
/// [`local_method_names`] returns). Walks the model MRO and drops the `Any` /
/// `Mu` tail, so the result grows automatically as the abstract RakuAST classes
/// gain model methods of their own.
pub(crate) fn inherited_method_names(class_name: &str) -> Option<Vec<&'static str>> {
    let mro = crate::rakuast::type_object_mro(class_name)?;
    let mut names = Vec::new();
    for cls in &mro {
        if cls == "Any" || cls == "Mu" {
            break;
        }
        if let Some(local) = local_method_names(cls) {
            names.extend(local);
        }
    }
    // A role the class does declares its methods too (`WHY` of
    // `Doc::DeclaratorTarget`, `has-placeholder-parameters` of
    // `PlaceholderParameterOwner`).
    if let Some(entry) = crate::rakuast::hierarchy::entry(class_name) {
        for role in &entry.roles {
            names.extend(super::owner_row_names(role));
        }
    }
    names.sort_unstable();
    names.dedup();
    Some(names)
}
