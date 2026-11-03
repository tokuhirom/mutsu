//! Reflection combines model constructors with the registered declaring rows.

// Cost: O(c + log p + f log f), c = model classes, p = row owners, f = local methods.
pub(crate) fn local_method_names(class_name: &str) -> Option<Vec<&'static str>> {
    let mut names = crate::rakuast::constructor_method_names(class_name)?;
    names.extend(super::owner_row_names(class_name));
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
    names.sort_unstable();
    names.dedup();
    Some(names)
}
