//! One `Attribute` meta-object identity per attribute (#10004).
//!
//! Rakudo keeps exactly one meta-object per attribute, so two
//! `.^attributes` lookups are `===` and share a `.WHICH`. mutsu builds the
//! introspection object afresh on every lookup (its keys are derived from the
//! current class registration, so they are never stale), but gives every
//! build of the same attribute the same instance id, which is what `===` and
//! `.WHICH` compare.
//!
//! The identity is keyed by owner, sigil and name. Because the object's keys
//! are rebuilt from the registration on every lookup, sharing an identity
//! across a redeclaration under the same owner name serves nothing stale. A
//! role attribute composed into two classes differs in owner, so the two
//! classes see two attributes (as in Rakudo); an inherited attribute is
//! reported with its declaring class as owner, so a subclass and its parent
//! see the same one; and the `$=pod` declarant of a `has` is the very
//! attribute `.^attributes` returns.

use super::*;
use crate::symbol::Symbol;

/// What one `Attribute` meta-object stands for: the attribute `sigil!name`
/// of the package `owner`.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct AttributeIdentity {
    pub(crate) owner: Symbol,
    pub(crate) sigil: char,
    pub(crate) name: Symbol,
}

/// The instance id every meta-object of `identity` carries, minted on first
/// use. Process-wide, as instance ids are: the threads of one program see one
/// attribute.
// Cost: O(1) expected (one hash probe under a mutex).
fn attribute_instance_id(identity: AttributeIdentity) -> u64 {
    static IDS: std::sync::OnceLock<std::sync::Mutex<HashMap<AttributeIdentity, u64>>> =
        std::sync::OnceLock::new();
    let mut ids = IDS
        .get_or_init(Default::default)
        .lock()
        .unwrap_or_else(|e| e.into_inner());
    *ids.entry(identity)
        .or_insert_with(crate::value::next_instance_id)
}

/// Build the `Attribute` meta-object `meta` describes, with `identity`'s one
/// instance id.
// Cost: O(k), k = meta keys.
pub(crate) fn attribute_meta_object(
    identity: AttributeIdentity,
    meta: HashMap<String, Value>,
) -> Value {
    Value::make_instance_with_id(
        Symbol::intern("Attribute"),
        meta,
        attribute_instance_id(identity),
    )
}

impl Interpreter {
    /// Create a list of BOOTSTRAPATTR instances for the Attribute class's own attributes.
    pub(super) fn make_bootstrapattr_list() -> Vec<Value> {
        // Raku's Attribute class has these well-known attributes (in order).
        // We model a subset that matches what the roast tests check.
        let bootstrapattrs: &[(&str, &str)] = &[
            ("name", "str"),
            ("type", "Mu"),
            ("build", "Mu"),
            ("package", "Mu"),
            ("inlined", "int"),
            ("has_accessor", "int"),
            ("rw", "int"),
            ("is_built", "int"),
            ("is_bound", "int"),
            ("required", "Mu"),
            ("container_descriptor", "Mu"),
            ("auto_viv_container", "Mu"),
            ("positional_delegate", "int"),
            ("associative_delegate", "int"),
            ("why", "Mu"),
            ("container_initializer", "Mu"),
            ("original", "Mu"),
            ("compose_order", "int"),
            ("composed", "int"),
            ("is_required", "int"),
            ("dimensions", "Mu"),
        ];
        bootstrapattrs
            .iter()
            .map(|(attr_name, type_name)| {
                let mut meta = HashMap::new();
                meta.insert("name".to_string(), Value::str(format!("$!{}", attr_name)));
                meta.insert(
                    "type".to_string(),
                    Value::package(Symbol::intern(type_name)),
                );
                meta.insert(
                    "__mutsu_attr_name".to_string(),
                    Value::str(attr_name.to_string()),
                );
                meta.insert("__mutsu_is_bootstrapattr".to_string(), Value::TRUE);
                attribute_meta_object(
                    AttributeIdentity {
                        owner: Symbol::intern("Attribute"),
                        sigil: '$',
                        name: Symbol::intern(attr_name),
                    },
                    meta,
                )
            })
            .collect()
    }

    /// Build an Attribute introspection object for a modelled built-in type
    /// attribute (private `$!name`, no accessor, read-only).
    pub(super) fn make_builtin_attribute_object(
        attr_name: &str,
        type_name: &str,
        owner: &str,
    ) -> Value {
        let has_accessor =
            crate::builtins::builtin_type_methods::builtin_type_attr_has_accessor(owner, attr_name);
        let mut meta = HashMap::new();
        meta.insert("name".to_string(), Value::str(format!("$!{}", attr_name)));
        meta.insert(
            "__mutsu_attr_name".to_string(),
            Value::str(attr_name.to_string()),
        );
        meta.insert(
            "__mutsu_attr_owner".to_string(),
            Value::str(owner.to_string()),
        );
        meta.insert("is_public".to_string(), Value::truth(has_accessor));
        meta.insert("is_rw".to_string(), Value::FALSE);
        meta.insert("sigil".to_string(), Value::str("$".to_string()));
        meta.insert(
            "type".to_string(),
            Value::package(Symbol::intern(type_name)),
        );
        meta.insert("has_accessor".to_string(), Value::truth(has_accessor));
        meta.insert("required".to_string(), Value::package(Symbol::intern("Mu")));
        attribute_meta_object(
            AttributeIdentity {
                owner: Symbol::intern(owner),
                sigil: '$',
                name: Symbol::intern(attr_name),
            },
            meta,
        )
    }
}
