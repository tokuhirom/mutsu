//! Which instance classes are `Attribute` meta-objects.
//!
//! `$attr does R` reblesses the meta-object into the mixin type
//! `Attribute+{R}` (stacked: `Attribute+{R}+{S}`), exactly as Rakudo does, so
//! every holder of the object sees the role. The code that answers an
//! `Attribute`'s own facts asks this predicate instead of comparing the class
//! name with `"Attribute"`, which a reblessed meta-object no longer has.

use crate::symbol::Symbol;

/// The mixin types a reblessed meta-object's class name starts with.
const MIXIN_PREFIX: &str = "Attribute+{";

/// Whether an instance of `class` is an `Attribute` meta-object: `Attribute`
/// itself, or a mixin type a `does` reblessed one into.
// Cost: O(1), one symbol compare plus a fixed-length prefix compare.
pub(crate) fn is_attribute_class(class: Symbol) -> bool {
    class == "Attribute" || class.as_str().starts_with(MIXIN_PREFIX)
}

/// Whether `value` is the result of composing a role into an `Attribute`
/// meta-object: a reblessed instance of an `Attribute+{...}` mixin type, or
/// the `Mixin` wrapper the other meta-objects still get. The attribute-trait
/// machinery records either as the attribute's meta-object.
// Cost: O(1).
pub(crate) fn is_composed_meta_object(value: &crate::value::Value) -> bool {
    match value.view() {
        crate::value::ValueView::Mixin(..) => true,
        crate::value::ValueView::Instance { class_name, .. } => {
            class_name.as_str().starts_with(MIXIN_PREFIX)
        }
        _ => false,
    }
}
