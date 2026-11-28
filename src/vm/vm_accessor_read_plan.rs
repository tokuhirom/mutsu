//! The gate in front of the fast public-accessor read (`$obj.x`), memoized.
//!
//! [`Interpreter::try_fast_accessor_read`] may answer a 0-arg call on an
//! instance only when the generated accessor is what full dispatch would run:
//! the accessor is not wrapped, the class is not a CStruct, the name is not
//! one the fast read never takes, and the public accessor wins the MRO race
//! against user methods of that name. Each of those is a registry probe, and
//! together they cost ~1,000 instructions on every accessor read (#19293).
//! Every one is a pure function of the registry for a given `(class,
//! method)`, so the combined answer is recorded in the registry-generation-
//! keyed probe memo (`runtime/user_method_probe_memo.rs`) and a later read
//! pays one memo lookup.

use crate::runtime::{Interpreter, UserMethodOrAccessor};
use crate::symbol::Symbol;

/// Names the fast accessor read never answers, even when a public attribute
/// of that name exists: object-protocol and introspection methods whose
/// built-in meaning a generated accessor must not shadow on this path.
// Cost: O(1) — one string match over a fixed set.
pub(super) fn accessor_fast_read_excluded(method: &str) -> bool {
    matches!(
        method,
        "new"
            | "BUILD"
            | "TWEAK"
            | "BUILDALL"
            | "DESTROY"
            | "Bool"
            | "so"
            | "not"
            | "defined"
            | "DEFINITE"
            | "WHAT"
            | "WHO"
            | "HOW"
            | "WHY"
            | "WHICH"
            | "WHERE"
            | "VAR"
            | "Str"
            | "gist"
            | "raku"
            | "ACCEPTS"
            | "isa"
            | "does"
            | "can"
            | "^name"
            | "^mro"
            | "^methods"
            | "^attributes"
            | "sink"
            | "self"
            | "clone"
            | "return"
            | "handled"
            | "bytes"
    )
}

impl Interpreter {
    /// Whether every gate of the fast accessor read passes for `method` on an
    /// instance of `class`, so the read can go straight to the attribute map.
    /// `false` means "run the gates one by one" (some of them answer the call
    /// another way, e.g. a CStruct field), never "decline".
    // Cost: O(1) on a memo hit; a miss costs the probes below, each O(d),
    // d = MRO depth.
    pub(super) fn accessor_read_is_plain(&mut self, class: Symbol, method: Symbol) -> bool {
        self.accessor_read_plain_memo(class, method, |this| {
            let cn = class.as_str();
            let name = method.as_str();
            if accessor_fast_read_excluded(name) || this.cstruct_class_name(cn).is_some() {
                return false;
            }
            if this.class_attribute_deprecated(cn, name).is_some() {
                return false;
            }
            if !matches!(
                this.resolve_user_method_or_accessor_sym(cn, method),
                Some(UserMethodOrAccessor::Accessor)
            ) {
                return false;
            }
            !this.attribute_accessor_owner(cn, name).is_some_and(|owner| {
                this.registry()
                    .method_wrap_chain(owner.as_str(), name, 0)
                    .is_some()
            })
        })
    }
}
