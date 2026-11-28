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
use crate::value::{RuntimeError, Value, ValueView};

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
            !this
                .attribute_accessor_owner(cn, name)
                .is_some_and(|owner| {
                    this.registry()
                        .method_wrap_chain(owner.as_str(), name, 0)
                        .is_some()
                })
        })
    }

    /// A 0-arg plain call (`self.name`, `$obj.x`) whose receiver is an
    /// instance and whose name is a plain public accessor of its class, read
    /// before the general `CallMethod` preamble.
    ///
    /// The preamble runs some twenty lanes before it reaches the accessor read
    /// (~2,900 instructions per call, #19293). For this receiver and name each
    /// of them declines: they act on a modifier, a quoted name, a container
    /// request, Seq/Slip/lazy/mixin/Package/container receivers, a wrapped or
    /// `^find_method` method, `.take`, a thrown exception's `.message`, an
    /// `Attribute` meta-object or a `Sequence` role method -- each excluded
    /// here -- or on a name the accessor gate already refuses (`WHAT`, `Str`,
    /// `gist`, `raku`, `return`, ...).
    // Cost: O(1) — a few tag tests and one memo lookup, plus the read.
    pub(super) fn try_early_accessor_lane(
        &mut self,
        method_sym: Symbol,
        arity: u32,
        modifier_idx: Option<u32>,
        quoted: bool,
        arg_sources_idx: Option<u32>,
    ) -> Option<Result<Value, RuntimeError>> {
        if arity != 0
            || modifier_idx.is_some()
            || quoted
            || arg_sources_idx.is_some()
            || self.accessor_ref_pending
            || self.has_any_wrap_chains()
            || crate::runtime::find_method_intercept::any_user_find_method()
        {
            return None;
        }
        let method = method_sym.as_str();
        if matches!(method, "take" | "message") || Self::is_sequence_role_method(method) {
            return None;
        }
        let target = self.stack.last()?;
        if target.is_lazy_match_value() {
            return None;
        }
        let ValueView::Instance { class_name, .. } = target.view() else {
            return None;
        };
        if class_name == crate::sym!("Attribute")
            || !self.accessor_read_is_plain(class_name, method_sym)
        {
            return None;
        }
        let target = self.stack.pop()?;
        self.set_pending_call_arg_sources(None);
        match self.try_fast_accessor_read(&target, method_sym, &[], false, false, false) {
            Some(result) => {
                crate::vm::vm_stats::record_dispatch_entry_outcome("callmethod", "accessor");
                // A site that may record this answer (`vm_method_site_ic`)
                // replays it through the accessor lane, filled from here.
                if result.is_ok() && self.caches.method_site_ic_site.is_some() {
                    self.note_accessor_lane(&target, method_sym);
                }
                Some(result)
            }
            None => {
                self.stack.push(target);
                None
            }
        }
    }
}
