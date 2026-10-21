//! The one shadow query: does a user-level declaration take a call from the
//! built-in method row it would otherwise reach (ADR-11276 §2.3, §2.4)?
//!
//! Rakudo has no such question: a built-in method is an ordinary method in
//! its class's method table, and resolution walks the receiver's MRO once.
//! mutsu's built-in rows live in their own table, so every entry that can
//! answer a call from a row (the interpreter's `call_method_with_values`, the
//! VM's `try_native_method`, and the `CallMethod`/`CallMethodMut` lanes) asks
//! first whether something the program declared comes first. Until slice 5
//! each entry wrote its own copy of that question, and the copies drifted:
//! one counted a public accessor only for a quoted name, another ignored an
//! augmented multi that the built-in outranks. This is the one answer they
//! all read.
//!
//! What it does *not* decide is whether the receiver's own state needs the
//! interpreter (a lazy `Seq` body, a `Proxy`, a computed exception message):
//! those are receiver guards, which stay with each entry.

use super::*;
use crate::symbol::Symbol;

/// The six MOP pseudo-methods a user method cannot override except through a
/// quoted call (`$x."WHAT"()`): Rakudo compiles them as macros. `WHICH` and
/// `WHY` are not among them: they are ordinary, overridable methods.
// Cost: O(1).
pub(crate) fn is_mop_pseudo_method(method: &str) -> bool {
    matches!(
        method,
        "DEFINITE" | "WHAT" | "WHO" | "HOW" | "WHERE" | "VAR"
    )
}

impl Interpreter {
    /// Whether a declaration of the program takes `target.method(|args)` from
    /// the built-in row: a method (or a public or class-scoped accessor) of
    /// the receiver's class or a role it does, a role
    /// mixed into the receiver, an `augment` or `.wrap` of the built-in type
    /// it is, or, for a `Cool`-only name the class lacks, its `handles *` or
    /// `FALLBACK`. `quoted` is a call whose name was a string literal
    /// (`$x."WHAT"()`), the one form in which a MOP pseudo-method can be
    /// overridden.
    // Cost: O(1) amortized: each probe is memoized per `(class, method)` for
    // one registry write generation; a miss is O(d), d = MRO depth.
    pub(crate) fn user_shadows_builtin(
        &mut self,
        target: &Value,
        method: Symbol,
        args: &[Value],
        quoted: bool,
    ) -> bool {
        let name = method.as_str();
        // A deferral (`nextsame`/`callsame`) into the built-in method runs it
        // for exactly this receiver: nothing shadows it there.
        if self.native_base_bypass_hit(target, method) {
            return false;
        }
        if is_mop_pseudo_method(name) {
            return quoted
                && match target.view() {
                    ValueView::Instance { class_name, .. } | ValueView::Package(class_name) => {
                        self.grammar_has_user_method_memo(class_name, method)
                    }
                    _ => false,
                };
        }
        // A lazy Match dispatches under its cursor's class (a grammar's own
        // methods come first); `view()` would materialize it.
        if target.is_lazy_match_value() {
            let owner = Symbol::intern(target.match_dispatch_class());
            return self.instance_class_shadows_builtin(owner, method, args);
        }
        match target.view() {
            ValueView::Instance { class_name, .. } => {
                if self.instance_class_shadows_builtin(class_name, method, args) {
                    return true;
                }
            }
            ValueView::Package(class_name) => {
                return self.package_shadows_builtin(target, class_name, method, args);
            }
            // A role mixed into a built-in value (`42 but R`, `my @a is R`):
            // its own `augment`ed type comes first too.
            ValueView::Mixin(..) => {
                if self.native_lever_a_user_override_sym(target, method) {
                    return true;
                }
            }
            // An enum member: a role its enum composes (`enum E does R`)
            // comes before the enumeration roles' own rows.
            ValueView::Enum { .. } => {
                return self.enum_composes_role_method(target, name)
                    || self.native_lever_a_user_override_sym(target, method);
            }
            // A plain built-in receiver (`Array`, `Str`, a `Range`, ...): only
            // an `augment` or a `.wrap` of its type comes first.
            _ => {
                return self.native_lever_a_user_override_sym(target, method) && {
                    // An `augment`ed `multi` is ranked with the built-in
                    // candidates of the method (#12751).
                    let owner = self.dispatch_owner_name(target);
                    !self.builtin_candidate_outranks_user(target, owner, name, args)
                };
            }
        }
        self.mixin_role_has_method(target, name) || self.mixin_wrapped_instance_has_method(target, name)
    }

    /// [`Self::user_shadows_builtin`] for an instance of `class`.
    // Cost: O(1) amortized, see `user_shadows_builtin`.
    fn instance_class_shadows_builtin(
        &mut self,
        class: Symbol,
        method: Symbol,
        args: &[Value],
    ) -> bool {
        let cn = class.as_str();
        let name = method.as_str();
        // An augmented slurpy-only `multi` on a built-in ancestor ranks after
        // the built-in method, so it takes no call (#12751, #13509).
        ((self.grammar_has_user_method_memo(class, method)
            || self.resolve_user_method_or_accessor_sym(cn, method).is_some())
            && !self.user_candidates_all_outranked_by_builtin(cn, name))
            || self.class_scoped_accessor_shadows(cn, name, args)
            // A `Cool`-only name (`.uc`, `.flip`, ...) is not resolvable on a
            // plain `Any`-derived instance, so `handles *` / `FALLBACK` gets
            // it; name-gated first so the MRO walk runs only for those names.
            || (Self::cool_only_builtin_method(name)
                && self.class_has_wildcard_handles_or_fallback(cn))
    }

    /// [`Self::user_shadows_builtin`] for the type object `class`. A type
    /// object has no instance attributes, so only a class-scoped accessor
    /// counts. Its `gist`/`raku` with no argument shadow only when a user
    /// candidate accepts the type object: an inherited `multi raku(::?CLASS:D:)`
    /// leaves `Mu`'s rendering to answer, as in Rakudo.
    // Cost: O(1) amortized, plus one candidate resolution for `gist`/`raku`.
    fn package_shadows_builtin(
        &mut self,
        target: &Value,
        class: Symbol,
        method: Symbol,
        args: &[Value],
    ) -> bool {
        let cn = class.as_str();
        let name = method.as_str();
        (self.grammar_has_user_method_memo(class, method)
            && (!matches!(name, "gist" | "raku")
                || !args.is_empty()
                || self
                    .resolve_method_with_owner_invocant(cn, name, args, target)
                    .is_some()))
            || self.class_scoped_accessor_shadows(cn, name, args)
    }
}
