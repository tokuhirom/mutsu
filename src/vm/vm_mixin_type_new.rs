//! `.new` on a role-mixed type object (`Base but R`, `(C but R).new`).
//!
//! Such a call has no `ValueView::Package` receiver, so none of the VM's
//! construction lanes claims it and it falls into the interpreter's whole
//! method cascade: the by-name dispatch groups, mixin method dispatch, an MRO
//! walk (`classhow_mro_unhidden_names`, rebuilt per call) and finally the
//! late `Mu.new` row, which is `dispatch_new` on the receiver. When nothing
//! in that cascade can claim `new` (no user `new` or `bless` in the base
//! class's MRO or any mixed-in role, no user `^find_method` anywhere), the
//! answer is that same `dispatch_new` call, made directly. Monad builds every
//! value this way (`parameterized(M, T).^pun but Monad::Maybe::Some`).

use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

impl Interpreter {
    /// `.new(args)` on a role-mixed type object whose construction is the
    /// default constructor's, made directly; `None` for any other receiver or
    /// when a user `new`/`bless`/`^find_method` could claim the call.
    // Cost: O(m + r), m = the base class's MRO, r = mixed-in roles (each a
    // memoized method probe), plus the construction itself.
    pub(crate) fn try_mixin_type_object_default_new(
        &mut self,
        target: &Value,
        method_sym: Symbol,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if method_sym != "new" || crate::runtime::find_method_intercept::any_user_find_method() {
            return None;
        }
        let ValueView::Mixin(inner, mixins) = target.view() else {
            return None;
        };
        let ValueView::Package(base) = inner.view() else {
            return None;
        };
        // A base another thread punned (published, not yet adopted here) is
        // composed by the cascade's adoption step first.
        if !self.registry().classes.contains_key(base.as_str()) {
            return None;
        }
        if self.mro_declares_user_new(base) || self.mro_declares_user_bless(base) {
            return None;
        }
        let roles = Self::mixin_role_applications(mixins);
        if roles.iter().any(|(role, _)| {
            self.role_or_parent_has_method(role, "new")
                || self.role_or_parent_has_method(role, "bless")
        }) {
            return None;
        }
        let result = self.dispatch_new(target.clone(), args.to_vec());
        Some(self.materialize_exception_message_in_result(result))
    }

    /// Whether any class of `class`'s MRO declares a user `bless`.
    // Cost: O(m), m = MRO length.
    fn mro_declares_user_bless(&mut self, class: Symbol) -> bool {
        let mro = self.class_mro(class.as_str());
        let registry = self.registry();
        mro.iter().any(|layer| {
            registry
                .user_method_overloads(layer.as_str(), "bless")
                .is_some()
        })
    }
}
