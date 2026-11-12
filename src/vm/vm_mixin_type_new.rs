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
        if !self.mixin_type_default_new_applies(target, base, mixins) {
            return None;
        }
        let result = self.dispatch_new(target.clone(), args.to_vec());
        Some(self.materialize_exception_message_in_result(result))
    }

    /// Whether nothing in `base`'s MRO or the mixed-in roles of `mixins` can
    /// claim `new` (see [`Self::try_mixin_type_object_default_new`]).
    ///
    /// A function of the registry alone, so a verdict is remembered per mixin
    /// type object and the registry write generation it was reached under
    /// (#17331: Monad constructs every value from one of a handful of cached
    /// `pun but Marker` types, and the MRO and role probes cost ~11K
    /// instructions per construction). The entry holds the type object, so
    /// the overrides address it is keyed by cannot be reused while it lives.
    // Cost: O(1) expected on a memo hit; O(m + r) on a miss (see above).
    fn mixin_type_default_new_applies(
        &mut self,
        target: &Value,
        base: Symbol,
        mixins: &crate::gc::Gc<crate::value::MixinOverrides>,
    ) -> bool {
        let key = (base, crate::gc::Gc::as_ptr(mixins) as *const () as usize);
        let generation = self.registry_write_generation();
        if let Some((_, seen, applies)) = self.caches.mixin_default_new_memo.get(&key)
            && *seen == generation
        {
            return *applies;
        }
        // A base another thread punned (published, not yet adopted here) is
        // composed by the cascade's adoption step first; no verdict is kept.
        if !self.registry().classes.contains_key(base.as_str()) {
            return false;
        }
        let applies = !(self.mro_declares_user_new(base) || self.mro_declares_user_bless(base))
            && !Self::mixin_role_applications(mixins)
                .iter()
                .any(|(role, _)| {
                    self.role_or_parent_has_method(role, "new")
                        || self.role_or_parent_has_method(role, "bless")
                });
        self.caches
            .mixin_default_new_memo
            .insert(key, (target.clone(), generation, applies));
        applies
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
