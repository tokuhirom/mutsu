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

/// One `mixin_default_new_memo` entry: the type object (held, so its
/// overrides address stays its own), the registry write generation, whether
/// the default constructor applies, and the composed class it builds through.
pub(crate) type MixinDefaultNewVerdict = (Value, u64, bool, Option<Symbol>);

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
        if method_sym != crate::sym!("new")
            || crate::runtime::find_method_intercept::any_user_find_method()
        {
            return None;
        }
        let ValueView::Mixin(inner, mixins) = target.view() else {
            return None;
        };
        let ValueView::Package(base) = inner.view() else {
            return None;
        };
        let (applies, class) = self.mixin_type_default_new_plan(target, base, mixins);
        if !applies {
            return None;
        }
        // The composed `Base+{R}` class is what `dispatch_new` would build
        // through; constructing it on the default constructor's fast path is
        // the same call a `Class.new` site makes (#17228: Monad builds every
        // value from one of these, at 3x a plain class's cost when each call
        // re-found the class and went through the general constructor).
        if let Some(class) = class
            && let Some(result) = self.try_native_default_construct(class, args)
        {
            return Some(self.materialize_exception_message_in_result(result));
        }
        let result = self.dispatch_new(target.clone(), args.to_vec());
        Some(self.materialize_exception_message_in_result(result))
    }

    /// Whether nothing in `base`'s MRO or the mixed-in roles of `mixins` can
    /// claim `new` (see [`Self::try_mixin_type_object_default_new`]), and the
    /// composed class the default constructor then builds through.
    ///
    /// A function of the registry alone, so a verdict is remembered per mixin
    /// type object and the registry write generation it was reached under
    /// (#17331: Monad constructs every value from one of a handful of cached
    /// `pun but Marker` types, and the MRO and role probes cost ~11K
    /// instructions per construction). The entry holds the type object, so
    /// the overrides address it is keyed by cannot be reused while it lives.
    // Cost: O(1) expected on a memo hit; O(m + r) on a miss (see above).
    fn mixin_type_default_new_plan(
        &mut self,
        target: &Value,
        base: Symbol,
        mixins: &crate::gc::Gc<crate::value::MixinOverrides>,
    ) -> (bool, Option<Symbol>) {
        let key = (base, crate::gc::Gc::as_ptr(mixins) as *const () as usize);
        let generation = self.registry_write_generation();
        if let Some((_, seen, applies, class)) = self.caches.mixin_default_new_memo.get(&key)
            && *seen == generation
        {
            return (*applies, *class);
        }
        // A base another thread punned (published, not yet adopted here) is
        // composed by the cascade's adoption step first; no verdict is kept.
        if !self.registry().classes.contains_key(base.as_str()) {
            return (false, None);
        }
        let applies = !(self.mro_declares_user_new(base) || self.mro_declares_user_bless(base))
            && !Self::mixin_role_applications(mixins)
                .iter()
                .any(|(role, _)| {
                    self.role_or_parent_has_method(role, "new")
                        || self.role_or_parent_has_method(role, "bless")
                });
        // The class `dispatch_new` builds through: only when it takes the
        // composed-class route (not the positional-constructor base, whose
        // class it reaches differently, nor the construct-then-compose path).
        let class = if applies
            && !crate::runtime::default_new_accepts_positionals(&self.class_mro(base.as_str()))
        {
            match self.mixin_type_object_class(&Value::package(base), mixins) {
                Ok(Some(class)) => Some(Symbol::intern(&class)),
                _ => None,
            }
        } else {
            None
        };
        // Read after composing the class: composing it moved the generation.
        let generation = self.registry_write_generation();
        self.caches
            .mixin_default_new_memo
            .insert(key, (target.clone(), generation, applies, class));
        (applies, class)
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
