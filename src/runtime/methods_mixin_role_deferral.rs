//! What a role method mixed into a value defers to: the candidates behind it
//! (`callsame` / `nextsame` / `callwith` / `nextwith` out of the role's
//! method), and the `Mu` base `clone` of a role mixin.
use super::*;
use crate::meta_ns::MetaNs;
use crate::symbol::Symbol;

impl Interpreter {
    /// Whether `owner` is a role this mixin composes (`__mutsu_role__<owner>`).
    // Cost: O(1), one hash probe.
    fn mixin_composes_role(mixins: &crate::value::MixinOverrides, owner: &str) -> bool {
        mixins
            .get(MetaNs::Role.str_key_for_str(owner))
            .is_some_and(Value::truthy)
    }

    /// Whether a role this mixin composes declares a method `name` with a
    /// body (a stub only requires it).
    // Cost: O(r), r = roles mixed in (one method-table probe each).
    pub(super) fn mixin_role_declares_method(
        &self,
        mixins: &crate::value::MixinOverrides,
        name: &str,
    ) -> bool {
        mixins
            .iter()
            .filter(|(_, applied)| applied.truthy())
            .filter_map(|(key, _)| key.strip_prefix("__mutsu_role__"))
            .any(|role| {
                self.registry().roles.get(role).is_some_and(|def| {
                    def.methods.get(name).is_some_and(|overloads| {
                        overloads
                            .iter()
                            .any(|d| !d.is_private && !Self::is_runtime_stub_method_def(d))
                    })
                })
            })
    }

    /// The sigil of the attribute `name` a mixed-in role declares.
    // Cost: O(r * a), r = roles mixed in, a = attributes each declares.
    fn mixin_role_attribute_sigil(
        &self,
        mixins: &crate::value::MixinOverrides,
        name: &str,
    ) -> Option<char> {
        mixins
            .iter()
            .filter_map(|(key, _)| key.strip_prefix("__mutsu_role__"))
            .find_map(|role| {
                self.registry().roles.get(role).and_then(|def| {
                    def.attributes
                        .iter()
                        .find(|attr| attr.name == name)
                        .map(|attr| attr.sigil)
                })
            })
    }

    /// The candidates behind a role method of a mixin whose inner value is an
    /// instance of `base_class`, after the base class's own candidates
    /// (`ranked`, the deferral expansion).
    ///
    /// A candidate a mixed-in role owns is the role's own method, which the
    /// role walk already tried: a punned role's instance (`R.new`) wraps an
    /// instance of the pun class `R`, whose MRO answers `R`'s methods again,
    /// so a `nextwith` out of `R.clone` re-entered `R.clone` instead of
    /// reaching `Mu.clone` (Dan's `DataFrame.clone`). With nothing left, the
    /// `Mu` base answers the names `Mu` implements natively, as in an ordinary
    /// method frame (`build_method_dispatch_frame`'s `mu_base_override`).
    // Cost: O(c), c = ranked candidates (one hash probe each).
    pub(super) fn mixin_role_base_deferral(
        &self,
        mixins: &crate::value::MixinOverrides,
        method: &str,
        ranked: Vec<(Symbol, MethodDef)>,
    ) -> Vec<super::DeferralEntry> {
        let mut remaining: Vec<super::DeferralEntry> = ranked
            .into_iter()
            .filter(|(owner, def)| {
                !Self::mixin_composes_role(mixins, &owner.resolve())
                    && !def
                        .role_origin
                        .as_deref()
                        .is_some_and(|role| Self::mixin_composes_role(mixins, role))
            })
            .map(|(owner, def)| super::DeferralEntry::Candidate {
                owner,
                def: Box::new(def),
                wraps_spliced: false,
            })
            .collect();
        if remaining.is_empty()
            && matches!(
                method,
                "clone" | "BUILDALL" | "POPULATE" | "gist" | "Str" | "raku" | "perl" | "defined" | "Bool" | "so"
                    | "not" | "WHICH" | "WHERE"
            )
        {
            remaining.push(super::DeferralEntry::Native {
                name: method.to_string(),
                base: super::NativeBase::MuBase,
            });
        }
        remaining
    }

    /// `.clone` of a role mixin: `Mu.clone` for a value that composes roles.
    ///
    /// A punned role's `__mutsu_attr__*` entries are construction seeds for
    /// the Mixin-owned role cell, not the live state. The generic instance
    /// clone unwraps to the inner value and drops the role composition, so
    /// the clone lost all attributes (`Zef::Client.fetch`'s
    /// `$candi.clone(:$dist)` then had no `.as`). Clone the mixin instead:
    /// recursively clone the inner value, deep-copy the role cell, copy every
    /// marker, and apply each `:attr(val)` override to the new role cell.
    /// `None` for a mixin with no role marker (an allomorph, a non-role
    /// `but`), which keeps its own path.
    // Cost: O(a + m), a = override args, m = mixin markers copied, plus the
    // inner value's clone.
    pub(crate) fn clone_role_mixin(
        &mut self,
        target: &Value,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let ValueView::Mixin(inner, mixins) = target.view() else {
            return None;
        };
        if !mixins
            .keys()
            .any(|k| k.starts_with("__mutsu_role__") || k.starts_with("__mutsu_attr__"))
        {
            return None;
        }
        let inner_owned = inner.as_ref().clone();
        let mut new_mixins = mixins.as_ref().clone();
        // An override names either a role attribute (a `__mutsu_attr__`
        // marker) or an attribute of the inner class. Apply the former to the
        // marker; forward the rest to the inner clone so `$w.clone(:id(99))`
        // on a `Widget but Named` still overrides Widget's own `$.id`.
        // An override that lands on a role marker must reach the deep-copied
        // role cell too — otherwise the accessor would keep serving the
        // pre-clone value from that cell.
        let inner_attrs = Self::self_instance_attrs(inner);
        let mut inner_args: Vec<Value> = Vec::new();
        for arg in args {
            if let ValueView::Pair(key, val) = arg.view()
                && new_mixins.contains_key(&MetaNs::Attr.owned_key_for_str(key))
            {
                // Assigned as `Mu.clone` assigns: a list into an `@`
                // attribute becomes that attribute's own Array.
                let val = match self.mixin_role_attribute_sigil(mixins, key) {
                    Some(sigil) => Self::coerce_provided_attr_value_by_sigil(val.clone(), sigil),
                    None => val.clone(),
                };
                new_mixins.insert(MetaNs::Attr.owned_key_for_str(key), val.clone());
                let attr = Symbol::intern(key);
                if new_mixins.set_role_attribute_by_name(key, val.clone()) {
                    continue;
                }
                let in_cell = inner_attrs
                    .as_ref()
                    .is_some_and(|c| c.as_map().contains_key(attr));
                if !in_cell {
                    continue;
                }
            }
            inner_args.push(arg.clone());
        }
        let inner_clone =
            match self.call_method_with_values_unviewed(inner_owned, "clone", inner_args) {
                Ok(v) => v,
                Err(e) => return Some(Err(e)),
            };
        let cloned = Value::mixin_with_state(inner_clone, new_mixins);
        // A cloned routine's own cell holds the copied composition
        // (ADR-11827 §2.3).
        Some(Ok(Self::note_routine_composition(cloned, None)))
    }
}
