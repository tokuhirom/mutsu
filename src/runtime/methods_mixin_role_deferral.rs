//! What a role method mixed into a value defers to: the candidates behind it
//! (`callsame` / `nextsame` / `callwith` / `nextwith` out of the role's
//! method).
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

    /// The sigil of the attribute `name` a mixed-in role declares.
    // Cost: O(r * a), r = roles mixed in, a = attributes each declares.
    pub(super) fn mixin_role_attribute_sigil(
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
}
