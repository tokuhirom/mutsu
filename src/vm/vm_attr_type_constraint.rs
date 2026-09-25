//! The declared type constraint of an attribute of the current `self`, as the
//! assignment paths read it on every `$!x = v` / `$.x = v` inside a method.
//!
//! The answer is a walk of `self`'s class MRO -- a sigil-collision scan, the
//! declaring class's type, and the nested-class qualification of that type --
//! and it depends on nothing but the class, the attribute name and the
//! registry. So it is memoized per `(class, attribute name)` and keyed on
//! [`Interpreter::registry_write_generation`], exactly as
//! `numeric_bridge_probe` does: every registry write goes through
//! `registry_mut()`, which bumps that generation on acquisition, so a later
//! class declaration, `augment` or MOP change cannot leave a stale answer
//! behind. It over-invalidates (any registry write drops every answer), which
//! costs one re-walk per attribute per write and never a wrong answer.
//! Debug builds re-derive the answer on every hit and assert it matches.

use super::*;

/// Per-`(class, attribute name)` memo of the declared attribute type
/// constraint, valid at [`Self::generation`].
#[derive(Default)]
pub(crate) struct AttrTypeConstraintCache {
    answers: rustc_hash::FxHashMap<(Symbol, Symbol), Option<String>>,
    generation: Option<u64>,
}

impl Interpreter {
    /// The class of the current `self` (the instance under a mixin).
    fn self_instance_class(&self) -> Option<Symbol> {
        let self_val = self.get_env_self()?;
        self_val.with_deref(|v| match v.view() {
            ValueView::Instance { class_name, .. } => Some(class_name),
            ValueView::Mixin(inner, _) => match inner.view() {
                ValueView::Instance { class_name, .. } => Some(class_name),
                _ => None,
            },
            _ => None,
        })
    }

    /// Look up the declared type constraint of an attribute of the current
    /// `self` instance, walking the MRO on a miss.
    // Cost: O(1) on a hit (two hash probes); O(d * a) on a miss, see
    // `attr_type_constraint_uncached`.
    pub(crate) fn self_attr_type_constraint(&self, attr_name: &str) -> Option<String> {
        self.self_attr_type_constraint_sym(attr_name, Symbol::intern(attr_name))
    }

    /// [`Self::self_attr_type_constraint`] for a caller that already holds
    /// `attr_name`'s interned form.
    // Cost: O(1) on a hit; O(d * a) on a miss.
    pub(crate) fn self_attr_type_constraint_sym(
        &self,
        attr_name: &str,
        attr_sym: Symbol,
    ) -> Option<String> {
        let class = self.self_instance_class()?;
        let generation = self.registry_write_generation();
        let key = (class, attr_sym);
        {
            let mut cache = self.attr_type_constraint_cache.borrow_mut();
            if cache.generation != Some(generation) {
                cache.answers.clear();
                cache.generation = Some(generation);
            } else if let Some(hit) = cache.answers.get(&key) {
                debug_assert_eq!(
                    *hit,
                    self.attr_type_constraint_uncached(class.as_str(), attr_name),
                    "stale attribute type constraint for {class}.{attr_name}"
                );
                return hit.clone();
            }
        }
        let answer = self.attr_type_constraint_uncached(class.as_str(), attr_name);
        // The walk reads the registry only, but guard against a write made
        // during it all the same: remember the answer only for the generation
        // it was computed under.
        if self.registry_write_generation() == generation {
            self.attr_type_constraint_cache
                .borrow_mut()
                .answers
                .insert(key, answer.clone());
        }
        answer
    }

    /// [`Self::self_attr_type_constraint`] for an explicit class, uncached.
    // Cost: O(d * a), d = ancestors of `class_name`, a = attributes declared
    // per ancestor (the sigil-collision scan).
    fn attr_type_constraint_uncached(&self, class_name: &str, attr_name: &str) -> Option<String> {
        let (bare, sigil) = if let Some((bare, _)) = crate::value::attr_twigil_base(attr_name) {
            (
                bare,
                crate::value::attr_twigil_sigil(attr_name).unwrap_or('$'),
            )
        } else {
            (attr_name, '$')
        };
        // One MRO for the whole lookup: an `Arc` of the registry's cached C3
        // order, where this used to copy it into `String`s up to three times
        // per attribute store (ADR-0121 D1).
        let mro = self.mro_syms_readonly(class_name);
        let has_sigil_collision = mro.iter().any(|cls| {
            self.registry()
                .classes
                .get(cls.as_str())
                .is_some_and(|class_def| {
                    class_def
                        .attributes
                        .iter()
                        .any(|attr| attr.name == bare && attr.sigil != sigil)
                })
        });
        let tc = if has_sigil_collision {
            mro.iter().find_map(|cls| {
                self.registry()
                    .classes
                    .get(cls.as_str())
                    .and_then(|class_def| {
                        class_def
                            .attributes
                            .iter()
                            .find(|attr| attr.name == bare && attr.sigil == sigil)
                            .and_then(|attr| attr.type_constraint.clone())
                    })
            })?
        } else {
            self.get_attr_type_constraint(class_name, bare)?
        };
        // A nested class type (`class URI { class Authority {}; has Authority
        // $.authority }`) is declared by its short name but registered fully
        // qualified — resolve it so the reset type object dispatches methods.
        if !self.registry().classes.contains_key(&tc) {
            for cls in mro.iter() {
                let qualified = format!("{}::{}", cls, tc);
                if self.registry().classes.contains_key(&qualified) {
                    return Some(qualified);
                }
            }
        }
        Some(tc)
    }
}
