//! A closure capture viewed as a stack of shared tiers rather than one copied
//! map (#9170).
//!
//! A closure capture keeps the closure's free variables plus every visible
//! *system name* — types, constants, dynamics, `__mutsu_*` metadata (ADR-0094
//! §4). Built as one flat map, that made every closure creation cost
//! O(system names in scope): a file that declares a thousand classes paid for
//! all of them on each `-> { }` it evaluated, whether or not the closure named
//! one.
//!
//! The system names of a wide tier change rarely, so each tier memoizes them
//! once ([`crate::env_tier::Tier::capture_sys`]) and a capture refers to that
//! memo instead of copying it. What a capture copies is only what is its own:
//! the free variables, the volatile names (the topic, `$/`, `$!`), and the
//! system names of the narrow frame tiers it was created in. The result is a
//! [`CaptureView`]: that own tier first, then the shared layers in the order
//! of the scopes they came from.
//!
//! A layer can carry a *hidden* set: names the layer must not answer for. The
//! closure's own parameters and locals shadow any enclosing system name of the
//! same spelling (a WhateverCode's `_`, a block's `@_`), which the flat
//! capture expressed by leaving them out of the copy. The layers are shared, so
//! the closure instead hides those names from every layer below its own tier.
//!
//! See ADR-9170.

use std::sync::{Arc, OnceLock};

use rustc_hash::FxHashSet;

use crate::env_tier::{SymMap, Tier};
use crate::symbol::Symbol;
use crate::value::Value;

/// One tier of a [`CaptureView`].
#[derive(Clone)]
pub(crate) struct Layer {
    pub(crate) tier: Arc<Tier>,
    /// Names this layer must not answer for — see the module docs. A lookup
    /// that finds such a name here moves on to the next layer.
    pub(crate) hidden: Option<Arc<FxHashSet<Symbol>>>,
    /// True for a tier's memoized system names
    /// ([`crate::env_tier::CaptureSys::sys`]): holds nothing but system names,
    /// so a nested capture can take it over as it is. False for a capture's
    /// own tier, which also holds that closure's free variables.
    pub(crate) shared: bool,
    /// True for a layer that resolves ABOVE the base tiers ([`GLOBAL_BASE`],
    /// the built-in dynamics) when the capture is looked up as itself: the
    /// creating chain's system names, which the flat copy held in its own map.
    /// False for a layer that was already a capture fallback when this capture
    /// was built, which resolves below them -- the precedence the call-time
    /// fallback gives every captured name (ADR-0092). [`CaptureView::over`]
    /// clears it, since a frame consults its whole capture below the base.
    ///
    /// [`GLOBAL_BASE`]: crate::env
    pub(crate) above_base: bool,
}

impl Layer {
    /// A capture's own tier, answering for every name it holds.
    pub(crate) fn open(tier: Arc<Tier>) -> Self {
        Self {
            tier,
            hidden: None,
            shared: false,
            above_base: false,
        }
    }

    /// A tier's memoized system names, with `hidden` shadowed out.
    pub(crate) fn shared(
        tier: Arc<Tier>,
        hidden: Option<Arc<FxHashSet<Symbol>>>,
        above_base: bool,
    ) -> Self {
        Self {
            tier,
            hidden,
            shared: true,
            above_base,
        }
    }

    #[inline]
    fn answers(&self, key: &Symbol) -> Option<&Value> {
        if self.hidden.as_ref().is_some_and(|h| h.contains(key)) {
            return None;
        }
        self.tier.get(key)
    }
}

/// A closure capture as a stack of tiers, highest precedence first. Immutable
/// once built. See the module docs.
pub(crate) struct CaptureView {
    layers: Box<[Layer]>,
    /// Whether the first layer resolves above the base tiers.
    has_above_base: bool,
    /// The layers folded into one map, for the consumers that iterate a
    /// capture rather than look names up in it. Built on first ask.
    merged: OnceLock<Arc<Tier>>,
}

impl CaptureView {
    pub(crate) fn new(layers: Vec<Layer>) -> Self {
        debug_assert!(
            layers
                .windows(2)
                .all(|w| w[0].above_base || !w[1].above_base),
            "above-base layers come first"
        );
        let has_above_base = layers.first().is_some_and(|l| l.above_base);
        Self {
            layers: layers.into_boxed_slice(),
            has_above_base,
            merged: OnceLock::new(),
        }
    }

    /// Whether any layer resolves above the base tiers (see
    /// [`Layer::above_base`]).
    #[inline]
    pub(crate) fn has_above_base(&self) -> bool {
        self.has_above_base
    }

    /// [`Self::get`] over the layers that resolve above the base tiers.
    #[inline]
    pub(crate) fn get_above_base(&self, key: &Symbol) -> Option<&Value> {
        self.layers
            .iter()
            .take_while(|l| l.above_base)
            .find_map(|layer| layer.answers(key))
    }

    /// [`Self::get`] over the layers that resolve below the base tiers.
    #[inline]
    pub(crate) fn get_below_base(&self, key: &Symbol) -> Option<&Value> {
        self.layers
            .iter()
            .skip_while(|l| l.above_base)
            .find_map(|layer| layer.answers(key))
    }

    /// Every visible entry, with the names `base` answers left out of the
    /// layers that resolve below it: what iterating the capture as itself
    /// shows.
    // Cost: O(e), e = entries over all layers.
    pub(crate) fn fold_over_base(&self, base: Option<&SymMap>) -> SymMap {
        let mut out = SymMap::default();
        for layer in self.layers.iter().rev() {
            for (k, v) in layer.tier.iter() {
                if layer.hidden.as_ref().is_some_and(|h| h.contains(k)) {
                    continue;
                }
                if !layer.above_base && base.is_some_and(|b| b.contains_key(k)) {
                    continue;
                }
                out.insert(*k, v.clone());
            }
        }
        out
    }

    /// A view of one tier, which is the whole capture.
    pub(crate) fn single(tier: Arc<Tier>) -> Self {
        Self::new(vec![Layer::open(tier)])
    }

    /// A view with `own` over every layer of `below`, with `removed` (names
    /// taken out of the capture after it was built) hidden from those layers.
    // Cost: O(l), l = layers of `below`; O(l * r) with r = removed names.
    pub(crate) fn over(
        own: Arc<Tier>,
        below: &CaptureView,
        removed: Option<&FxHashSet<Symbol>>,
    ) -> Self {
        let mut layers = Vec::with_capacity(below.layers.len() + 1);
        layers.push(Layer::open(own));
        let removed = removed.filter(|r| !r.is_empty());
        for layer in below.layers.iter() {
            let hidden = match removed {
                None => layer.hidden.clone(),
                Some(removed) => {
                    let mut hidden: FxHashSet<Symbol> =
                        layer.hidden.as_deref().cloned().unwrap_or_default();
                    hidden.extend(removed.iter().copied());
                    Some(Arc::new(hidden))
                }
            };
            layers.push(Layer {
                tier: Arc::clone(&layer.tier),
                hidden,
                shared: layer.shared,
                above_base: false,
            });
        }
        Self::new(layers)
    }

    pub(crate) fn layers(&self) -> &[Layer] {
        &self.layers
    }

    /// The value the highest layer that answers for `key` holds.
    // Cost: O(l), l = layers (a hash probe each).
    #[inline]
    pub(crate) fn get(&self, key: &Symbol) -> Option<&Value> {
        self.layers.iter().find_map(|layer| layer.answers(key))
    }

    /// Every visible entry, folded into one tier.
    // Cost: O(1) once built; the build is O(e), e = entries over all layers.
    pub(crate) fn merged(&self) -> &Arc<Tier> {
        self.merged.get_or_init(|| {
            if let [only] = &*self.layers
                && only.hidden.is_none()
            {
                return Arc::clone(&only.tier);
            }
            let mut out = SymMap::default();
            for layer in self.layers.iter().rev() {
                for (k, v) in layer.tier.iter() {
                    if layer.hidden.as_ref().is_some_and(|h| h.contains(k)) {
                        continue;
                    }
                    out.insert(*k, v.clone());
                }
            }
            Arc::new(Tier::new(out))
        })
    }

    pub(crate) fn iter(&self) -> std::collections::hash_map::Iter<'_, Symbol, Value> {
        self.merged().iter()
    }

    pub(crate) fn keys(&self) -> std::collections::hash_map::Keys<'_, Symbol, Value> {
        self.merged().keys()
    }

    /// Upper bound on the visible entries, without folding the layers.
    pub(crate) fn len_upper_bound(&self) -> usize {
        self.layers.iter().map(|l| l.tier.len()).sum()
    }

    /// Upper bound on what a closure capture can keep from this view.
    pub(crate) fn capture_upper_bound(&self) -> usize {
        self.layers
            .iter()
            .map(|l| l.tier.capture_upper_bound())
            .sum()
    }

    /// Every value any layer holds, shadowed or not — for GC root
    /// enumeration, where visiting a shadowed value too is harmless.
    pub(crate) fn all_values(&self) -> impl Iterator<Item = &Value> {
        self.layers.iter().flat_map(|l| l.tier.values())
    }

    /// The keys whose value in some layer is a `ContainerRef` cell, possibly
    /// repeated and possibly shadowed: the caller looks each one up through
    /// [`Self::get`] (see [`Tier::container_ref_keys`]).
    pub(crate) fn container_ref_keys(&self) -> impl Iterator<Item = Symbol> + '_ {
        self.layers
            .iter()
            .flat_map(|l| l.tier.container_ref_keys().iter().copied())
    }
}

impl std::fmt::Debug for CaptureView {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CaptureView")
            .field("layers", &self.layers.len())
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn s(name: &str) -> Symbol {
        Symbol::intern(name)
    }

    fn tier(entries: &[(&str, i64)]) -> Arc<Tier> {
        let mut map = SymMap::default();
        for (k, v) in entries {
            map.insert(s(k), Value::int(*v));
        }
        Arc::new(Tier::new(map))
    }

    #[test]
    fn the_highest_layer_wins_and_hidden_names_fall_through() {
        let hidden: FxHashSet<Symbol> = [s("_")].into_iter().collect();
        let view = CaptureView::new(vec![
            Layer::open(tier(&[("own", 1)])),
            Layer::shared(
                tier(&[("own", 2), ("_", 3), ("Shared", 4)]),
                Some(Arc::new(hidden)),
                false,
            ),
            Layer::open(tier(&[("_", 5)])),
        ]);
        assert_eq!(view.get(&s("own")), Some(&Value::int(1)));
        assert_eq!(view.get(&s("Shared")), Some(&Value::int(4)));
        // Hidden in the middle layer, so the lower layer answers.
        assert_eq!(view.get(&s("_")), Some(&Value::int(5)));
        assert_eq!(view.get(&s("missing")), None);
        let merged = view.merged();
        assert_eq!(merged.get(&s("own")), Some(&Value::int(1)));
        assert_eq!(merged.get(&s("_")), Some(&Value::int(5)));
        assert_eq!(merged.len(), 3);
    }

    #[test]
    fn over_puts_the_own_tier_first() {
        let below = CaptureView::single(tier(&[("a", 1), ("b", 2)]));
        let view = CaptureView::over(tier(&[("a", 9)]), &below, None);
        assert_eq!(view.get(&s("a")), Some(&Value::int(9)));
        assert_eq!(view.get(&s("b")), Some(&Value::int(2)));
        assert_eq!(view.layers().len(), 2);
    }
}
