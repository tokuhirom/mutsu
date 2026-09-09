//! Memoized "does an instance of this class need the interpreter's Numeric
//! bridge?" probe (#7712).
//!
//! `try_native_method_raw` asks this of **every** instance receiver before it
//! will decline a method, and the question is three type-graph walks deep:
//! `Real`, then `Numeric` (each walking the class's MRO *and* the transitive
//! closure of its composed roles), then a `Bridge` method lookup. On a
//! `Buf.push` loop those two `type_matches_value` walks alone were 52% of the
//! whole program.
//!
//! The answer is a property of the **class**, not of the call: given a fixed
//! registry, "does an instance of `C` match `Real`/`Numeric`, or have a
//! `Bridge` method" cannot vary between two calls with the same receiver
//! class. So it memoizes per class symbol — the only real question is
//! invalidation.
//!
//! **The choke point already exists.** The answer is derived from
//! `registry.classes` (parents + MRO), `registry.class_composed_roles`,
//! `registry.role_parents` and `registry.subsets`, none of which
//! `Registry::method_generation` covers. But every one of those maps lives
//! behind `Interpreter::registry_mut()` — the sole write path to the shared
//! `Arc<RwLock<Arc<Registry>>>` — and that accessor already bumps
//! `registry_write_gen` on *acquisition*. Keying the cache on
//! [`Interpreter::registry_write_generation`] is therefore sound *by
//! construction* rather than under an enumeration of mutation sites: a
//! registry write that does not bump it is not expressible, so a mutation site
//! added later is covered with no edit here. It over-invalidates (a
//! method-table write drops type-relation answers too), which costs one rewalk
//! per class per registry write and never a wrong answer.
//!
//! Two things the generation deliberately does not cover, handled explicitly:
//!
//! * A user `subset Real`/`subset Numeric` shadowing the builtin name makes the
//!   probe run a `where` predicate — user code, whose side effects roast counts
//!   (`S12-subset/subtypes.t`) and whose answer may differ per call. The cache
//!   switches itself off entirely while either name is shadowed.
//! * "The answer depends only on the class" is a property of
//!   `type_matches_value`'s instance arms, not of the registry. Debug builds
//!   therefore re-derive the answer on every cache hit and assert it matches,
//!   so a future arm that reads the *instance* fails loudly instead of
//!   silently serving a stale answer. `make test` runs `t/` against the
//!   *release* binary, so the tripwire costs CI nothing there; run the suite
//!   against `target/debug/mutsu` to exercise it
//!   (`MUTSU_BIN=target/debug/mutsu prove -e scripts/run-t-test.sh t/`).

use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use crate::value::Value;
use std::collections::HashMap;

/// Per-class memo of the native-dispatch numeric-bridge probe. See the module
/// docs for why keying on the registry write generation is sound.
#[derive(Default, Clone)]
pub(crate) struct NumericBridgeProbeCache {
    /// class symbol -> probe answer, valid only at [`Self::generation`].
    answers: HashMap<Symbol, bool>,
    /// The `registry_write_gen` `answers` was built under; `None` until the
    /// first probe.
    generation: Option<u64>,
    /// `Real` or `Numeric` is shadowed by a user `subset` at
    /// [`Self::generation`] — the probe runs user code, so nothing is cached.
    subset_shadowed: bool,
}

impl Interpreter {
    /// Whether an instance of `class_name` must decline the pure-native fast
    /// path in favour of the interpreter's Numeric bridge: it matches `Real`
    /// or `Numeric`, or its class provides a `Bridge` method.
    ///
    /// `target` must be the receiver whose `ValueView::Instance` supplied
    /// `class_name`; it is what the underlying `type_matches_value` walks (and,
    /// in debug builds, what the cache is verified against).
    pub(crate) fn instance_needs_numeric_bridge(
        &mut self,
        class_name: Symbol,
        target: &Value,
    ) -> bool {
        let generation = self.registry_write_generation();
        if self.numeric_bridge_probe.generation != Some(generation) {
            self.numeric_bridge_probe.answers.clear();
            self.numeric_bridge_probe.generation = Some(generation);
            self.numeric_bridge_probe.subset_shadowed = {
                let registry = self.registry();
                registry.subsets.contains_key("Real") || registry.subsets.contains_key("Numeric")
            };
        }
        if self.numeric_bridge_probe.subset_shadowed {
            return self.compute_numeric_bridge_probe(class_name, target);
        }
        let cached = self.numeric_bridge_probe.answers.get(&class_name).copied();
        if let Some(cached) = cached {
            #[cfg(debug_assertions)]
            {
                let fresh = self.compute_numeric_bridge_probe(class_name, target);
                assert_eq!(
                    cached,
                    fresh,
                    "numeric-bridge probe is not a function of the receiver class alone: \
                     class `{}` cached {cached} but re-derived {fresh} with no registry write \
                     in between. A `type_matches_value` arm now reads the instance; the memo in \
                     `numeric_bridge_probe.rs` must gate on that shape (see #7712).",
                    class_name.as_str(),
                );
            }
            return cached;
        }
        let answer = self.compute_numeric_bridge_probe(class_name, target);
        // The walk itself may have taken a registry write guard (an MRO cache
        // fill goes through `registry_mut()`), which bumps the generation the
        // entry would be filed under. Publish only when it did not, so an entry
        // is never keyed to a generation it was not computed at.
        if self.registry_write_generation() == generation {
            self.numeric_bridge_probe.answers.insert(class_name, answer);
        }
        answer
    }

    /// The un-memoized probe: the three questions `try_native_method_raw` used
    /// to ask inline.
    fn compute_numeric_bridge_probe(&mut self, class_name: Symbol, target: &Value) -> bool {
        self.type_matches_value("Real", target)
            || self.type_matches_value("Numeric", target)
            || self.has_user_method(class_name.as_str(), "Bridge")
    }
}
