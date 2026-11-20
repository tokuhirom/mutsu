//! What building a method dispatch frame asks of the receiver class and the
//! method name alone, memoized per `(class, method)` for one registry write
//! generation.
//!
//! `build_method_dispatch_frame` decides whether the call needs a frame with
//! candidates or only the empty boundary. Each override test it makes reads a
//! separate per-name memo (`has_user_method`, its role-including twin, the
//! metamodel-HOW test, the shadowed-accessor owner, the structural candidate
//! count, the deferral expansion), and on AttrX::Mooish's attribute methods
//! those lookups -- each an intern pair and a hash probe -- were most of an
//! ~8.5k-instruction build that ended in the empty boundary anyway. They are
//! all functions of the registry shape, so one record per `(class, method)`
//! answers them together; the receiver-value tests (native storage, a mixin,
//! a type object, a numeric payload) stay per call.

use super::*;

/// See the module doc. Every field is the answer of the probe it names.
#[derive(Clone, Copy, Debug)]
pub(crate) struct MethodFrameFacts {
    /// `has_user_method(class, method)`.
    pub(crate) user_method: bool,
    /// `has_user_method_including_role(class, method)`.
    pub(crate) user_method_including_role: bool,
    /// `is_metamodel_how_class(class)`.
    pub(crate) metamodel_how: bool,
    /// `first_public_accessor_owner(class, method)`: an ancestor's generated
    /// accessor of this name.
    pub(crate) accessor_owner: Option<Symbol>,
    /// `has_multiple_dispatch_candidates(class, method)`.
    pub(crate) multiple_candidates: bool,
    /// The flat deferral expansion holds at most one entry.
    pub(crate) expansion_at_most_one: bool,
    /// The registry write generation the record was built under. The
    /// underlying probes are keyed by narrower counters (the method
    /// generation, the type stamp); every registry write bumps this one, so a
    /// record is never served across any write.
    generation: u64,
}

impl Interpreter {
    /// The [`MethodFrameFacts`] of `(receiver_class, method_name)`.
    // Cost: O(1) amortized: one memo probe per call for a registry write
    // generation; a miss runs each underlying probe once (each itself
    // memoized, O(d) on its own miss, d = MRO depth).
    pub(super) fn method_frame_facts(
        &mut self,
        receiver_class: &str,
        method_name: &str,
    ) -> MethodFrameFacts {
        self.refresh_method_caches_for_generation();
        let key = (Symbol::intern(receiver_class), Symbol::intern(method_name));
        let generation = self.registry_write_generation();
        if let Some(facts) = self.caches.method_frame_facts.get(&key)
            && facts.generation == generation
        {
            return *facts;
        }
        let facts = MethodFrameFacts {
            user_method: self.has_user_method(receiver_class, method_name),
            user_method_including_role: self
                .has_user_method_including_role(receiver_class, method_name),
            metamodel_how: self.is_metamodel_how_class(receiver_class),
            accessor_owner: super::user_method_probe_memo::probe_key(method_name)
                .and_then(|name| self.first_public_accessor_owner(receiver_class, name)),
            multiple_candidates: self.has_multiple_dispatch_candidates(receiver_class, method_name),
            expansion_at_most_one: self
                .deferral_expansion_shared(receiver_class, method_name)
                .len()
                <= 1,
            generation,
        };
        // A probe that wrote the registry (an MRO memo) leaves answers that
        // belong to neither generation: record only an undisturbed build.
        if self.registry_write_generation() == generation {
            self.caches.method_frame_facts.insert(key, facts);
        }
        facts
    }
}
