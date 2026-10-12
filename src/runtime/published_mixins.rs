//! Process-wide publication of runtime-created mixin types (#11456,
//! ADR-11456).
//!
//! Every thread runs on its own interpreter, whose declaration registry is a
//! copy-on-write snapshot of its parent's (`TypeState::fork_for_thread`). A
//! type created at run time on one thread therefore lands in that thread's
//! registry only. For most run-time types that is invisible, but `$obj does R`
//! reblesses the *shared* object into `C+{R}` in place
//! (`Interpreter::does_rebless_instance`): every other thread holding the
//! object now sees a class name its own registry has never heard of, and
//! smartmatch, method dispatch and the MOP all answer as if the type had no
//! roles and no methods.
//!
//! A mixin type is fully described by its recipe — the base class and the
//! roles composed onto it — and composing the same recipe from the same
//! declarations yields the same type. So the thread that creates one
//! publishes the recipe here, and a thread that meets the name without a
//! registry entry composes it into its own registry from the recipe
//! (`Interpreter::adopt_published_mixin`).

use std::collections::HashMap;
use std::sync::RwLock;
use std::sync::atomic::{AtomicBool, Ordering};

/// What a mixin type is made of: `base+{roles}`.
#[derive(Clone, Debug)]
pub(crate) struct MixinRecipe {
    pub(crate) base: String,
    pub(crate) roles: Vec<String>,
    /// The composed type's role-body lexicals (its `package_lexicals`
    /// entry) as the creating thread left them. A scalar is a shared cell
    /// (`share_body_static_scalar`), so an adopting thread binds the same
    /// containers instead of running the role bodies a second time (#12825).
    pub(crate) body_lexicals: Vec<(String, crate::value::Value)>,
}

/// The recipes of every mixin type created in this process, keyed by the
/// type's name. One instance is shared (not snapshotted) by an interpreter and
/// every thread forked from it.
#[derive(Default, Debug)]
pub(crate) struct PublishedMixins {
    /// Set once the first recipe is published, so the miss-path probe of a
    /// program that never mixes in costs one relaxed load.
    any: AtomicBool,
    recipes: RwLock<HashMap<String, MixinRecipe>>,
}

impl PublishedMixins {
    /// Record `name`'s recipe. The first publication of a name wins; a later
    /// one (another thread composing the same recipe) describes the same type.
    // Cost: O(n) expected, n = name.len() (hash + one write-locked insert).
    pub(crate) fn publish(&self, name: &str, recipe: MixinRecipe) {
        let mut recipes = self.recipes.write().unwrap_or_else(|e| e.into_inner());
        recipes.entry(name.to_string()).or_insert(recipe);
        self.any.store(true, Ordering::Release);
    }

    /// Whether `name` may have a published recipe: something was published
    /// and `name` has the shape of a mixin type name.
    // Cost: O(1).
    #[inline]
    pub(crate) fn may_hold(&self, name: &str) -> bool {
        self.any.load(Ordering::Acquire) && name.ends_with('}')
    }

    /// The recipe of the mixin type `name`, when one was published.
    // Cost: O(1) when `may_hold` is false; otherwise O(n) expected,
    // n = name.len() (hash + read-locked probe).
    pub(crate) fn lookup(&self, name: &str) -> Option<MixinRecipe> {
        if !self.may_hold(name) {
            return None;
        }
        let recipes = self.recipes.read().unwrap_or_else(|e| e.into_inner());
        recipes.get(name).cloned()
    }
}
