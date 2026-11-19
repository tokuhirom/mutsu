//! The env writes a method call makes for the class-body statics its class
//! sees, prepared once per class (#17335).
//!
//! Every compiled method call re-seats the `my` statics of its class body
//! (and of the enclosing class bodies) in the frame's env
//! (`Interpreter::inject_class_body_statics`). Rebuilding that list per call
//! cloned every static's `String` name, re-interned it and recomputed the
//! typed and stale lanes: AttrX::Mooish's role body declares four statics
//! (`my class AttrProxy`, `my enum PvtMode`, ...), and its methods run ~20
//! times per lazy-attribute round.
//!
//! The list is a function of the two world tables it reads -- the statics'
//! values (`package_lexicals`) and their recorded declarations
//! (`class_body_static_types`) -- and of the class name, so it is kept per
//! class under both tables' generations. A write to either table, on any
//! thread, moves its generation (`WorldCell::generation`), so a kept list is
//! exactly the one a rebuild would produce.

use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use crate::value::Value;
use std::sync::Arc;

/// The symbols `merge_method_env` drops for a class's statics, by class:
/// the `class_body_static_names` generation each was built under, and the
/// names with their `__mutsu_type::` / `__mutsu_hash_key_type::` lanes.
pub(crate) type MergeDrops = rustc_hash::FxHashMap<Symbol, (u64, Arc<[Symbol]>)>;

/// The prepared lists, by class: the `(package_lexicals,
/// class_body_static_types)` generations each was built under, and the
/// writes in order.
pub(crate) type Plans = rustc_hash::FxHashMap<Symbol, ((u64, u64), Arc<[(Symbol, Value)]>)>;

impl Interpreter {
    /// The env writes for `class_name`'s visible class-body statics, as
    /// [`Interpreter::collect_class_body_statics`] builds them, kept until
    /// either table they read is written.
    // Cost: O(1) expected on a hit (one intern, one probe); O(s) on a miss,
    // s = the statics of the class and its enclosing packages.
    pub(crate) fn class_body_statics_plan(&mut self, class_name: &str) -> Arc<[(Symbol, Value)]> {
        let key = Symbol::intern(class_name);
        let generations = (
            self.lexicals.package_lexicals.generation(),
            self.lexicals.class_body_static_types.generation(),
        );
        if let Some((built, plan)) = self.caches.class_body_statics_plans.get(&key)
            && *built == generations
        {
            return Arc::clone(plan);
        }
        let plan: Arc<[(Symbol, Value)]> = self.collect_class_body_statics(class_name).into();
        self.caches
            .class_body_statics_plans
            .insert(key, (generations, Arc::clone(&plan)));
        plan
    }

    /// The env keys a method of `owner_class` never merges back into its
    /// caller because they are the class's body statics: each name
    /// `class_body_static_names` records for the class, and its
    /// `__mutsu_type::` and `__mutsu_hash_key_type::` lanes -- the keys both
    /// method-exit predicates (`is_method_local`) drop, as symbols. Kept per
    /// class until that table is written.
    // Cost: O(1) expected on a hit; O(n) on a miss, n = the class's statics.
    pub(crate) fn class_body_static_merge_drops(&mut self, owner_class: &str) -> Arc<[Symbol]> {
        let generation = self.lexicals.class_body_static_names.generation();
        let key = Symbol::intern(owner_class);
        if let Some((built, drops)) = self.caches.class_body_static_merge_drops.get(&key)
            && *built == generation
        {
            return Arc::clone(drops);
        }
        let mut drops = Vec::new();
        if let Some(names) = self
            .lexicals
            .class_body_static_names
            .read()
            .get(owner_class)
        {
            for name in names {
                drops.push(Symbol::intern(name));
                drops.push(crate::meta_ns::MetaNs::Type.key(Symbol::intern(name)));
                drops.push(crate::meta_ns::MetaNs::HashKeyType.key(Symbol::intern(name)));
            }
        }
        let drops: Arc<[Symbol]> = drops.into();
        self.caches
            .class_body_static_merge_drops
            .insert(key, (generation, Arc::clone(&drops)));
        drops
    }
}
