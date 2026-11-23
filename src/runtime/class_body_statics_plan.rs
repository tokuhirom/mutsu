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

/// One class's prepared writes: the list in order, and the same writes
/// pre-applied to an overlay tier (see [`StaticsPlan::tier`]).
#[derive(Clone)]
pub(crate) struct StaticsPlan {
    pub(crate) writes: Arc<[(Symbol, Value)]>,
    /// The class's own statics, typed lanes and stale lanes only, without
    /// the enclosing class bodies' (`collect_own_class_body_statics`): what
    /// a class-scoped routine call injects and later restores
    /// (`inject_class_body_statics_saving_except`).
    pub(crate) own: Arc<[(Symbol, Value)]>,
    /// The writes applied, in order, to an empty tier: what a frame overlay
    /// that has had no write yet holds after the injection. A frame adopts it
    /// as its overlay (`Env::adopt_fresh_overlay`) instead of inserting each
    /// write; its first own write copies it on write. `None` when a write's
    /// key has an insert side effect a pre-built tier would skip (`?FILE`,
    /// `&return`).
    pub(crate) tier: Option<Arc<crate::env::Tier>>,
}

/// The prepared plans, by class, with the `(package_lexicals,
/// class_body_static_types)` generations each was built under.
pub(crate) type Plans = rustc_hash::FxHashMap<Symbol, ((u64, u64), StaticsPlan)>;

impl Interpreter {
    /// The env writes for `class_name`'s visible class-body statics, as
    /// [`Interpreter::collect_class_body_statics`] builds them, kept until
    /// either table they read is written.
    // Cost: O(1) expected on a hit (one intern, one probe, two `Arc`
    // clones); O(s) on a miss, s = the statics of the class and its
    // enclosing packages.
    pub(crate) fn class_body_statics_plan(&mut self, class_name: &str) -> StaticsPlan {
        self.class_body_statics_plan_sym(Symbol::intern(class_name))
    }

    /// [`Self::class_body_statics_plan`] keyed by the class name's symbol.
    // Cost: as `class_body_statics_plan`, without the intern.
    pub(crate) fn class_body_statics_plan_sym(&mut self, key: Symbol) -> StaticsPlan {
        let class_name = key.as_str();
        let generations = (
            self.lexicals.package_lexicals.generation(),
            self.lexicals.class_body_static_types.generation(),
        );
        if let Some((built, plan)) = self.caches.class_body_statics_plans.get(&key)
            && *built == generations
        {
            return plan.clone();
        }
        let writes: Arc<[(Symbol, Value)]> = self.collect_class_body_statics(class_name).into();
        let mut own = Vec::new();
        self.collect_own_class_body_statics(class_name, &mut own);
        let plan = StaticsPlan {
            tier: crate::env::Env::seed_tier_for_writes(&writes),
            writes,
            own: own.into(),
        };
        self.caches
            .class_body_statics_plans
            .insert(key, (generations, plan.clone()));
        plan
    }
}
