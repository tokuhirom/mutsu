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
}
