//! `nqp::getattr` / `nqp::bindattr` of a class HOW's `@!BUILDPLAN` and
//! `@!BUILDALLPLAN` (ADR-14162 §2).
//!
//! A read answers the plan the MOP stored for the class, else a derived one.
//! A bind stores the list as the class's replacement plan; construction then
//! runs it (`buildplan_run.rs`).

use super::*;

impl Interpreter {
    /// Serve `name` on `obj` when it is a class HOW's BUILDPLAN attribute:
    /// read it, or bind `bind` to it. `None` declines anything else.
    // Cost: O(1) to decline; a read costs `buildplan_of`/`buildallplan_of`.
    pub(crate) fn nqp_buildplan_attr(
        &mut self,
        obj: &Value,
        name: &str,
        bind: Option<&Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        let all = match name {
            "@!BUILDPLAN" => false,
            "@!BUILDALLPLAN" => true,
            _ => return None,
        };
        let class = Self::how_target_from_value(obj)?;
        let Some(plan) = bind else {
            return Some(Ok(if all {
                self.buildallplan_of(&class)
            } else {
                self.buildplan_of(&class)
            }));
        };
        let mut registry = self.registry_mut();
        let state = registry.buildplans.entry(class).or_default();
        if all {
            state.all_plan = Some(plan.clone());
            state.all_plan_bound = true;
        } else {
            state.plan = Some(plan.clone());
            // A BUILDALLPLAN `create_BUILDPLAN` derived from the old plan is
            // stale now; one a module bound is the module's to keep.
            if !state.all_plan_bound {
                state.all_plan = None;
            }
        }
        state.replaced = true;
        Some(Ok(plan.clone()))
    }
}
