//! The method-table lane for a receiver with no dispatch shape (#17331).
//!
//! [`vm_method_site_lane`] answers a built-in
//! method on a receiver of a built-in shape from the method table, past the
//! full `CallMethodMut` path's probes. A type object or an instance of a user
//! class, a role type object and `Any`/`Mu` themselves have no shape, so their
//! `.WHICH` ran the whole probe chain (~30 lanes, each answering "no") before
//! `answer_shapeless` found `Mu.WHICH`: ~16k instructions, against ~1.5k for
//! the same row on an `Int`. Monad builds its type-cache keys from
//! `M.WHICH ~ '|' ~ T.WHICH` on every constructor call.
//!
//! # The guard
//!
//! The lane takes every guard of the shaped lane (it runs after them), and:
//!
//! - the call has no arguments and names one of [`SHAPELESS_LANE_METHODS`],
//!   the identity and definedness methods every object inherits from `Mu`;
//! - the receiver is a plain type object (no `but` mixin) or a plain instance
//!   of a class (not a `Failure`, whose definedness handles it, nor a lazy
//!   `Match`);
//! - the receiver's type does not declare or inherit a user method of that
//!   name (`user_shadows_builtin`, the probe the full path asks), nor has
//!   user code `augment`ed it;
//! - `answer_shapeless`'s row for the call is a pure one.
//!
//! # The memo
//!
//! Everything but the first two checks depends only on the receiver's type,
//! the method and the registry, so it is remembered per
//! `(type, type-object bit, method)` for one registry write generation in the
//! `caches` subsystem (per interpreter, so per thread: no lock). A remembered
//! miss sends later calls straight to the full path.
//!
//! In debug builds the answer is cross-checked against the full path exactly
//! as the shaped lane's is (`check_method_site_lane`).

use super::*;
use crate::builtins::method_table;

/// The `Mu` methods the lane answers for a receiver with no shape.
const SHAPELESS_LANE_METHODS: [&str; 3] = ["WHICH", "defined", "DEFINITE"];

impl Interpreter {
    /// The row that answers the zero-argument `CallMethodMut` whose receiver
    /// sits at `self.stack[base]` and has no dispatch shape, or `None` to run
    /// the full path. The caller has applied the shaped lane's guards.
    // Cost: O(1) on a memo hit (one hash probe); a miss adds the memoized
    // user-method probe and one table lookup.
    pub(super) fn shapeless_site_lane_row(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        target_name_idx: u32,
        base: usize,
    ) -> Option<method_table::RowId> {
        let method = code.const_sym(name_idx);
        if !SHAPELESS_LANE_METHODS.contains(&method.as_str()) {
            return None;
        }
        let (type_sym, type_object) = match self.stack[base].view() {
            ValueView::Package(name) => {
                if name.with_str(|s| s.contains("+{")) {
                    return None;
                }
                (name, true)
            }
            ValueView::Instance { class_name, .. } => {
                if class_name == "Failure" || self.stack[base].is_lazy_match_value() {
                    return None;
                }
                (class_name, false)
            }
            _ => return None,
        };
        if Self::is_array_hash_attr_twigil(Self::const_str(code, target_name_idx)) {
            return None;
        }
        let generation = self.registry_write_generation();
        let key = (type_sym, type_object, method);
        if let Some(&(memo_gen, row)) = self.caches.shapeless_site_lane.get(&key)
            && memo_gen == generation
        {
            return row;
        }
        let target = self.stack[base].clone();
        let row = if self.user_shadows_builtin(&target, method, &[], false)
            || self.native_lever_a_user_override_sym(&target, method)
        {
            None
        } else {
            method_table::shapeless_row(Some(self), &target, method, 0)
                .filter(|&row| method_table::row(row).handler.is_pure())
        };
        if self.dispatch.native_base_bypass.is_none() {
            self.caches
                .shapeless_site_lane
                .insert(key, (generation, row));
        }
        row
    }
}
