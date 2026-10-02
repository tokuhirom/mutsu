//! ADR-0024's textual-order edge (mutsu#9911): a named sub called *before*
//! the declaration of a variable it reads.
//!
//! ```raku
//! my $c = "outer";
//! { my $c = "inner"; say f() }   # raku: outer
//! sub f { $c }
//! ```
//!
//! The hoisted `RegisterDecl` of `f` runs at the top of the unit, before
//! `my $c = "outer"` has run, so the declaration slot it resolved `$c` to
//! (`free_var_decl_slots`) still holds Nil and there is nothing to box. The
//! in-sequence registration that boxes the live value runs only after the
//! block, so the call inside the block used to fall back to by-name
//! resolution and read the block's shadowing `my $c`.
//!
//! The hoisted pass now seeds a fresh cell (holding `Any`, or an empty
//! Array/Hash) in the declaration slot and in the unit store, and records it
//! here — for a free variable the compiler found declared in the sub's own
//! scope (`hoist_seed_slots`), whose declaration therefore runs after the
//! hoist on every entry of that scope. The declaration's binding reset
//! ([`Interpreter::reset_hoist_pending_cell`]) and its store
//! ([`Interpreter::adopt_hoist_pending_cell`]) then go THROUGH that cell
//! instead of replacing the slot, so the sub, the declaring frame and the
//! store share one container from the hoist on. A call that runs before the
//! declaration reads `Any`, as in raku.

use crate::gc::Gc;
use crate::runtime::Interpreter;
use crate::value::{ContainerCell, Value, ValueView};

/// One cell seeded by a hoisted registration, waiting for its declaration.
#[derive(Clone)]
pub(crate) struct HoistPendingCell {
    cell: Gc<ContainerCell>,
    /// The `unit_lexicals` bucket the cell was stored under.
    unit_key: String,
    /// The variable name (a scalar's without its sigil), the bucket's key.
    name: String,
}

impl Interpreter {
    /// Seed the cell for a hoisted sub's free variable `name` whose
    /// declaration in slot `slot` has not run yet in this entry of its scope:
    /// a fresh container (`Any`, or an empty `@`/`%`) the declaration later
    /// adopts. Returns the cell value the caller stores under `unit_key`, or
    /// `None` for a `&` code variable, which keeps the in-sequence capture.
    // Cost: O(p), p = pending hoist cells (bounded by distinct names).
    pub(super) fn seed_hoist_capture_cell(
        &mut self,
        slot: usize,
        name: &str,
        unit_key: &str,
    ) -> Option<Value> {
        // Scalar names carry no sigil here (`c` for `$c`).
        if name.starts_with('&') {
            return None;
        }
        let init = if name.starts_with('@') {
            Value::real_array(Vec::new())
        } else if name.starts_with('%') {
            Value::hash(crate::value::ValueMap::default())
        } else {
            Value::package(crate::symbol::wk::any())
        };
        let boxed = init.into_container_ref();
        let ValueView::ContainerRef(cell) = boxed.view() else {
            return None;
        };
        self.locals[slot] = boxed.clone();
        self.env_mut().insert(name.to_string(), boxed.clone());
        // A re-entered scope (a loop body) seeds again before an earlier
        // seed's declaration ran (`my $x;` without an initializer never
        // adopts): the new cell supersedes it, which keeps this list bounded
        // by the distinct names awaiting a declaration.
        self.hoist_pending_cells
            .retain(|p| !(p.name == name && p.unit_key == unit_key));
        self.hoist_pending_cells.push(HoistPendingCell {
            cell: (*cell).clone(),
            unit_key: unit_key.to_string(),
            name: name.to_string(),
        });
        Some(boxed)
    }

    /// Before a declaration's store into `slot`: take the pending hoist cell
    /// the slot still holds, if any.
    // Cost: O(p), p = pending hoist cells (sub calls that precede a free
    // variable's declaration; 0 in ordinary code, which never gets here).
    pub(super) fn take_hoist_pending_cell(&mut self, slot: usize) -> Option<HoistPendingCell> {
        let ValueView::ContainerRef(cur) = self.locals.get(slot)?.view() else {
            return None;
        };
        let pos = self
            .hoist_pending_cells
            .iter()
            .position(|p| Gc::ptr_eq(&p.cell, &cur))?;
        Some(self.hoist_pending_cells.swap_remove(pos))
    }

    /// A declaration's binding reset (`SetVarDynamic`) of `slot`: when the
    /// slot holds a pending hoist cell, reset the cell's content instead of
    /// replacing it, and report that the cell was kept.
    // Cost: O(p), p = pending hoist cells.
    pub(super) fn reset_hoist_pending_cell(
        &mut self,
        slot: usize,
        name: &str,
        default: &Value,
    ) -> bool {
        let Some(ValueView::ContainerRef(cur)) = self.locals.get(slot).map(Value::view) else {
            return false;
        };
        let Some(pending) = self
            .hoist_pending_cells
            .iter()
            .find(|p| Gc::ptr_eq(&p.cell, &cur))
        else {
            return false;
        };
        Value::store_through_cell(&pending.cell, default);
        let boxed = Value::container_ref(pending.cell.clone());
        self.env_mut().insert(name.to_string(), boxed);
        true
    }

    /// After the declaration's store into `slot`: put its value into the
    /// pending cell and the cell back into the slot, so the hoisted sub keeps
    /// reading the declaration's own container.
    // Cost: O(1).
    pub(super) fn adopt_hoist_pending_cell(&mut self, slot: usize, pending: HoistPendingCell) {
        let new = self.locals[slot].clone();
        if let ValueView::ContainerRef(c) = new.view() {
            if !Gc::ptr_eq(&c, &pending.cell) {
                // The store already boxed the variable itself: the store entry
                // follows that cell.
                self.set_hoist_store_entry(&pending, Some(new));
            }
            return;
        }
        // A variable with a non-boxable type constraint must stay unboxed (see
        // `type_constrained_unboxable`); drop the seeded entry so the sub keeps
        // the in-sequence capture's (skip) behavior rather than a stale `Any`.
        if self.type_constrained_unboxable(&pending.name) {
            self.set_hoist_store_entry(&pending, None);
            return;
        }
        Value::store_through_cell(&pending.cell, &new);
        let boxed = Value::container_ref(pending.cell.clone());
        self.locals[slot] = boxed.clone();
        self.env_mut().insert(pending.name, boxed);
    }

    fn set_hoist_store_entry(&mut self, pending: &HoistPendingCell, value: Option<Value>) {
        let bucket = self.unit_lexicals_cow_mut().get_mut(&pending.unit_key);
        if let Some(bucket) = bucket {
            match value {
                Some(v) => {
                    bucket.insert(pending.name.clone(), v);
                }
                None => {
                    bucket.remove(&pending.name);
                }
            }
        }
    }
}
