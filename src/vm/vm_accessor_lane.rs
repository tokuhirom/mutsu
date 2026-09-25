//! ADR-0121 D3: the generated-accessor lane of `CallMethodMut`.
//!
//! `$obj.x` on a variable compiles to `CallMethodMut`, which walks a long
//! chain of probes (proto bodies, exception delegates, lazy lists, Failure,
//! NativeCall, the plain-method lane, ...) before `try_fast_accessor_read`
//! decides the call is the generated accessor and reads the attribute by
//! name. For an instance laid out by a [`crate::value::ClassLayout`] that
//! outcome is a fixed slot of the layout, so the lane remembers
//! `(layout id, method name) -> slot` and the next call on an instance of
//! the same layout reads the slot before any of the chain runs.
//!
//! # Why the memo only replays what the full path did
//!
//! It is written from exactly one place: right after the full path's
//! `try_fast_accessor_read` answered the call, so every probe ahead of it has
//! just declined. What those probes read has to be pinned for the replay:
//!
//! * **the call shape** -- no arguments, no `.^`/`.!` modifier, not quoted, no
//!   accessor-ref marker -- is required on both sides;
//! * **the registry** (methods, wraps, accessor visibility, MRO) is pinned by
//!   `Registry::method_generation`: the lane is cleared with the other method
//!   caches in `refresh_method_caches_for_generation`;
//! * **the class shape** is pinned by the layout id, which is new for every
//!   layout ever built;
//! * **per-instance state** a probe could read is ruled out on every hit: the
//!   instance must have no undeclared attribute (an exception's thrown
//!   message, a builtin base's attribute), the slot must be present, and its
//!   value must be a scalar (an `@.x` / `%.x` read attaches container
//!   metadata); and on the install side the class families the plain-method
//!   lane refuses (not declared by the program, CStruct, `IO::Handle` /
//!   `IO::Path`) and a deprecated attribute (whose read warns) never enter.
//!
//! Anything else falls through to the full path unchanged.

use super::*;

impl Interpreter {
    /// The attribute value a lane hit answers for the receiver on top of the
    /// stack, or `None` to run the full dispatch.
    // Cost: O(1): a generation compare, one probe of the lane, and one slot
    // read under the instance's read guard.
    pub(super) fn try_accessor_lane(&mut self, method_sym: crate::symbol::Symbol) -> Option<Value> {
        if self.accessor_lane.is_empty() {
            return None;
        }
        self.refresh_method_caches_for_generation();
        let ValueView::Instance { attributes, .. } = self.stack.last()?.view() else {
            return None;
        };
        let map = attributes.as_map();
        if map.has_undeclared() {
            return None;
        }
        let layout_id = map.layout()?.id();
        let slot = *self.accessor_lane.get(&(layout_id, method_sym))?;
        let value = map.slot(slot as usize)?.deref_container();
        if matches!(value.view(), ValueView::Array(..) | ValueView::Hash(_)) {
            return None;
        }
        Some(value)
    }

    /// Record that the full `CallMethodMut` path just answered `method_sym` on
    /// `target` with the generated accessor's plain read, when that answer is
    /// one the lane may replay for every instance of `target`'s layout.
    ///
    /// Runs once per `(layout, method)` pair and generation, never on a hit.
    pub(super) fn note_accessor_lane(&mut self, target: &Value, method_sym: crate::symbol::Symbol) {
        let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = target.view()
        else {
            return;
        };
        let method = method_sym.as_str();
        // The key `try_fast_accessor_read` picked: the public name, else the
        // `!`-suffixed private storage name.
        let resolved = {
            let map = attributes.as_map();
            if map.has_undeclared() {
                return;
            }
            let Some(layout) = map.layout() else {
                return;
            };
            let key = if map.contains_key(method_sym) {
                method_sym
            } else {
                crate::symbol::Symbol::intern(&format!("{method}!"))
            };
            // A declared-but-absent public slot would outrank the private key
            // once filled, so that choice cannot be replayed.
            if key != method_sym && layout.slot_of(method_sym).is_some() {
                return;
            }
            layout.slot_of(key).and_then(|slot| {
                let value = map.slot(slot)?.deref_container();
                let scalar = !matches!(value.view(), ValueView::Array(..) | ValueView::Hash(_));
                (scalar && slot <= u32::MAX as usize).then_some((layout.id(), slot as u32))
            })
        };
        let Some((layout_id, slot)) = resolved else {
            return;
        };
        if !self.plain_method_lane_class_eligible(class_name) {
            return;
        }
        if self
            .class_attribute_deprecated(&class_name.resolve(), method)
            .is_some()
        {
            return;
        }
        self.refresh_method_caches_for_generation();
        self.accessor_lane.insert((layout_id, method_sym), slot);
    }
}
