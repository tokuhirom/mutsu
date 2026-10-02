//! `OUTER::` across a compilation-frame boundary, past a shadowing declaration
//! (#10827).
//!
//! `sub s { my $x = 3; $OUTER::x = 5 }` names the `$x` of the scope around the
//! sub, but inside the sub the plain name `x` is the sub's own `$x` -- in its
//! slot and in its env overlay alike. So the outer binding needs a key of its
//! own: the declaring frame boxes the binding into a cell right before it
//! creates the closure or sub ([`OpCode::BoxOuterRef`]) and publishes the cell
//! under `__mutsu_outer::<scope>:<name>`, which the closure capture keeps like
//! every other `__mutsu_*` key. The nested body reads through the cell
//! ([`OpCode::GetOuterCapture`]) and writes through it with an ordinary by-name
//! store of that key -- a plain write lands in the container, and a `:=`
//! reseats the binding cell (ADR-0097 §14.1), which the declaring slot shares.

use super::*;

impl Interpreter {
    /// [`OpCode::BoxOuterRef`]: give slot `slot` a shared cell and publish it
    /// under the key constant `key_idx`.
    // Cost: O(1).
    pub(super) fn exec_box_outer_ref_op(
        &mut self,
        code: &CompiledCode,
        slot: u32,
        key_idx: u32,
        rebinds: bool,
        visible: bool,
    ) {
        let idx = slot as usize;
        let Some(cur) = self.locals.get(idx).cloned() else {
            return;
        };
        let key = Self::const_str(code, key_idx);
        let name = code.locals[idx].as_str();
        // An `@`/`%` container takes a container cell exactly like the one a
        // capture gives it at its declaration (ADR-0039,
        // `box_decl_local_container_cell`): a mutating method call
        // (`@OUTER::a.push(1)`) writes its result back by name, and only a
        // cell carries that write to the slot (#10857). A native element type
        // stays bare, as there. The shapes the closure-capture boxing refuses
        // (`box_captured_lexicals`) are shared as they are: a type object other
        // than the `Any` seed, a Proxy, a Seq-family value, a Sub.
        let shareable = if name.starts_with(['@', '%']) {
            let bare = cur.deref_container();
            !matches!(bare.view(), ValueView::Array(..) | ValueView::Hash(..))
                || self
                    .container_element_type_constraint(name)
                    .is_some_and(|t| Self::is_native_element_type(&t))
        } else {
            name.starts_with('&')
                || (!cur.is_any_type_object()
                    && matches!(
                        cur.view(),
                        ValueView::Proxy { .. }
                            | ValueView::Seq(..)
                            | ValueView::HyperSeq(..)
                            | ValueView::RaceSeq(..)
                            | ValueView::Slip(..)
                            | ValueView::Sub(..)
                    ))
        };
        // A rebind on either side -- the nested `$OUTER::x := $y`, or this
        // frame's own `$x := ...` after the capture -- must swap what both see,
        // which takes a binding cell (ADR-0097 §14).
        let rebinds = rebinds || code.rebound_slots.contains(&slot);
        let cell = if shareable {
            cur
        } else if cur.is_container_ref() {
            if rebinds && Self::binding_cell_of(&cur).is_none() {
                Self::wrap_in_binding_cell(cur)
            } else {
                cur
            }
        } else {
            let container = cur.into_container_ref();
            if visible {
                self.register_container_cell_constraint_for_name(&container, name);
            }
            if rebinds {
                Self::wrap_in_binding_cell(container)
            } else {
                container
            }
        };
        if !self.locals[idx].same_binding(&cell) {
            self.locals[idx] = cell.clone();
            // Only the binding the plain name denotes here owns the env entry
            // of that name; a shadowed slot's name belongs to its shadow.
            if visible {
                self.env_mut().insert(name.to_string(), cell.clone());
            }
        }
        // Re-publishing the same cell would still move the env tier (a
        // copy-on-write by-name store), defeating the closure-capture memo.
        let unchanged = self
            .env()
            .get(key)
            .is_some_and(|prev| prev.same_binding(&cell));
        if !unchanged {
            self.env_mut().insert(key.to_string(), cell);
        }
    }

    /// [`OpCode::GetOuterCapture`]: read the published cell, or resolve the
    /// name the way `GetOuterVar` does when no cell was published.
    // Cost: O(1) amortized (one env lookup; the fallback is `get_outer_var`'s).
    pub(super) fn exec_get_outer_capture_op(
        &self,
        code: &CompiledCode,
        key_idx: u32,
        name_idx: u32,
        depth: u32,
    ) -> Value {
        let key = Self::const_str(code, key_idx);
        if let Some(val) = self.env().get(key) {
            return val.clone().into_deref();
        }
        let name = Self::const_str(code, name_idx);
        self.get_outer_var(code, name, depth as usize, None)
    }
}
