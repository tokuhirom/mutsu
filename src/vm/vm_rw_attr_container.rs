//! ADR-0067: an `is rw` method whose tail is a bare private attribute hands
//! back the attribute's *container*.
//!
//! ADR-0059 fixed the rule — "an `is rw` routine returns a container" — and
//! ADR-0067's slices 1-5 made every tail shape but one obey it:
//!
//! ```text
//! method m(\x)   is rw { x }        -> WrapVarRef + CaptureVarCell   (slice 1)
//! method at($i)  is rw { @!l[$i] }  -> container-mode subscript compile
//! method acc     is rw { $!v }      -> GetLocal(<seeded slot>)       <- a VALUE
//! ```
//!
//! The `$!v` tail is different because a method frame does not read the
//! attribute out of the instance: dispatch *seeds a local slot* named `!v` with
//! a copy. Boxing that slot (what `CaptureVarCell` would do) mints a cell that
//! is disconnected from the instance, so writes through it would evaporate.
//! [`crate::opcode::OpCode::AttrContainerRef`] therefore reaches past the slot
//! to `self`'s own attribute cell and promotes *that*, which is the identical
//! `promote_attr_to_container` call `try_fast_accessor_read`'s `want_ref`
//! branch makes for a public accessor. Sharing the promotion (rather than
//! minting a second cell) is what makes `my $x := $c.v` and
//! `my $x := $c.acc` name the same container when `acc` exposes `v`.

use crate::opcode::CompiledCode;
use crate::runtime::Interpreter;
use crate::value::{Value, ValueView};

impl Interpreter {
    /// The instance an `AttrContainerRef` is relative to: this frame's `self`.
    ///
    /// Prefer the frame's own local slot — a method frame always has one — and
    /// fall back to env only for the shapes that run a method body without a
    /// `self` local (an inlined block, a `.wrap`ped body). Returning `None`
    /// leaves the plain value on the stack, which is the pre-ADR behaviour.
    fn frame_self_value(&self, code: &CompiledCode) -> Option<Value> {
        if let Some(slot) = code.locals.iter().position(|n| n == "self") {
            let v = self.locals[slot].clone();
            if !matches!(v.view(), ValueView::Nil) {
                return Some(v);
            }
        }
        self.env().get("self").cloned()
    }

    /// Replace the top-of-stack plain attribute read with the attribute's own
    /// shared `ContainerRef` cell.
    ///
    /// Declines (leaving the value in place) when there is no instance
    /// invocant, when the attribute is absent, or when the slot holds an
    /// aggregate. The aggregate exclusion mirrors `try_fast_accessor_read`'s:
    /// an `@`/`%`-shaped value is already a shared container whose identity the
    /// accessor path carries type metadata for, and wrapping it in a scalar
    /// cell would disagree with that storage.
    pub(super) fn exec_attr_container_ref_op(&mut self, code: &CompiledCode, name_idx: u32) {
        let attr = Self::const_str(code, name_idx).to_string();
        let Some(base) = self.frame_self_value(code) else {
            return;
        };
        let ValueView::Instance {
            attributes,
            class_name,
            ..
        } = base.view()
        else {
            return;
        };
        // Public `has $.v` stores under `v`; private-only `has $!v` under `v!`.
        let priv_key = format!("{}!", attr);
        let key = {
            let map = attributes.as_map();
            if map.get(attr.as_str()).is_some() {
                attr.clone()
            } else if map.get(priv_key.as_str()).is_some() {
                priv_key
            } else {
                return;
            }
        };
        let current = attributes.as_map().get(key.as_str()).cloned();
        if matches!(
            current.as_ref().map(|v| v.view()),
            Some(ValueView::Array(..) | ValueView::Hash(_) | ValueView::Mixin(..))
        ) {
            return;
        }
        let cn = class_name.resolve();
        let cell_val = attributes.promote_attr_to_container(key.as_str());
        // A typed attribute's constraint travels with the cell, so a write
        // through the returned container type-checks exactly like `$obj.v = x`.
        if let ValueView::ContainerRef(cell) = cell_val.view()
            && let Some(tc) = self.attr_container_type_constraint(&cn, &attr)
            && !matches!(tc.as_str(), "Mu" | "Any")
        {
            crate::value::register_container_constraint(&cell, &tc);
        }
        self.stack.pop();
        self.stack.push(cell_val);
    }

    /// The declared type of attribute `attr` on `class_name`, searched up the
    /// MRO. Unlike `rw_accessor_type_constraint` this does not require a public
    /// `is rw` accessor: an `is rw` *method* may expose a private-only
    /// attribute, and its constraint must still travel with the container.
    fn attr_container_type_constraint(&mut self, class_name: &str, attr: &str) -> Option<String> {
        for cn in self.class_mro(class_name).iter() {
            if let Some(tc) = self.get_attr_type_constraint(cn.as_str(), attr) {
                return Some(tc);
            }
        }
        None
    }
}
