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
        let Some(cell_val) = self.try_promote_attr_container(code, name_idx) else {
            return;
        };
        self.stack.pop();
        self.stack.push(cell_val);
    }

    /// [`crate::opcode::OpCode::ResolveAttrRwCandidate`]'s handler: follows the
    /// plain `GetLocal` read of a `$!attr`/`$.attr` positional call argument,
    /// honoring a pending `accessor_ref_pending` marker (see
    /// [`Self::exec_attr_container_ref_op`] /
    /// [`crate::opcode::OpCode::MarkAccessorRefContext`]) instead of always
    /// leaving the plain value in place.
    ///
    /// The flag is consumed (and unconditionally cleared) here, exactly like
    /// `CallMethod`/`CallMethodMut` consume it for an accessor-shaped argument
    /// — this op is the Var-argument twin of that producer/consumer pair, for
    /// the one rw-tail shape `AttrContainerRef` itself cannot reach from a
    /// plain read (#8904): an `is rw`/`is raw` parameter bound to a caller's
    /// `$!attr` argument. When the flag is unset, or the promotion declines
    /// (no instance invocant, absent attribute, aggregate-shaped slot), the
    /// plain value `GetLocal` pushed is left untouched.
    pub(super) fn exec_resolve_attr_rw_candidate_op(&mut self, code: &CompiledCode, name_idx: u32) {
        if std::mem::take(&mut self.accessor_ref_pending)
            && let Some(cell_val) = self.try_promote_attr_container(code, name_idx)
        {
            self.stack.pop();
            self.stack.push(cell_val);
        }
    }

    /// The shared promotion core behind [`Self::exec_attr_container_ref_op`]
    /// and [`Self::exec_resolve_attr_rw_candidate_op`]: promote `self`'s `attr`
    /// (by the constant-pool index of its *bare* name) to its shared
    /// `ContainerRef` cell, or decline (`None`) under the same conditions
    /// `exec_attr_container_ref_op`'s doc comment lists.
    fn try_promote_attr_container(&mut self, code: &CompiledCode, name_idx: u32) -> Option<Value> {
        let attr = Self::const_str(code, name_idx).to_string();
        let base = self.frame_self_value(code)?;
        let ValueView::Instance {
            attributes,
            class_name,
            ..
        } = base.view()
        else {
            return None;
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
                return None;
            }
        };
        let current = attributes.as_map().get(key.as_str()).cloned();
        if matches!(
            current.as_ref().map(|v| v.view()),
            Some(ValueView::Array(..) | ValueView::Hash(_) | ValueView::Mixin(..))
        ) {
            return None;
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
        Some(cell_val)
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

    /// The container a public auto-accessor hands back when its caller asked
    /// for one (`MarkAccessorRefContext`: a `:=` bind RHS, a `.VAR` chain, an
    /// `is rw` routine's tail, or a wrapped accessor's terminal reached from
    /// such a context): `method`'s attribute slot on the instance `target`,
    /// promoted to its shared `ContainerRef` cell.
    ///
    /// This is the one want-ref consumer for an auto-accessor, shared by the
    /// VM fast path (`try_fast_accessor_read`) and the wrap-chain terminal
    /// (`DeferralEntry::Accessor`), so every route to the attribute names the
    /// same cell.
    ///
    /// Declines (`None`) unless `method` is a public `is rw` accessor of a
    /// present, scalar-shaped slot: raku returns the decontainerized value of a
    /// read-only accessor, and an `@`/`%` value is already a shared container
    /// whose type metadata the aggregate accessor path carries.
    // Cost: O(a + m), a = the class's attributes, m = its MRO length (the
    // rw/type-constraint lookup); the slot probe and promotion are O(1).
    pub(crate) fn rw_accessor_container(
        &mut self,
        target: &Value,
        class_name: &str,
        method: &str,
    ) -> Option<Value> {
        let ValueView::Instance { attributes, .. } = target.view() else {
            return None;
        };
        let priv_key = format!("{}!", method);
        let (key, current) = {
            let map = attributes.as_map();
            match map.get(method) {
                Some(v) => (method.to_string(), v.clone()),
                None => (priv_key.clone(), map.get(&priv_key)?.clone()),
            }
        };
        if matches!(
            current.view(),
            ValueView::Array(..) | ValueView::Hash(_) | ValueView::Mixin(..)
        ) {
            return None;
        }
        let type_constraint = self.rw_accessor_type_constraint(class_name, method)?;
        let cell_val = attributes.promote_attr_to_container(key.as_str());
        // A typed rw attribute's constraint travels with the cell, so a later
        // write through the container (`$ref = v`, `f() = v`) type-checks
        // exactly like `$obj.x = v` does.
        if let ValueView::ContainerRef(cell) = cell_val.view()
            && let Some(tc) = type_constraint.as_ref()
            && !matches!(tc.as_str(), "Mu" | "Any")
        {
            crate::value::register_container_constraint(&cell, tc);
        }
        if let Some(msg) = self.class_attribute_deprecated(class_name, method) {
            self.check_deprecation_for_method(method, class_name, &msg);
        }
        Some(cell_val)
    }
}
