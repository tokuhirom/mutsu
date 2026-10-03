//! Assignment to a sigilless name bound to a mutable `Array` / `Hash`.
//!
//! `my \v = @a; v = Empty` assigns *into* the Array: a sigilless name holds no
//! container of its own, so rakudo's assignment falls back to the bound
//! object's `STORE`, and `Array.STORE` / `Hash.STORE` replace the elements in
//! place. Every other holder of that Array (`@a`, a package stash entry, a
//! `for OUR::.kv -> \k, \v { v = Empty }` loop variable, as P5reset's `reset`
//! does) sees the new contents. mutsu used to reject it with "Cannot modify an
//! immutable Array".
//!
//! The compiler emits `SigillessAggregateStore` ahead of the store of every
//! assignment whose target is a source-level sigilless name (never ahead of
//! the synthetic per-iteration binds of a `for` loop's parameters, which must
//! re-seat the name rather than write into the previous iteration's value).
//! When the name holds such an aggregate, the op stores into it in place and
//! leaves the aggregate for the ordinary store, which then re-seats the same
//! object. `CheckReadOnly` lets the assignment through for a read-only
//! sigilless binding the same way it does for an object with a user `STORE`
//! (#9551).

use super::*;

impl Interpreter {
    /// Whether `value` is an aggregate a sigilless name can assign into: a
    /// real `Array` (not a `List`, which rakudo refuses as immutable) or a
    /// `Hash` that is not a `Map`. An itemized one is the content of a Scalar
    /// (`my $h = {...}; f($h)` binding `\c`), so the assignment replaces the
    /// Scalar's value instead.
    // Cost: O(1).
    pub(super) fn is_sigilless_assignable_aggregate(value: &Value) -> bool {
        match value.view() {
            ValueView::Array(_, kind) => {
                !kind.is_itemized()
                    && (kind.is_real_array() || kind == crate::value::ArrayKind::Shaped)
            }
            ValueView::Hash(_) => !value.hash_is_itemized() && !value.is_immutable_map(),
            _ => false,
        }
    }

    /// The value `Nil` resets to when assigned through a sigilless name bound
    /// to a shared Scalar cell (`for ($z,) -> \q { q = Nil }`): the cell's own
    /// `is default`, else its `of` type object, else `Any`. `None` when the
    /// name is not bound to a cell; an alias reached by name decays through
    /// `sigilless_alias_nil_decay` instead.
    // Cost: O(1).
    fn sigilless_nil_reset_value(&mut self, name: Symbol, bound: Option<&Value>) -> Option<Value> {
        if self
            .env()
            .get_sym(crate::runtime::sigilless_alias_key(&name.resolve()))
            .is_some_and(|v| matches!(v.view(), ValueView::Str(_)))
        {
            return None;
        }
        let Some(ValueView::ContainerRef(cell)) = bound.map(Value::view) else {
            return None;
        };
        if let Some(def) = cell.default_value() {
            return Some(def);
        }
        Some(match crate::value::lookup_container_constraint(&cell) {
            Some(tc) if tc != "Mu" && tc != "Nil" => {
                let nominal = loan_env!(self, nominal_type_object_name_for_constraint(&tc));
                Value::package(Symbol::intern(&nominal))
            }
            _ => Value::package(crate::symbol::wk::any()),
        })
    }

    /// `OpCode::SigillessAggregateStore`: when the sigilless name (local
    /// `slot`, or `env` when `slot` is `u32::MAX`) is bound to a mutable
    /// Array/Hash, replace the right-hand side on the stack with that
    /// aggregate after storing the right-hand side into it.
    // Cost: O(n), n = elements of the right-hand side when the name holds a
    // mutable aggregate; otherwise O(1) (one slot read or env probe).
    pub(super) fn exec_sigilless_aggregate_store_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        slot: u32,
    ) {
        let bound = if slot == u32::MAX {
            self.env().get_sym(code.const_sym(name_idx)).cloned()
        } else {
            self.locals.get(slot as usize).cloned()
        };
        if self.stack.last().is_some_and(Value::is_nil) {
            if let Some(reset) =
                self.sigilless_nil_reset_value(code.const_sym(name_idx), bound.as_ref())
            {
                self.stack.pop();
                self.stack.push(reset);
                return;
            }
        }
        let Some(aggregate) = bound.map(|v| v.deref_container()) else {
            return;
        };
        if !Self::is_sigilless_assignable_aggregate(&aggregate) {
            return;
        }
        let rhs = self.stack.pop().unwrap_or(Value::NIL);
        match self.store_into_aggregate_lvalue(&aggregate, rhs.clone()) {
            Some(stored) => self.stack.push(stored),
            None => self.stack.push(rhs),
        }
    }
}
