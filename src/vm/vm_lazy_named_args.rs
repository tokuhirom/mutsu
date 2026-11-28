//! Resolving the lazy `:$var` arguments of a `CallMethodMut` site (#19384).
//!
//! A named argument naming a caller variable must hand its callee the
//! variable's *container*: an `is rw` or raw parameter, a `|c` capture and a
//! `*%h` slurpy all write through to it (rakudo: `sub f(*%h) { %h<x> = 7 };
//! f(:$x)` sets `$x`). The eager `MakeNamedArg` built that container at the
//! argument -- it promoted the variable's slot to a shared cell and registered
//! the variable's type constraint on it, ~4k Ir per argument per call -- for
//! every callee, although the most common one, the default constructor, only
//! copies the value into an attribute.
//!
//! A lazy `MakeNamedArg` (`OpCode::MakeNamedArg { lazy: true }`, emitted only
//! for the arguments of a `CallMethodMut` site) leaves the `WrapVarRef`-tagged
//! value in the Pair. [`Interpreter::resolve_lazy_named_args`] runs first thing
//! at the site, before anything reads the arguments:
//!
//! - when the call is a `.new` the default constructor answers for a class
//!   that runs no user code over the arguments (no user `new`, `bless`,
//!   `BUILD`, `TWEAK`, BUILDPLAN or `set_build`), each such Pair takes the
//!   variable's current value, and the constructor lane runs on them;
//! - otherwise, or when the lane declines, each Pair takes the shared cell the
//!   eager op would have built, and the call proceeds exactly as before.
//!
//! So no other path ever sees a lazy Pair, and only a callee that cannot write
//! through skips the cell.

use super::*;
use crate::opcode::OpCode;

impl Interpreter {
    /// Whether `arg` is a Pair a lazy `MakeNamedArg` minted: its value is
    /// still the `VarRef` naming the caller's variable.
    // Cost: O(1), tag probes.
    fn is_lazy_named_pair(arg: &Value) -> bool {
        arg.is_string_pair_value() && matches!(arg.view(), ValueView::Pair(_, v) if v.is_varref())
    }

    /// The `CallMethodMut` site at `ip` with lazy named arguments on the
    /// stack: answer it through the constructor lane when that lane may take
    /// the variables' values, else give every such argument its shared cell.
    /// `None` means the site goes on as usual (the arguments are eager now).
    // Cost: O(a), a = arguments, when the site has none lazy; otherwise the
    // cell captures or the construction.
    pub(super) fn resolve_lazy_named_args(
        &mut self,
        code: &CompiledCode,
        ip: usize,
    ) -> Option<Result<(), RuntimeError>> {
        let OpCode::CallMethodMut {
            name_idx,
            arity,
            target_name_idx,
            target_slot,
            modifier_idx,
            quoted,
            arg_sources_idx,
        } = code.ops[ip]
        else {
            return None;
        };
        let base = self.stack.len().checked_sub(arity as usize)?;
        if arity == 0 || !self.stack[base..].iter().any(Self::is_lazy_named_pair) {
            return None;
        }
        if !self.lazy_named_ctor_candidate(code, name_idx, modifier_idx, quoted, base) {
            self.capture_lazy_named_args(code, base);
            return None;
        }
        let taken = self.take_lazy_named_values(code, base);
        Some(self.in_method_call(|vm| {
            vm.call_method_mut_site_around(code, ip, arity, target_name_idx, target_slot, |vm| {
                if let Some(result) = vm.try_ctor_early_lane(
                    code,
                    code.const_sym(name_idx),
                    arity as usize,
                    modifier_idx,
                    quoted,
                    arg_sources_idx,
                ) {
                    return result;
                }
                // The lane declined (its arguments are back on the stack where
                // they were): the full path binds the shared cells.
                vm.give_back_lazy_named_cells(code, taken);
                vm.exec_call_method_mut_op(
                    code,
                    name_idx,
                    arity,
                    target_name_idx,
                    target_slot,
                    modifier_idx,
                    quoted,
                    arg_sources_idx,
                )
            })
        }))
    }

    /// Whether the constructor lane may answer this call with the variables'
    /// values: a plain `.new` on a class the lane holds, whose construction
    /// runs no user code that could bind an argument `is rw` or keep its
    /// container (a user `new`/`bless`, `BUILD`, `TWEAK`, a replaced
    /// BUILDPLAN, an `Attribute.set_build` closure).
    // Cost: O(1) hash probes; the BUILDPLAN check is O(m), m = MRO length,
    // only once a program has replaced a BUILDPLAN.
    fn lazy_named_ctor_candidate(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        modifier_idx: Option<u32>,
        quoted: bool,
        base: usize,
    ) -> bool {
        if self.caches.ctor_lane.is_empty()
            || code.const_sym(name_idx) != crate::symbol::wk::new_method()
            || modifier_idx.is_some()
            || quoted
            || self.accessor_ref_pending
        {
            return false;
        }
        let Some(ValueView::Package(class_sym)) = base
            .checked_sub(1)
            .and_then(|idx| self.stack.get(idx))
            .map(Value::view)
        else {
            return false;
        };
        if !self.ctor_lane_hit(class_sym) {
            return false;
        }
        let Some(plan) = self.caches.ctor_lane.get(&class_sym) else {
            return false;
        };
        let runs_no_user_code = plan.eligible
            && !plan.has_build
            && !plan.has_tweak
            && !plan.has_custom_bless
            && !plan.has_build_overrides;
        runs_no_user_code && !self.buildplan_replaced_in_mro(class_sym)
    }

    /// Give each lazy Pair in `stack[base..]` the value its variable holds now
    /// -- what the eager cell would read when the constructor copies it -- and
    /// return the `VarRef`s taken out, by stack index, for
    /// [`Self::give_back_lazy_named_cells`]. A variable this cannot read
    /// directly gets its shared cell instead.
    // Cost: O(a), a = arguments.
    fn take_lazy_named_values(&mut self, code: &CompiledCode, base: usize) -> Vec<(usize, Value)> {
        let mut taken = Vec::new();
        for idx in base..self.stack.len() {
            if !Self::is_lazy_named_pair(&self.stack[idx]) {
                continue;
            }
            let ValueView::Pair(_, varref) = self.stack[idx].view() else {
                continue;
            };
            let varref = varref.clone();
            let value = match self.lazy_named_current_value(code, &varref) {
                Some(value) => value,
                None => self.lazy_named_cell(code, &varref),
            };
            self.stack[idx].swap_pair_value(value);
            taken.push((idx, varref));
        }
        taken
    }

    /// The value the eager cell of `varref`'s variable would hold, read from
    /// the variable's own slot of this frame; `None` when the variable is not
    /// a plain local here (a `:=` alias, a captured outer variable), where
    /// only the cell capture knows its container.
    ///
    /// A slot that already holds a container hands it on as the eager capture
    /// does. A plain scalar value is handed on as it is: the constructor
    /// decontainerizes the cell anyway. Anything else (an Array, a List, an
    /// object) is put in a fresh cell, since the attribute assignment treats a
    /// Scalar holding it as one item.
    // Cost: O(1); O(1) env probe for the alias check once the program has a
    // sigilless or `:=` binding.
    fn lazy_named_current_value(&self, code: &CompiledCode, varref: &Value) -> Option<Value> {
        let slot = varref.varref_slot().filter(|slot| *slot != u32::MAX)? as usize;
        let (name, _, _) = varref.as_varref()?;
        let name = name.as_str();
        if code.locals.get(slot).map(String::as_str) != Some(name) {
            return None;
        }
        if crate::env::sigilless_meta_keys_possible() && self.resolve_alias_root(name) != name {
            return None;
        }
        let current = self.locals.get(slot)?;
        if current.is_container_ref()
            || matches!(
                current.view(),
                ValueView::HashEntryRef { .. }
                    | ValueView::Proxy { .. }
                    | ValueView::Int(_)
                    | ValueView::BigInt(_)
                    | ValueView::Num(_)
                    | ValueView::Str(_)
                    | ValueView::Bool(_)
                    | ValueView::Nil
                    | ValueView::Rat(..)
                    | ValueView::FatRat(..)
                    | ValueView::BigRat(..)
                    | ValueView::Complex(..)
                    | ValueView::Package(_)
                    | ValueView::Enum { .. }
            )
        {
            return Some(current.clone());
        }
        Some(current.clone().into_container_ref())
    }

    /// The shared cell an eager `MakeNamedArg` would have built for `varref`.
    // Cost: see `capture_named_arg_cell`.
    fn lazy_named_cell(&mut self, code: &CompiledCode, varref: &Value) -> Value {
        let Some((name, inner, _)) = varref.as_varref() else {
            return varref.clone();
        };
        let inner = inner.clone();
        let slot_hint = varref.varref_slot();
        self.capture_named_arg_cell(code, name.as_str(), inner, slot_hint)
    }

    /// Give every lazy Pair in `stack[base..]` its variable's shared cell, as
    /// the eager `MakeNamedArg` would have.
    // Cost: O(a), a = arguments, plus one cell capture per lazy Pair.
    fn capture_lazy_named_args(&mut self, code: &CompiledCode, base: usize) {
        for idx in base..self.stack.len() {
            if !Self::is_lazy_named_pair(&self.stack[idx]) {
                continue;
            }
            let ValueView::Pair(_, varref) = self.stack[idx].view() else {
                continue;
            };
            let varref = varref.clone();
            let cell = self.lazy_named_cell(code, &varref);
            self.stack[idx].swap_pair_value(cell);
        }
    }

    /// Undo [`Self::take_lazy_named_values`] for a call the constructor lane
    /// declined: each argument it rewrote takes its variable's shared cell.
    // Cost: O(t) cell captures, t = rewritten arguments.
    fn give_back_lazy_named_cells(&mut self, code: &CompiledCode, taken: Vec<(usize, Value)>) {
        for (idx, varref) in taken {
            let cell = self.lazy_named_cell(code, &varref);
            if let Some(arg) = self.stack.get_mut(idx) {
                arg.swap_pair_value(cell);
            }
        }
    }
}
