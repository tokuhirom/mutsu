//! Per-activation free variables of a sub declared inside a routine
//! (mutsu#9111; the compile-time half is `compiler/lexsub_aliases.rs`).
//!
//! `sub sh($p) { my sub t() { $p }; -> { my $p = 99; t() } }` — `t` reads the
//! `$p` of the `sh` call that declared it, whoever calls `t`. mutsu reads a
//! named sub's free variables by name in the env that is live at the call, so
//! any local of the caller named `$p` answered instead.
//!
//! Each execution of the declaration (hoisted and in-sequence) binds a hidden
//! local of the declaring frame to the free variable's cell, boxing the
//! variable in place when it is not one yet, exactly as ADR-0024's capture
//! does. While the sub's own frame is on top, a by-name access to one of its
//! free variables resolves that alias from env first. The alias is visible
//! there because the sub runs in an overlay of its caller's env and every
//! lexical caller either is the declaring frame or captured the alias
//! (the compiler folds it into the calls' capture set).
//!
//! Frame discipline follows ADR-0024 §3: only the last routine frame counts,
//! and a block frame on top opts out, so a closure keeps its own captures.

use super::*;
use crate::opcode::LexSubFreeAlias;

/// `Interpreter::lexsub_free_aliases`: sub name -> (free variable env key,
/// alias local name). Several declarations may share a sub name; each adds
/// its own aliases, and the one bound in the live env chain answers.
pub(crate) type LexSubAliasTable = rustc_hash::FxHashMap<Symbol, Vec<(Symbol, Symbol)>>;

impl Interpreter {
    /// `RegisterDecl` of a sub plan: bind this activation's aliases.
    pub(super) fn bind_lexsub_free_aliases(&mut self, code: &CompiledCode, idx: u32) {
        let Some(plan) = code.sub_decl_plans.get(idx as usize) else {
            return;
        };
        if plan.lexsub_free_aliases.is_empty() {
            return;
        }
        let sub = plan.name;
        for a in &plan.lexsub_free_aliases {
            let Some(cell) = self.lexsub_alias_cell(code, a) else {
                continue;
            };
            if let Some(slot) = self.locals.get_mut(a.alias_slot as usize) {
                *slot = cell.clone();
            }
            self.env_mut().insert_sym(a.alias, cell);
            let known = self
                .lexsub_free_aliases
                .get(&sub)
                .is_some_and(|list| list.contains(&(a.var, a.alias)));
            if !known {
                crate::runtime::cow_table_mut(&mut self.lexsub_free_aliases)
                    .entry(sub)
                    .or_default()
                    .push((a.var, a.alias));
            }
        }
    }

    /// The cell `a`'s free variable is bound to in the running (declaring)
    /// frame, boxing a local of that frame in place. `None` leaves the
    /// variable on the ordinary by-name resolution.
    fn lexsub_alias_cell(&mut self, code: &CompiledCode, a: &LexSubFreeAlias) -> Option<Value> {
        let name = a.var.resolve();
        if let Some(slot) = a.var_slot.map(|s| s as usize)
            && code.locals.get(slot).is_some_and(|n| *n == name)
        {
            // `state`/`our` storage lives in its own stores; a cell here would
            // detach the slot from them.
            if code.state_locals.iter().any(|(s, _)| *s == slot)
                || code.our_locals.iter().any(|(s, _)| *s == slot)
            {
                return None;
            }
            let cur = self.locals.get(slot)?.clone();
            if cur.is_container_ref() {
                return Some(cur);
            }
            // Hoisted pass before the variable's own declaration ran: the
            // in-sequence pass binds it.
            if cur.is_nil() || self.type_constrained_unboxable(&name) {
                return None;
            }
            let boxed = cur.into_container_ref();
            self.locals[slot] = boxed.clone();
            self.env_mut().insert(name, boxed.clone());
            return Some(boxed);
        }
        // A variable of an enclosing frame reaches this one as a captured
        // binding; only a shared cell tracks the original.
        let found = self
            .lexsub_alias_slot(&name)
            .or_else(|| self.env().get_sym(a.var))?;
        found.is_container_ref().then(|| found.clone())
    }

    /// The alias local `name` resolves through while the running routine is a
    /// routine-nested sub that declared one for it.
    #[inline]
    fn lexsub_alias_sym(&self, name: &str) -> Option<Symbol> {
        if self.lexsub_free_aliases.is_empty() {
            return None;
        }
        let frame = self.routine_stack().last()?;
        if frame.is_block {
            return None;
        }
        let list = self.lexsub_free_aliases.get(&frame.name)?;
        list.iter()
            .filter(|(var, _)| var.as_str() == name)
            .map(|(_, alias)| *alias)
            .find(|alias| self.env().get_sym(*alias).is_some())
    }

    /// True while the running routine is a routine-nested sub with bound
    /// aliases, i.e. while [`Self::lexsub_alias_slot`] may answer.
    #[inline]
    pub(crate) fn lexsub_alias_frame_active(&self) -> bool {
        if self.lexsub_free_aliases.is_empty() {
            return false;
        }
        self.routine_stack()
            .last()
            .is_some_and(|f| !f.is_block && self.lexsub_free_aliases.contains_key(&f.name))
    }

    /// The running routine-nested sub's own binding of its free variable
    /// `name` (the shared cell), or `None`.
    #[inline]
    pub(crate) fn lexsub_alias_slot(&self, name: &str) -> Option<&Value> {
        let alias = self.lexsub_alias_sym(name)?;
        self.env().get_sym(alias)
    }

    /// Mutable counterpart of [`Self::lexsub_alias_slot`].
    pub(crate) fn lexsub_alias_slot_mut(&mut self, name: &str) -> Option<&mut Value> {
        let alias = self.lexsub_alias_sym(name)?;
        self.env_mut().get_mut_sym(alias)
    }

    /// True when `callee`'s write to its free variable `name` went through an
    /// alias bound in the live env chain, so it must not be replayed into the
    /// caller's slot of that name (which may be an unrelated shadow).
    pub(crate) fn is_lexsub_alias_write(&self, callee: &str, name: &str) -> bool {
        if self.lexsub_free_aliases.is_empty() {
            return false;
        }
        let Some(list) = self.lexsub_free_aliases.get(&Symbol::intern(callee)) else {
            return false;
        };
        list.iter()
            .any(|(var, alias)| var.as_str() == name && self.env().get_sym(*alias).is_some())
    }

    /// The binding a TRIR chunk of the routine-nested sub `callee` reads for
    /// its free variable `name`. Per activation, so never memoized.
    pub(crate) fn lexsub_alias_binding_for(&self, callee: Symbol, name: &str) -> Option<Value> {
        if self.lexsub_free_aliases.is_empty() {
            return None;
        }
        let list = self.lexsub_free_aliases.get(&callee)?;
        list.iter()
            .filter(|(var, _)| var.as_str() == name)
            .find_map(|(_, alias)| self.env().get_sym(*alias).cloned())
    }

    /// ADR-0024 §4 for routine-nested subs: a closure created while such a
    /// sub runs captures the sub's own cells, not the caller's shadows.
    pub(super) fn inject_lexsub_alias_captures(&self, cc: &CompiledCode, env: &mut Env) {
        if !self.lexsub_alias_frame_active() {
            return;
        }
        for sym in &cc.free_var_syms {
            if let Some(cell) = sym.with_str(|s| self.lexsub_alias_slot(s).cloned()) {
                env.insert_sym(*sym, cell);
            }
        }
    }
}
