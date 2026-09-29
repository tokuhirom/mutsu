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

/// `Interpreter::lexsub_latest_cells`: (sub name, free variable env key) ->
/// the cell the most recent activation of the declaring routine bound.
///
/// Rakudo's `capturelex`: each execution of the declaration re-points the
/// sub's static outer at the running activation, so a call from code that
/// holds none of the aliases — the sub escaped through `is export`, an `our`
/// alias or a trait registry — still sees the latest activation's lexicals
/// rather than whatever its caller has under the name. Before any activation
/// ran, an exported sub's entry is its static outer's fresh container
/// (mutsu#10114, seeded by `seed_lexsub_static_cells`).
pub(crate) type LexSubLatestCells = rustc_hash::FxHashMap<(Symbol, Symbol), LexSubLatestCell>;

/// Which routine a [`LexSubLatestCells`] entry belongs to: the nested sub's
/// declaring package and the file its body lives in, i.e. what a
/// `RoutineFrame` of that sub records as `package` / `def_file`.
///
/// The table is keyed by the sub's bare name (ADR-0114 §4). An entry whose
/// owner matches the running routine answers even where the caller's env
/// holds a same-named variable — the sub's free variable is never the
/// caller's (mutsu#10114). One whose owner does not match (an unrelated
/// routine of the same name) answers only where the env has no binding at all.
pub(crate) type LexSubOwner = (Symbol, Option<Symbol>);

/// One [`LexSubLatestCells`] entry.
#[derive(Clone)]
pub(crate) struct LexSubLatestCell {
    pub(crate) owner: LexSubOwner,
    pub(crate) cell: Value,
}

impl Interpreter {
    /// The [`LexSubOwner`] of the routine `plan` declares.
    fn lexsub_plan_owner(
        plan: &crate::opcode::CompiledSubDeclPlan,
        compiled_fns: &CompiledFns,
    ) -> Option<LexSubOwner> {
        let cf = compiled_fns.get(plan.compiled_routine_keys.first()?)?;
        Some((cf.package_sym(), cf.source_file_sym()))
    }

    /// `RegisterDecl` of a sub plan: bind this activation's aliases.
    pub(super) fn bind_lexsub_free_aliases(
        &mut self,
        code: &CompiledCode,
        idx: u32,
        compiled_fns: &CompiledFns,
    ) {
        let Some(plan) = code.sub_decl_plans.get(idx as usize) else {
            return;
        };
        if plan.lexsub_free_aliases.is_empty() {
            return;
        }
        let sub = plan.name;
        let owner = Self::lexsub_plan_owner(plan, compiled_fns);
        for a in &plan.lexsub_free_aliases {
            let Some(cell) = self.lexsub_alias_cell(code, a) else {
                // This activation has no cell for the variable (the hoisted
                // pass before its `my` ran, or a variable that cannot be
                // boxed): a stale entry from an earlier activation or the
                // static seed must not answer for it instead of the env.
                if self.lexsub_latest_cells.contains_key(&(sub, a.var)) {
                    crate::runtime::cow_table_mut(&mut self.lexsub_latest_cells)
                        .remove(&(sub, a.var));
                }
                continue;
            };
            if let Some(slot) = self.locals.get_mut(a.alias_slot as usize) {
                *slot = cell.clone();
            }
            self.env_mut().insert_sym(a.alias, cell.clone());
            if let Some(owner) = owner {
                crate::runtime::cow_table_mut(&mut self.lexsub_latest_cells)
                    .insert((sub, a.var), LexSubLatestCell { owner, cell });
            }
            self.note_lexsub_alias(sub, a);
        }
    }

    /// Record in `lexsub_free_aliases` that `sub` reads `a.var` through
    /// `a.alias`, once.
    fn note_lexsub_alias(&mut self, sub: Symbol, a: &LexSubFreeAlias) {
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
        match self.lexsub_alias_sym(name) {
            Some(alias) => self.env().get_sym(alias),
            None => self.lexsub_latest_cell(name),
        }
    }

    /// The cell the latest activation of the running routine-nested sub's
    /// declaring routine bound for `name`, for a call made from code that
    /// captured none of its aliases (see [`LexSubLatestCells`]).
    ///
    /// The running routine is identified by its frame's name, package and
    /// defining file (see [`LexSubOwner`]); only an entry of that same routine
    /// overrides a binding the ordinary by-name resolution finds in the env —
    /// the caller's own same-named variable (mutsu#10114). The table is keyed
    /// by the bare sub name, so an unrelated same-named routine falls back to
    /// answering only where the env has no binding of `name` at all.
    // Cost: O(1) expected; two hash probes.
    #[inline]
    fn lexsub_latest_cell(&self, name: &str) -> Option<&Value> {
        if self.lexsub_latest_cells.is_empty() {
            return None;
        }
        let frame = self.routine_stack().last()?;
        if frame.is_block || frame.is_method {
            return None;
        }
        let entry = self
            .lexsub_latest_cells
            .get(&(frame.name, Symbol::intern(name)))?;
        (entry.owner == (frame.package, frame.def_file) || self.env().get(name).is_none())
            .then_some(&entry.cell)
    }

    /// Mutable counterpart of [`Self::lexsub_alias_slot`].
    pub(crate) fn lexsub_alias_slot_mut(&mut self, name: &str) -> Option<&mut Value> {
        match self.lexsub_alias_sym(name) {
            Some(alias) => self.env_mut().get_mut_sym(alias),
            None => {
                self.lexsub_latest_cell(name)?;
                let frame = self.routine_stack().last()?.name;
                crate::runtime::cow_table_mut(&mut self.lexsub_latest_cells)
                    .get_mut(&(frame, Symbol::intern(name)))
                    .map(|entry| &mut entry.cell)
            }
        }
    }

    /// True when `callee`'s write to its free variable `name` went through an
    /// alias bound in the live env chain, or through its latest-activation
    /// cell, so it must not be replayed into the caller's slot of that name
    /// (which may be an unrelated shadow). `owner` is the callee's
    /// [`LexSubOwner`], as [`Self::lexsub_latest_cell`] compares it.
    pub(crate) fn is_lexsub_alias_write(
        &self,
        callee: &str,
        owner: LexSubOwner,
        name: &str,
    ) -> bool {
        if self.lexsub_free_aliases.is_empty() {
            return false;
        }
        let callee = Symbol::intern(callee);
        let Some(list) = self.lexsub_free_aliases.get(&callee) else {
            return false;
        };
        list.iter().any(|(var, alias)| {
            var.as_str() == name
                && (self.env().get_sym(*alias).is_some()
                    || self
                        .lexsub_latest_cells
                        .get(&(callee, *var))
                        .is_some_and(|entry| {
                            entry.owner == owner || self.env().get_sym(*var).is_none()
                        }))
        })
    }

    /// The binding a TRIR chunk of the routine-nested sub `callee` reads for
    /// its free variable `name`. Per activation, so never memoized. `owner`
    /// is the chunk's [`LexSubOwner`].
    pub(crate) fn lexsub_alias_binding_for(
        &self,
        callee: Symbol,
        owner: LexSubOwner,
        name: &str,
    ) -> Option<Value> {
        if self.lexsub_free_aliases.is_empty() {
            return None;
        }
        let list = self.lexsub_free_aliases.get(&callee)?;
        list.iter()
            .filter(|(var, _)| var.as_str() == name)
            .find_map(|(_, alias)| self.env().get_sym(*alias).cloned())
            .or_else(|| {
                let entry = self
                    .lexsub_latest_cells
                    .get(&(callee, Symbol::intern(name)))?;
                (entry.owner == owner || self.env().get(name).is_none()).then(|| entry.cell.clone())
            })
    }

    /// mutsu#10114: give each free variable of the exported routine-nested
    /// sub `plan` declares its static outer's container, for a call made
    /// before any activation of the declaring routine ran.
    ///
    /// Rakudo compiles the `is export` routine against the declaring
    /// routine's *static* frame, whose lexicals were never initialized: an
    /// out-of-extent call reads an empty `@`/`%` or an `Any` scalar, and never
    /// the importer's same-named variable. An activation's binding replaces
    /// the entry (`bind_lexsub_free_aliases`), as `capturelex` would.
    ///
    /// `statics` is the declaring routine's static frame: every sub nested in
    /// the same routine shares one container per variable, as they share the
    /// one frame in Rakudo.
    ///
    /// Cost: O(v), v = the plan's aliased free variables.
    pub(super) fn seed_lexsub_static_cells(
        &mut self,
        plan: &crate::opcode::CompiledSubDeclPlan,
        compiled_fns: &CompiledFns,
        statics: &mut rustc_hash::FxHashMap<Symbol, Value>,
    ) {
        if plan.lexsub_free_aliases.is_empty() {
            return;
        }
        let Some(owner) = Self::lexsub_plan_owner(plan, compiled_fns) else {
            return;
        };
        for a in &plan.lexsub_free_aliases {
            // The table is what marks a frame of the sub as one whose free
            // variables resolve here, on the write paths too.
            self.note_lexsub_alias(plan.name, a);
            let key = (plan.name, a.var);
            if self.lexsub_latest_cells.contains_key(&key) {
                continue;
            }
            let cell = statics
                .entry(a.var)
                .or_insert_with(|| {
                    match a.var.as_str().as_bytes().first() {
                        Some(b'@') => Value::real_array(Vec::new()),
                        Some(b'%') => Value::hash(crate::value::HashData::default()),
                        _ => Value::package(crate::symbol::wk::any()),
                    }
                    .into_container_ref()
                })
                .clone();
            crate::runtime::cow_table_mut(&mut self.lexsub_latest_cells)
                .insert(key, LexSubLatestCell { owner, cell });
        }
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
