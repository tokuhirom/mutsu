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

    /// Whether a body of the sub `plan` declares rebinds its free variable
    /// `var` (`$var := ...`).
    // Cost: O(r), r = rebound free variables of the sub's bodies.
    fn lexsub_rebinds_var(
        plan: &crate::opcode::CompiledSubDeclPlan,
        compiled_fns: &CompiledFns,
        var: Symbol,
    ) -> bool {
        plan.compiled_routine_keys.iter().any(|key| {
            compiled_fns
                .get(key)
                .is_some_and(|cf| cf.code.free_var_rebinds.contains(&var))
        })
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
        let sub = plan.name;
        self.note_exported_sub_self(plan, compiled_fns);
        if plan.lexsub_free_aliases.is_empty() {
            // A same-named sub with no routine-nested free variables (a
            // mainline block's `my sub reset`) now shadows any routine-nested
            // one of that name, whose latest-activation cells must not answer
            // for it: the owner key is only (package, file), so they would
            // (mutsu#10391).
            // Cost: O(1) unless a routine-nested sub of this name exists,
            // then O(c), c = latest-cell entries.
            // A `multi` candidate does not shadow its siblings: they share the
            // name and each keeps the cells it bound.
            if !plan.multi && self.lexicals.lexsub_free_aliases.contains_key(&sub) {
                crate::runtime::cow_table_mut(&mut self.lexicals.lexsub_latest_cells)
                    .retain(|(name, var), _| *name != sub || *var == crate::symbol::wk::self_());
            }
            return;
        }
        let owner = Self::lexsub_plan_owner(plan, compiled_fns);
        // Only the hoisted registration can run before a free variable's
        // declaration (see `lexsub_alias_cell`).
        let hoisted_pass = plan.pass.hoisted;
        for a in &plan.lexsub_free_aliases {
            // The hoisted pass runs before a declaration in the sub's own
            // scope has: whatever the slot holds is not that variable yet (a
            // caller's same-named binding may show through), so boxing it
            // would hand a closure made before the declaration a cell the
            // declaration then replaces. Seed the cell the declaration adopts
            // instead (#9911's mechanism, `vm_hoist_capture_cells.rs`).
            let seeded = match a.var_slot {
                Some(slot)
                    if plan.hoist_seed_slots.contains(&slot)
                        && !code.param_locals.contains(&a.var)
                        && !a.var.as_str().starts_with('&')
                        && (self.hoist_seeds_own_slot(code, slot as usize)
                            // A still-empty slot of another kind boxes
                            // nothing (the in-sequence pass binds it); only a
                            // value showing through before the declaration
                            // needs the seeded cell.
                            || (!a.var.as_str().starts_with(['@', '%'])
                                && self.locals.get(slot as usize).is_some_and(|v| !v.is_nil()))) =>
                {
                    let name = a.var.resolve();
                    self.pending_hoist_cell_at(slot as usize)
                        .or_else(|| self.seed_hoist_capture_cell(slot as usize, &name, ""))
                }
                _ => None,
            };
            let Some(cell) = seeded.or_else(|| self.lexsub_alias_cell(code, a, hoisted_pass))
            else {
                // This activation has no cell for the variable (the hoisted
                // pass before its `my` ran, or a variable that cannot be
                // boxed): a stale entry from an earlier activation or the
                // static seed must not answer for it instead of the env.
                if self
                    .lexicals
                    .lexsub_latest_cells
                    .contains_key(&(sub, a.var))
                {
                    crate::runtime::cow_table_mut(&mut self.lexicals.lexsub_latest_cells)
                        .remove(&(sub, a.var));
                }
                continue;
            };
            // A variable the sub rebinds with `:=` needs a binding cell, so
            // the rebind seats the new binding inside the cell this frame's
            // slot shares (`unit_scope_lexical_rebind`) instead of replacing
            // the callee's env entry, which the return would then drop.
            let cell = if Self::binding_cell_of(&cell).is_none()
                && Self::lexsub_rebinds_var(plan, compiled_fns, a.var)
                && let Some(slot) = a.var_slot.map(|s| s as usize)
                && code.locals.get(slot).is_some_and(|n| *n == a.var.resolve())
            {
                let wrapped = Self::wrap_in_binding_cell(cell);
                self.locals[slot] = wrapped.clone();
                self.env_mut().insert_sym(a.var, wrapped.clone());
                wrapped
            } else {
                cell
            };
            if let Some(slot) = self.locals.get_mut(a.alias_slot as usize) {
                *slot = cell.clone();
            }
            self.env_mut().insert_sym(a.alias, cell.clone());
            if let Some(owner) = owner {
                crate::runtime::cow_table_mut(&mut self.lexicals.lexsub_latest_cells)
                    .insert((sub, a.var), LexSubLatestCell { owner, cell });
            }
            self.note_lexsub_alias(sub, a);
        }
    }

    /// An `is export` sub declared in a method body reads `self` (and so its
    /// attributes) from the latest activation of that method, as Rakudo's
    /// `capturelex` re-points the sub's static outer at the running frame
    /// (mutsu#14870: `submethod BUILD { multi sub log(...) is export { $!bar } }`).
    /// Filed under the sub's name and the key `self` in `lexsub_latest_cells`.
    // Cost: O(1).
    fn note_exported_sub_self(
        &mut self,
        plan: &crate::opcode::CompiledSubDeclPlan,
        compiled_fns: &CompiledFns,
    ) {
        if !plan.is_export || plan.name_chunk.is_some() {
            return;
        }
        let Some(frame) = self.routine_stack().last() else {
            return;
        };
        if !frame.is_method || frame.is_block {
            return;
        }
        let Some(owner) = Self::lexsub_plan_owner(plan, compiled_fns) else {
            return;
        };
        let Some(self_val) = self.env().get_sym(crate::symbol::wk::self_()).cloned() else {
            return;
        };
        crate::runtime::cow_table_mut(&mut self.lexicals.lexsub_latest_cells).insert(
            (plan.name, crate::symbol::wk::self_()),
            LexSubLatestCell {
                owner,
                cell: self_val,
            },
        );
    }

    /// The `self` the latest activation of the running exported sub's declaring
    /// method recorded ([`Self::note_exported_sub_self`]).
    // Cost: O(1) expected.
    #[inline]
    pub(crate) fn exported_sub_latest_self(&self) -> Option<Value> {
        if self.lexicals.lexsub_latest_cells.is_empty() {
            return None;
        }
        let frame = self.routine_stack().last()?;
        if frame.is_block || frame.is_method {
            return None;
        }
        let entry = self
            .lexicals
            .lexsub_latest_cells
            .get(&(frame.name, crate::symbol::wk::self_()))?;
        (entry.owner == (frame.package, frame.def_file)).then(|| entry.cell.clone())
    }

    /// Record in `lexsub_free_aliases` that `sub` reads `a.var` through
    /// `a.alias`, once.
    fn note_lexsub_alias(&mut self, sub: Symbol, a: &LexSubFreeAlias) {
        let known = self
            .lexicals
            .lexsub_free_aliases
            .get(&sub)
            .is_some_and(|list| list.contains(&(a.var, a.alias)));
        if !known {
            crate::runtime::cow_table_mut(&mut self.lexicals.lexsub_free_aliases)
                .entry(sub)
                .or_default()
                .push((a.var, a.alias));
        }
    }

    /// The cell `a`'s free variable is bound to in the running (declaring)
    /// frame, boxing a local of that frame in place. `None` leaves the
    /// variable on the ordinary by-name resolution.
    fn lexsub_alias_cell(
        &mut self,
        code: &CompiledCode,
        a: &LexSubFreeAlias,
        hoisted_pass: bool,
    ) -> Option<Value> {
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
            // in-sequence pass binds it. On the in-sequence pass the
            // declaration has run, and a `Nil` there is its value (`my $t :=
            // %h<absent>`): without a cell of its own, this activation's
            // alias kept the cell an earlier iteration of an inline loop
            // body left bound (#12478).
            if (cur.is_nil() && hoisted_pass) || self.type_constrained_unboxable(&name) {
                return None;
            }
            let boxed = cur.into_container_ref();
            self.locals[slot] = boxed.clone();
            self.env_mut().insert(name, boxed.clone());
            return Some(boxed);
        }
        // A loop parameter bound by name in this frame's env: that binding is
        // the original, so box it in place like a slot. The next iteration
        // binds a fresh value, leaving this one's cell with the sub.
        if a.env_param && self.lexsub_alias_sym(&name).is_none() {
            let cur = self.env().get_sym(a.var)?.clone();
            if cur.is_container_ref() {
                return Some(cur);
            }
            if cur.is_nil() || self.type_constrained_unboxable(&name) {
                return None;
            }
            let boxed = cur.into_container_ref();
            self.env_mut().insert_sym(a.var, boxed.clone());
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
    fn active_loop_param_shadows(&self, name: &str) -> bool {
        self.topic_state
            .active_loop_param_names
            .iter()
            .any(|params| params.contains(name))
    }

    #[inline]
    fn lexsub_alias_sym(&self, name: &str) -> Option<Symbol> {
        if self.lexicals.lexsub_free_aliases.is_empty() {
            return None;
        }
        // A pointy `for` parameter is an active lexical in the running
        // candidate, even though the VM binds it by name rather than into a
        // compiled local slot. Do not let a sibling multi candidate's
        // free-variable alias bypass that shadowing binding.
        if self.active_loop_param_shadows(name) {
            return None;
        }
        let frame = self.routine_stack().last()?;
        if frame.is_block {
            return None;
        }
        let list = self.lexicals.lexsub_free_aliases.get(&frame.name)?;
        list.iter()
            .filter(|(var, _)| var.as_str() == name)
            .map(|(_, alias)| *alias)
            .find(|alias| self.env().get_sym(*alias).is_some())
    }

    /// True while the running routine is a routine-nested sub with bound
    /// aliases, i.e. while [`Self::lexsub_alias_slot`] may answer.
    #[inline]
    pub(crate) fn lexsub_alias_frame_active(&self) -> bool {
        if self.lexicals.lexsub_free_aliases.is_empty() {
            return false;
        }
        self.routine_stack()
            .last()
            .is_some_and(|f| !f.is_block && self.lexicals.lexsub_free_aliases.contains_key(&f.name))
    }

    /// The running routine-nested sub's own binding of its free variable
    /// `name` (the shared cell), or `None`.
    #[inline]
    pub(crate) fn lexsub_alias_slot(&self, name: &str) -> Option<&Value> {
        if self.active_loop_param_shadows(name) {
            return None;
        }
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
        if self.lexicals.lexsub_latest_cells.is_empty() {
            return None;
        }
        let frame = self.routine_stack().last()?;
        if frame.is_block || frame.is_method {
            return None;
        }
        let entry = self
            .lexicals
            .lexsub_latest_cells
            .get(&(frame.name, Symbol::intern(name)))?;
        // A program-scope routine of the same name, package and file (a
        // `sub sv { $s }` beside a routine's nested `sub sv { $s }`) runs a
        // body of its own with its own captured cells: the nested sub's
        // latest activation is not its outer (#13900).
        if self.active_unit_lexical_bucket().is_some() {
            return None;
        }
        (entry.owner == (frame.package, frame.def_file) || self.env().get(name).is_none())
            .then_some(&entry.cell)
    }

    /// Mutable counterpart of [`Self::lexsub_alias_slot`].
    pub(crate) fn lexsub_alias_slot_mut(&mut self, name: &str) -> Option<&mut Value> {
        if self.active_loop_param_shadows(name) {
            return None;
        }
        match self.lexsub_alias_sym(name) {
            Some(alias) => self.env_mut().get_mut_sym(alias),
            None => {
                self.lexsub_latest_cell(name)?;
                let frame = self.routine_stack().last()?.name;
                crate::runtime::cow_table_mut(&mut self.lexicals.lexsub_latest_cells)
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
        if self.lexicals.lexsub_free_aliases.is_empty() {
            return false;
        }
        let callee = Symbol::intern(callee);
        let Some(list) = self.lexicals.lexsub_free_aliases.get(&callee) else {
            return false;
        };
        list.iter().any(|(var, alias)| {
            var.as_str() == name
                && (self.env().get_sym(*alias).is_some()
                    || self
                        .lexicals
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
        if self.lexicals.lexsub_free_aliases.is_empty() {
            return None;
        }
        let list = self.lexicals.lexsub_free_aliases.get(&callee)?;
        list.iter()
            .filter(|(var, _)| var.as_str() == name)
            .find_map(|(_, alias)| self.env().get_sym(*alias).cloned())
            .or_else(|| {
                let entry = self
                    .lexicals
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
            if self.lexicals.lexsub_latest_cells.contains_key(&key) {
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
            crate::runtime::cow_table_mut(&mut self.lexicals.lexsub_latest_cells)
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
            if self.env().frame_capture_holds(*sym) {
                continue;
            }
            if let Some(cell) = sym.with_str(|s| self.lexsub_alias_slot(s).cloned()) {
                env.insert_sym(*sym, cell);
            }
        }
    }
}

impl Interpreter {
    /// Whether the hoisted pass seeds the cell of `code`'s slot `slot`
    /// although it is still empty: the routine's own plain `my` variable
    /// (`CompiledCode::is_fresh_local`), which a call no longer fills by name.
    /// A nested sub called before the declaration ran then shares the
    /// container the declaration adopts, as in rakudo, instead of resolving
    /// its free variable through a same-bodied sub's captured cell (#13890,
    /// #13900). The adopting declaration gives the cell what its own
    /// container would carry (`adopt_hoist_pending_cell`).
    // Cost: O(1).
    fn hoist_seeds_own_slot(&self, code: &CompiledCode, slot: usize) -> bool {
        code.is_fresh_local(slot) && self.locals.get(slot).is_some_and(Value::is_nil)
    }
}
