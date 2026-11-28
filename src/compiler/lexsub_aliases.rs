//! Per-activation free-variable aliases for a sub declared inside a routine
//! (mutsu#9111).
//!
//! A named sub reads its free variables by name, in whatever env is live when
//! it is called. ADR-0024 gives a mainline or bare-block sub lexical
//! resolution through the `unit_lexicals` buckets, but those are keyed by the
//! sub's name, so they cannot hold one `$p` per call of the routine that
//! declares a `my sub t() { $p }`. Such a sub read its caller's `$p` instead —
//! the local of a closure or a nested block that happened to shadow it.
//!
//! The declaring frame therefore gets one hidden local per free variable. Each
//! execution of the declaration binds it to that variable's cell (see
//! `vm/vm_lexsub_aliases.rs`), and the callee's by-name resolution consults it
//! before the ambient env. Being an ordinary local of the declaring frame, the
//! alias is per activation by construction, survives the call-return merge,
//! and is captured by every closure that calls the sub (it is folded into the
//! sub's entry of [`Compiler::lexical_sub_free_vars`]).

use super::Compiler;
use crate::opcode::LexSubFreeAlias;
use crate::symbol::Symbol;

/// Prefix of every alias local. A `__mutsu_` name is a system name: never a
/// plain user lexical, so no user code can spell it and the capture machinery
/// keeps it as is.
const LEXSUB_ALIAS_PREFIX: &str = "__mutsu_lexsub_";

impl Compiler {
    /// Whether a sub declared here binds its free variables per activation of
    /// the declaring scope: inside a routine body (mutsu#9111), or inside a
    /// block of a package body (mutsu#10559).
    ///
    /// The package-block case is the shape whose sub escapes the block through
    /// the package's interface — `is export`, imported by an inline module's
    /// importer at BEGIN time, before the block ran. The importer's alias and
    /// the in-sequence registration are the same routine identity (name,
    /// package, file), so the latest activation's cells answer the import
    /// alias's call as they answer every other one. A mainline block of
    /// `GLOBAL` keeps ADR-0024's per-block bucket.
    // Cost: O(1).
    ///
    /// A compilation unit's mainline is no routine body even when it is
    /// compiled under a routine's frame: a module `require`d inside a routine
    /// runs its body there, in that routine's env, but its top-level subs
    /// read the module's file-scope lexicals, not an alias of a routine
    /// activation that never declared them (#18017).
    pub(super) fn binds_lexsub_free_vars(&self) -> bool {
        ((self.is_routine || self.lexically_in_routine) && !self.is_mainline)
            || (self.lexically_in_block
                && !crate::qualified::is_global_package(Symbol::intern(&self.current_package)))
    }

    /// Allocate the hidden alias locals for the sub `name` whose bodies were
    /// compiled under `keys`, and add them to the sub's
    /// [`Compiler::lexical_sub_free_vars`] entry. Empty unless this scope is a
    /// routine body (or lexically inside one).
    pub(super) fn alloc_lexsub_free_aliases(
        &mut self,
        name: &str,
        fingerprint: Option<u64>,
        keys: &[Symbol],
    ) -> Vec<LexSubFreeAlias> {
        if !self.binds_lexsub_free_vars() || name.contains("::") {
            return Vec::new();
        }
        let mut vars: Vec<Symbol> = Vec::new();
        for key in keys {
            let Some(cf) = self.compiled_functions.get(key) else {
                continue;
            };
            for sym in cf
                .code
                .free_var_syms
                .iter()
                .chain(cf.code.free_var_writes.iter())
            {
                if !vars.contains(sym) {
                    vars.push(*sym);
                }
            }
        }
        // Two declarations with identical text share a fingerprint; the
        // serial keeps their aliases apart when one's frame calls the other's.
        let serial = super::compile_session::mint();
        let tag = fingerprint.unwrap_or(0);
        let mut out = Vec::new();
        for var in vars {
            let var_name = var.resolve();
            // Plain `my` lexicals only: dynamic, attribute, special and
            // `__mutsu_*` names resolve through their own stores. A `&` code
            // variable is aliased like any other lexical (mutsu#10483).
            if !crate::env::is_plain_user_lexical(&var_name)
                || self.constant_vars.contains(&var_name)
                || self.sigilless_locals.contains(&var_name)
                || self.enclosing_sigilless.contains(&var_name)
            {
                continue;
            }
            let var_slot = self.local_map.get(&var_name).copied();
            // A single `for ... -> $i` parameter has no slot: the `ForLoop`
            // opcode binds it by name in this frame's env (mutsu#10512).
            let env_param = var_slot.is_none() && self.for_param_names.contains(&var_name);
            if var_slot.is_none() && !env_param && !self.enclosing_local_names.contains(&var_name) {
                continue;
            }
            let alias_name = format!("{LEXSUB_ALIAS_PREFIX}{tag:x}_{serial}_{name}_{var_name}");
            let alias_slot = self.alloc_local(&alias_name);
            out.push(LexSubFreeAlias {
                var,
                var_slot,
                env_param,
                alias: Symbol::intern(&alias_name),
                alias_slot,
            });
        }
        if !out.is_empty() {
            let sub = Symbol::intern(name);
            let table = std::rc::Rc::make_mut(&mut self.lexical_sub_free_vars);
            let free = table.entry(sub).or_default();
            for a in &out {
                if !free.contains(&a.alias) {
                    free.push(a.alias);
                }
            }
        }
        out
    }

    /// Rewrite the bodies compiled under `keys` to read the free variables
    /// `aliases` covers through slots of their own frames
    /// ([`CompiledCode::lexsub_free_slot_plan`](crate::opcode::CompiledCode::lexsub_free_slot_plan)).
    // Cost: O(k * (o + v * c)), k = `keys`, o = a body's ops, v = aliased
    // variables, c = its constants.
    pub(super) fn assign_lexsub_free_slots(
        &mut self,
        keys: &[Symbol],
        aliases: &[LexSubFreeAlias],
    ) {
        if aliases.is_empty() {
            return;
        }
        let vars: Vec<Symbol> = aliases.iter().map(|a| a.var).collect();
        self.rewrite_free_slot_reads(keys, &vars);
    }

    /// [`Self::assign_lexsub_free_slots`] for a sub declared at the unit's
    /// mainline or in a bare block of `GLOBAL`, which reads its free variables
    /// through ADR-0024's captured cells: each plain lexical this scope
    /// declares (`decl_slots`, baked from `local_map`) that the bodies read.
    /// The VM takes the cell from the running sub's capture bucket.
    // Cost: as `assign_lexsub_free_slots`.
    pub(super) fn assign_mainline_free_slots(&mut self, keys: &[Symbol], decl_slots: &[(Symbol, u32)]) {
        if self.binds_lexsub_free_vars()
            || !crate::qualified::is_global_package(Symbol::intern(&self.current_package))
        {
            return;
        }
        let vars: Vec<Symbol> = decl_slots
            .iter()
            .map(|(sym, _)| *sym)
            .filter(|sym| {
                sym.with_str(|name| {
                    crate::env::is_plain_user_lexical(name)
                        && !self.constant_vars.contains(name)
                        && !self.sigilless_locals.contains(name)
                        && !self.enclosing_sigilless.contains(name)
                })
            })
            .collect();
        self.rewrite_free_slot_reads(keys, &vars);
    }

    /// Apply [`CompiledCode::lexsub_free_slot_plan`](crate::opcode::CompiledCode::lexsub_free_slot_plan)
    /// for `vars` to each body compiled under `keys`.
    // Cost: O(k * (o + v * c)).
    fn rewrite_free_slot_reads(&mut self, keys: &[Symbol], vars: &[Symbol]) {
        if vars.is_empty() {
            return;
        }
        for key in keys {
            let Some(cf) = self.compiled_functions.get(key) else {
                continue;
            };
            let Some(plan) = cf.code.lexsub_free_slot_plan(vars) else {
                continue;
            };
            let mut cf = (**cf).clone();
            std::sync::Arc::make_mut(&mut cf.code).apply_lexsub_free_slot_plan(plan);
            self.compiled_functions.insert(*key, cf);
        }
    }

    /// A `&name` code-variable read (`&t`, `&t(...)`) of a lexically visible
    /// routine-nested sub: the fetched Sub may be called after the declaring
    /// routine returned, so fold its free variables exactly as a call site
    /// does (mutsu#9110). A `&name` local of this very code (a `&t` parameter
    /// or `my &t`) shadows the sub, so it folds nothing.
    pub(crate) fn fold_lexical_sub_free_vars_for_code_var(&mut self, name: &str) {
        if self.lexical_sub_free_vars.is_empty() || name.contains("::") {
            return;
        }
        if self.local_map.contains_key(&format!("&{name}")) {
            return;
        }
        self.fold_lexical_sub_free_vars(&Symbol::intern(name));
    }
}
