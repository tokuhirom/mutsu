//! What a module load needs to know about the module's AST, recorded with
//! its compiled section so that a precompilation hit does not read the AST at
//! all (ADR-12026 §2.1).
//!
//! [`ModuleLoadFacts::compute`] runs every pure AST pass `load_module_inner`
//! performs, on the AST the compile sees: after the BEGIN prologue is
//! ordered, before the undeclared-routine guards are spliced in. A load uses
//! the facts in place of the passes whether they were just computed (a miss)
//! or read back (a hit), so the two paths share one implementation.
//!
//! The AST itself stays reachable through [`ModuleAst`], which decodes it on
//! the first request. Only the rare consumers the facts cannot serve ask for
//! it: module-level phasers, declarator-documented units, a `state`-declaring
//! sub's shared-body capture, a compile the cache cannot serve, and the verify
//! mode.

use super::Interpreter;
use super::module_merge::ScopeNameCandidate;
use super::undeclared_routines::RecordedCalls;
use crate::ast::Stmt;
use crate::symbol::Symbol;
use crate::value::RuntimeError;
use std::cell::OnceCell;

/// The results of the pure AST passes of a module load. See the module docs.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub(crate) struct ModuleLoadFacts {
    /// Where the undeclared-routine guards go: after the BEGIN prologue.
    pub(crate) prologue_len: usize,
    /// Whether the unit has no statements at all (before guards).
    pub(crate) is_empty: bool,
    /// Whether the unit's top level holds a block phaser the mainline splits
    /// off (`PRE`/`ENTER`/`POST`/`LEAVE`/`KEEP`/`UNDO`).
    pub(crate) has_block_phasers: bool,
    pub(crate) exported_operator_names: Vec<String>,
    /// The AST half of the undeclared-routine check.
    pub(crate) recorded_calls: Option<RecordedCalls>,
    pub(crate) unit_name: Option<String>,
    pub(crate) scope_name_candidates: Vec<ScopeNameCandidate>,
    /// Every statement is a `use` (before guards).
    pub(crate) use_only: bool,
    pub(crate) top_level_uses: Vec<Symbol>,
    pub(crate) unit_lexical_names: Vec<String>,
    pub(crate) unit_package_scope_names: Vec<String>,
    pub(crate) unit_our_var_names: Vec<String>,
    pub(crate) own_dynamic_names: Vec<String>,
    pub(crate) has_state_sub: bool,
    /// Sorted, so equal facts encode alike.
    pub(crate) exported_type_names: Vec<String>,
    pub(crate) our_routine_names: Vec<Symbol>,
    /// Where the source's Pod blocks are (ADR-12026 §2.4), so `$=pod` is
    /// rebuilt from those ranges; none when they cannot be isolated.
    pub(crate) pod_ranges: Option<super::io_pod_blocks::PodRanges>,
}

impl ModuleLoadFacts {
    /// Run the load's pure AST passes over `stmts` (prologue ordered, no
    /// guards yet). The `EXPORTHOW` directive check is not among them: it
    /// raises, so it runs where the load runs it, and an entry is only ever
    /// written for a unit that passed it.
    // Cost: O(n), n = size of the AST (a handful of walks).
    pub(crate) fn compute(stmts: &[Stmt], prologue_len: usize, pod_source: &str) -> Self {
        let mut exported_type_names: Vec<String> = Interpreter::collect_exported_type_names(stmts)
            .into_iter()
            .collect();
        exported_type_names.sort();
        ModuleLoadFacts {
            prologue_len,
            is_empty: stmts.is_empty(),
            has_block_phasers: Interpreter::block_has_split_phasers(stmts),
            exported_operator_names: Interpreter::extract_module_exported_operator_names(stmts),
            recorded_calls: super::undeclared_routines::record_mainline_calls(stmts),
            unit_name: Interpreter::detect_unit_package_name(stmts),
            scope_name_candidates: Interpreter::module_scope_name_candidates(stmts),
            use_only: Interpreter::should_skip_runtime_for_use_only_module(stmts),
            top_level_uses: Interpreter::top_level_use_modules(stmts),
            unit_lexical_names: Interpreter::collect_unit_lexical_names(stmts),
            unit_package_scope_names: Interpreter::collect_unit_package_scope_names(stmts),
            unit_our_var_names: Interpreter::collect_unit_our_var_names(stmts),
            own_dynamic_names: Interpreter::collect_module_own_dynamic_names(stmts),
            has_state_sub: Interpreter::module_has_state_sub(stmts),
            exported_type_names,
            our_routine_names: Interpreter::module_our_routine_names(stmts),
            pod_ranges: Interpreter::pod_ranges_of(pod_source),
        }
    }
}

/// A module's AST during its load: either in hand (the load parsed it or read
/// the AST cache), or still encoded in the precompilation entry and decoded
/// on the first [`ModuleAst::stmts`]. Either way the guards are spliced in at
/// `prologue_len`, so every reader sees exactly what the compile saw.
pub(crate) struct ModuleAst {
    stmts: OnceCell<Vec<Stmt>>,
    encoded: Option<Vec<u8>>,
    guards: Vec<Stmt>,
    prologue_len: usize,
    /// Whether the encoded AST has no statements (from the load facts).
    empty_unguarded: bool,
    source_path: String,
}

impl ModuleAst {
    /// An AST already in hand, without its guards.
    pub(crate) fn loaded(stmts: Vec<Stmt>, prologue_len: usize, source_path: String) -> Self {
        let cell = OnceCell::new();
        let _ = cell.set(stmts);
        ModuleAst {
            stmts: cell,
            encoded: None,
            guards: Vec::new(),
            prologue_len,
            empty_unguarded: false,
            source_path,
        }
    }

    /// An AST still encoded in a precompilation entry (`stmts` encoded with
    /// [`crate::precomp::encode_stmts`], without guards), whose load facts
    /// are `facts`.
    pub(crate) fn encoded(bytes: Vec<u8>, facts: &ModuleLoadFacts, source_path: String) -> Self {
        ModuleAst {
            stmts: OnceCell::new(),
            encoded: Some(bytes),
            guards: Vec::new(),
            prologue_len: facts.prologue_len,
            empty_unguarded: facts.is_empty,
            source_path,
        }
    }

    /// Splice the undeclared-routine `guards` in after the prologue. Done
    /// once, before the first reader.
    // Cost: O(n) when the AST is in hand (the splice), else O(g).
    pub(crate) fn set_guards(&mut self, guards: Vec<Stmt>) {
        if guards.is_empty() {
            return;
        }
        match self.stmts.get_mut() {
            Some(stmts) => {
                let at = self.prologue_len.min(stmts.len());
                stmts.splice(at..at, guards);
            }
            None => self.guards = guards,
        }
    }

    /// Whether the module has no statements, guards included.
    // Cost: O(1).
    pub(crate) fn is_empty(&self) -> bool {
        match self.stmts.get() {
            Some(stmts) => stmts.is_empty(),
            None => self.empty_unguarded && self.guards.is_empty(),
        }
    }

    /// The module's statements, guards included, decoding them on the first
    /// call.
    // Cost: O(n) on the first call (the decode), O(1) after.
    pub(crate) fn stmts(&self) -> Result<&[Stmt], RuntimeError> {
        if let Some(stmts) = self.stmts.get() {
            return Ok(stmts);
        }
        let mut stmts = self.decode_unguarded()?;
        let at = self.prologue_len.min(stmts.len());
        stmts.splice(at..at, self.guards.iter().cloned());
        Ok(self.stmts.get_or_init(|| stmts))
    }

    /// The statements if they have been decoded (or were in hand from the
    /// start), else none. For a consumer the load decoded the AST for up
    /// front (see `load_module_inner`).
    // Cost: O(1).
    pub(crate) fn materialized_or_empty(&self) -> &[Stmt] {
        self.stmts.get().map_or(&[], Vec::as_slice)
    }

    /// The encoded AST decoded afresh, without guards (the verify mode
    /// recomputes the facts from it).
    // Cost: O(n), n = size of the AST.
    pub(crate) fn decode_unguarded(&self) -> Result<Vec<Stmt>, RuntimeError> {
        let bytes = self.encoded.as_deref().ok_or_else(|| {
            RuntimeError::new(format!("{}: no AST to decode", self.source_path))
        })?;
        crate::precomp::decode_stmts(bytes).ok_or_else(|| {
            RuntimeError::new(format!(
                "the precompilation cache entry for {} is unreadable; remove it from the cache directory",
                self.source_path
            ))
        })
    }
}
