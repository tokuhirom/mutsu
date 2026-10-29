//! The names a `sub EXPORT` hook returns, bound in the importing block's
//! lexical import set instead of env aliases (ADR-8751 stage 1, #14140).
//!
//! A hook's export set exists only once the hook has run. The names it spells
//! as literal pair keys are declared by the compiler as the block's own
//! bindings (`Compiler::bind_block_imported_vars`), filled from here through
//! `OpCode::GetHookExport`. Its plain routines are bound in the set as code
//! values. A lowercase term under a key the scan cannot see (`%( $t => ... )`)
//! is kept in the set too; the block's bareword reads find it through their
//! `OpCode::LexImportCall` prefix.

use super::lexical_imports::{LATE_IMPORT_MARK, LexicalImport};
use super::*;

impl Interpreter {
    /// Whether the importing block declares the name a `sub EXPORT` hook
    /// returned under `key` (`$x`, `@a`, `vrai`) itself, and if so keep
    /// `value` for its binding (`OpCode::GetHookExport`): the import then
    /// writes no env alias for it (ADR-8751).
    // Cost: O(|key|).
    pub(crate) fn bind_hook_export_lexically(&mut self, key: &str, value: &Value) -> bool {
        let decl_key = match key.chars().next() {
            Some('$') => key[1..].to_string(),
            Some('@' | '%') => key.to_string(),
            _ => crate::runtime::term_names::term_key(key),
        };
        if !self.block_binds_imported_var(&decl_key) {
            return false;
        }
        self.note_block_bound_import(key.strip_prefix('$').unwrap_or(key));
        if let Some(capture) = self.module.lexical_import_capture.as_mut() {
            capture
                .hook_values
                .insert(Symbol::intern(&decl_key), value.clone());
        }
        true
    }

    /// Whether the running import binds the routine `name` a `sub EXPORT`
    /// hook returned (`'&name' => ...`) in its lexical import set, and if so
    /// keep `value` for it: the import then writes no env alias for it
    /// (ADR-8751). Only a plain routine of the importing compunit's block
    /// qualifies; an operator or a term keeps the env alias, which its
    /// built-in op or bareword reads.
    // Cost: O(|name|).
    pub(crate) fn bind_hook_routine_lexically(&mut self, name: &str, value: &Value) -> bool {
        // `&MAIN` is read by name when the program ends, not through a call
        // site, so it stays an env binding.
        if name.contains(':') || name == "MAIN" || !matches!(value.view(), ValueView::Sub(_)) {
            return false;
        }
        let importer = self.executing_unit_sym_for_module_load();
        let Some(capture) = self.module.lexical_import_capture.as_mut() else {
            return false;
        };
        if capture.unit != importer {
            return false;
        }
        capture
            .hook_routines
            .insert(Symbol::intern(name), value.clone());
        true
    }

    /// Keep the routine a unit-level `use` of a `sub EXPORT` hook module
    /// bound as the env alias `&name`, for [`Self::unit_hook_routine`].
    // Cost: O(|name|).
    pub(crate) fn note_unit_hook_routine(&mut self, name: &str, value: &Value) {
        if name.contains(':') || !matches!(value.view(), ValueView::Sub(_)) {
            return;
        }
        let unit = self.executing_unit_sym_for_module_load();
        self.module
            .unit_hook_routines
            .entry(unit)
            .or_default()
            .insert(Symbol::intern(name), value.clone());
    }

    /// The routine `name` a `sub EXPORT` hook returned to a top-level `use`
    /// of the running unit. The env alias the import writes is the caller
    /// frame's, so a closure of the unit that runs after its routine returned
    /// (a lazy `map` callback) finds the routine here (#14385).
    // Cost: O(1).
    pub(crate) fn unit_hook_routine(&self, name: &str) -> Option<Value> {
        if self.module.unit_hook_routines.is_empty() {
            return None;
        }
        // The chunk that is running names its own unit: a callback recompiled
        // from the importer's source runs under the caller's frames.
        let unit = match self.running_code().and_then(|code| code.source_file) {
            Some(file) => self.unit_of_source_sym(Some(file)),
            None => self.executing_unit_sym(),
        };
        self.module
            .unit_hook_routines
            .get(&unit)?
            .get(&Symbol::intern(name))
            .cloned()
    }

    /// What a top-level `use` of the running code binds `name` to, for a use
    /// written BELOW the call that names it. Raku binds the `use` at compile
    /// time, so a name that resolved nowhere else still reaches it; this is
    /// consulted only where the lookup would report the name as unknown.
    /// The sets come from the code's fallback chains
    /// (`Compiler::register_late_import_fallback`).
    // Cost: O(c + s), c = the code's import chains, s = sets in the fallback chains.
    pub(crate) fn unit_late_import(&self, name: &str) -> Option<LexicalImport> {
        let code = self.running_code()?;
        let name = Symbol::intern(name);
        code.lexical_import_chains
            .iter()
            .filter(|chain| {
                chain
                    .first()
                    .is_some_and(|mark| mark.with_str(|m| m == LATE_IMPORT_MARK))
            })
            .find_map(|chain| self.lexical_import_binding(&chain[1..], name))
    }

    /// The value a `sub EXPORT` hook returned for the declaration key `name`
    /// in the `use` that fills lexical import set `set`.
    // Cost: O(1).
    pub(crate) fn lexical_import_hook_value(&self, set: Symbol, name: Symbol) -> Option<Value> {
        self.module
            .lexical_import_hook_values
            .get(&set)?
            .get(&name)
            .cloned()
    }

    /// Whether the running import keeps the lowercase term a `sub EXPORT`
    /// hook returned under `key` (a key the parse-time scan did not see,
    /// `%( $t => ... )`) in its lexical import set, under the term key, with
    /// no env alias. Only the importing compunit's block-scoped `use`
    /// qualifies; an uppercase key, usually a type that type positions
    /// resolve by name, keeps the env alias.
    // Cost: O(|key|).
    pub(crate) fn bind_hook_term_lexically(&mut self, key: &str, value: &Value) -> bool {
        if !key.starts_with(|c: char| c.is_alphabetic())
            || !key.chars().all(|c| c.is_alphanumeric() || matches!(c, '_' | '-' | '\''))
        {
            return false;
        }
        // An uppercase key usually hands out a type, which type positions
        // resolve by name; only a plain value is a term of the set.
        if key.starts_with(|c: char| c.is_uppercase())
            && matches!(value.view(), ValueView::Package(_) | ValueView::CustomType { .. })
        {
            return false;
        }
        let importer = self.executing_unit_sym_for_module_load();
        let Some(capture) = self.module.lexical_import_capture.as_mut() else {
            return false;
        };
        if capture.unit != importer {
            return false;
        }
        capture.hook_values.insert(
            Symbol::intern(&crate::runtime::term_names::term_key(key)),
            value.clone(),
        );
        // A top-level routine the block calls does not see it, not even
        // through an `EVAL` (`hide_unit_names_from_frame`).
        self.note_block_bound_import(key);
        true
    }

    /// The term `name` a `sub EXPORT` hook bound in one of the import sets
    /// `chain` (innermost first) of the blocks lexically around the reader.
    // Cost: O(c), c = enclosing `use`-blocks.
    pub(crate) fn lexical_import_hook_term(&self, chain: &[Symbol], name: Symbol) -> Option<Value> {
        if self.module.lexical_import_hook_values.is_empty() {
            return None;
        }
        let key = crate::runtime::term_names::term_key_sym(name);
        if self.env.frame_hides_unit_name(key) {
            return None;
        }
        chain.iter().find_map(|id| {
            self.module
                .lexical_import_hook_values
                .get(id)?
                .get(&key)
                .cloned()
        })
    }

    /// The names of the terms a `sub EXPORT` hook bound in the import sets
    /// of the blocks whose import scope is open, for the `EVAL` undeclared-name
    /// check.
    // Cost: O(s + t), s = open import scopes' sets, t = their hook values.
    pub(crate) fn active_lexical_import_hook_terms(&self) -> Vec<String> {
        if self.module.lexical_import_hook_values.is_empty() {
            return Vec::new();
        }
        self.active_lexical_import_chain()
            .iter()
            .filter_map(|id| self.module.lexical_import_hook_values.get(id))
            .flat_map(|values| values.keys())
            .filter(|key| !self.env.frame_hides_unit_name(**key))
            .filter_map(|key| {
                crate::runtime::term_names::term_spelling(&key.resolve()).map(str::to_string)
            })
            .collect()
    }

    /// The names of the terms a `sub EXPORT` hook bound in the import sets
    /// the pre-run of the `EVAL`'d unit `stmts` filled for its top-level
    /// `use`s, for the unit's CHECK-time name checks.
    // Cost: O(s + t), s = the unit's top-level statements, t = their hook values.
    pub(crate) fn eval_unit_hook_terms(&self, stmts: &[Stmt]) -> Vec<String> {
        if self.module.lexical_import_hook_values.is_empty() {
            return Vec::new();
        }
        crate::compiler::Compiler::eval_unit_use_set_ids(stmts)
            .into_iter()
            .flatten()
            .filter_map(|id| self.module.lexical_import_hook_values.get(&id))
            .flat_map(|values| values.keys())
            .filter_map(|key| {
                crate::runtime::term_names::term_spelling(&key.resolve()).map(str::to_string)
            })
            .collect()
    }
}
