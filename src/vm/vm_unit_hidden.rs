//! What a top-level routine's frame hides of the program scope (#13604,
//! ADR-12529 phase 3).
//!
//! A routine declared at a compunit's top level that names lexicals by string
//! (`EVAL '$x'`, `::('$x')`) resolves them through its frame's static link to
//! the program scope (`Env::mark_outer_is_unit`). While a mainline block calls
//! it, the program scope also holds that block's own `my`s and the symbols a
//! block-scoped `use` imported: variables, constants and type names. Neither
//! is in the routine's lexical outer, so the frame hides them
//! (`Env::set_unit_hidden`).

use crate::opcode::CompiledCode;
use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use std::sync::Arc;

impl Interpreter {
    /// Hide from the running frame root, just marked
    /// [`crate::env::Env::mark_outer_is_unit`] for `code`, the program-scope
    /// names `code`'s lexical outer does not declare: the compile-time set of
    /// block-only lexicals (`CompiledCode::unit_hidden_lexicals`) plus the
    /// variables the open block-scoped `use`s imported. A no-op for any code
    /// that is not a top-level routine naming lexicals by string.
    // Cost: O(1) when no block-scoped `use` is open; otherwise O(h + i),
    // h = the block-only lexicals, i = the names the open block imports bound.
    pub(crate) fn hide_unit_names_from_frame(&mut self, code: &CompiledCode) {
        let Some(hidden) = &code.unit_hidden_lexicals else {
            return;
        };
        // A block's import scope (`scope_classes`; a BEGIN preload is not
        // one). An import over a name an enclosing import already bound
        // shadows that binding, which the routine does see: kept visible.
        // Any variable-shaped key counts, an uppercase one too: an imported
        // `constant KONST` or `class Foo` is no more the routine's than a
        // lowercase variable is (ADR-8751). A same-named symbol of the
        // routine's own outer is a shadowed value, so it stays visible.
        let imported: Vec<Symbol> = self
            .module
            .import_scope_stack
            .iter()
            .filter(|scope| scope.scope_classes)
            .flat_map(|scope| {
                scope
                    .imported_env_keys
                    .iter()
                    .filter(|key| {
                        key.with_str(crate::env::is_user_variable_key)
                            && !scope.shadowed_env_values.contains_key(*key)
                    })
                    .copied()
            })
            .collect();
        let hidden = if imported.is_empty() {
            if hidden.is_empty() {
                return;
            }
            Arc::clone(hidden)
        } else {
            let mut all = rustc_hash::FxHashSet::clone(hidden);
            all.extend(imported);
            Arc::new(all)
        };
        self.env_mut().set_unit_hidden(Some(hidden));
    }
}
