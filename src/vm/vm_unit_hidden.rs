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

use crate::meta_ns::MetaNs;
use crate::opcode::CompiledCode;
use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use std::sync::Arc;

impl Interpreter {
    /// Hide from the running frame root, just marked
    /// [`crate::env::Env::mark_outer_is_unit`] for `code`, the program-scope
    /// names `code`'s lexical outer does not declare: the compile-time set of
    /// block-only lexicals (`CompiledCode::unit_hidden_lexicals`) plus the
    /// variables the open block-scoped `use`s imported. A no-op for a
    /// top-level routine that names no lexical by string while no block's
    /// `use` is open.
    // Cost: O(s) when no open block-scoped `use` bound a name, s = open import
    // scopes; otherwise O(s + h + i), h = the block-only lexicals, i = the
    // names the open block imports bound.
    pub(crate) fn hide_unit_names_from_frame(&mut self, code: &CompiledCode) {
        // Any top-level routine hides the open block imports (ADR-8751): a
        // routine that names no lexical by string has no block-only
        // lexicals to hide, but it still reads an imported constant or type
        // name through the program scope.
        let empty;
        let hidden = match &code.unit_hidden_lexicals {
            Some(hidden) => hidden,
            None => {
                if !self.module.import_scope_stack.iter().any(|scope| {
                    scope.scope_classes
                        && !(scope.imported_env_keys.is_empty()
                            && scope.block_bound_keys.is_empty())
                }) {
                    return;
                }
                empty = Arc::new(rustc_hash::FxHashSet::default());
                &empty
            }
        };
        // A block's import scope (`scope_classes`; a BEGIN preload is not
        // one). An import over a name an enclosing import already bound
        // shadows that binding, which the routine does see: kept visible.
        // Any variable-shaped key counts, an uppercase one too: an imported
        // `constant KONST` or `class Foo` is no more the routine's than a
        // lowercase variable is (ADR-8751). A same-named symbol of the
        // routine's own outer is a shadowed value, so it stays visible. So is
        // the block's merge of the module's packages (`MetaNs::ModuleMerge`):
        // the routine's `::('M::Foo')` is not granted by it (#13801).
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
                        key.with_str(|s| {
                            crate::env::is_user_variable_key(s)
                                || s.starts_with(MetaNs::ModuleMerge.prefix())
                                // An imported enum value's own key
                                // namespace (#7914, #14214).
                                || s.starts_with(crate::meta_ns::ENUM_BARE_PREFIX)
                        }) && !scope.shadowed_env_values.contains_key(*key)
                    })
                    .copied()
                    .chain(scope.block_bound_keys.iter().copied())
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
