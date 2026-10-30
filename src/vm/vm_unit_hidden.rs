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
    /// Mark the running frame root, just made for `code` (a routine declared
    /// at a compunit's top level, `CompiledCode::outer_is_unit`), as linked to
    /// that compunit's scope: the program scope for the program's own code,
    /// with the names it must not see hidden; for a loaded module's routine,
    /// nothing past the frame, since the module's file scope lives in the
    /// per-compunit stores ([`crate::env::Env::mark_outer_is_foreign_unit`]).
    // Cost: O(1) for a module's routine; else as
    // [`Self::hide_unit_names_from_frame`].
    pub(crate) fn link_frame_to_code_unit(&mut self, code: &CompiledCode) {
        if self.code_is_foreign_unit(code) {
            self.env_mut().mark_outer_is_foreign_unit();
        } else {
            self.env_mut().mark_outer_is_unit();
        }
        self.hide_unit_names_from_frame(code);
    }

    /// Whether `code` was compiled from a loaded module's file while no
    /// module's code runs in its own scope: a loaded module's code, whose
    /// lexical outer is the module's file scope, never the program scope.
    /// While a module's body runs, its file-scope lexicals are still in the
    /// env it loads in (they move to the per-compunit stores once it has
    /// run), and the routines it calls from its mainline read them there; a
    /// re-run `sub EXPORT` runs over that env again
    /// (`ModuleState::module_scope_runs`).
    ///
    /// Only code that names a lexical by string (`static_name_lookup`: an
    /// `EVAL`, `::('$x')`, `MY::`) takes the link, as ADR-12529 §2.3 builds a
    /// name view only for such code. The rest reads its names through slots,
    /// upvalues and the per-compunit stores; a body the interpreter runs over
    /// its caller's env (a `Proxy` FETCH from `call_sub_value`) still reaches
    /// the program scope through such a routine's frame.
    // TODO: link every module routine this way once no body runs over its
    // caller's env (ADR-12529 phase 4), and decide by the loading module's
    // compunit rather than "any module is loading": while one loads, an
    // already-loaded module's routine it calls still sees the loading scope.
    // Cost: O(1).
    #[inline]
    pub(crate) fn code_is_foreign_unit(&self, code: &CompiledCode) -> bool {
        code.static_name_lookup
            && self.module.module_load_stack.is_empty()
            && self.module.module_scope_runs == 0
            && code.source_file.is_some_and(|file| {
                self.module
                    .module_visibility
                    .loaded_unit_set
                    .contains(&file)
            })
    }

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
                        && !(self
                            .env()
                            .import_layer_keys(scope.import_layer)
                            .is_none_or(|keys| keys.is_empty())
                            && scope.merged_modules.is_empty()
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
                self.env()
                    .import_layer_keys(scope.import_layer)
                    .unwrap_or_default()
                    .into_iter()
                    .filter(|key| {
                        key.with_str(|s| {
                            crate::env::is_user_variable_key(s)
                                // An imported enum value's own key
                                // namespace (#7914, #14214).
                                || s.starts_with(crate::meta_ns::ENUM_BARE_PREFIX)
                        }) && !scope
                            .unit_env_imports
                            .get(key)
                            .is_some_and(|entry| entry.units.contains(&scope.unit))
                    })
                    .chain(scope.block_bound_keys.iter().copied())
                    .chain(
                        scope
                            .merged_modules
                            .iter()
                            .map(|module| MetaNs::ModuleMerge.key(*module)),
                    )
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
