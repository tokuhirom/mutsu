//! Materialize a compiler-selected lexical scope as a pseudo-stash.
use super::*;

impl Interpreter {
    /// Build a pseudo-stash for the exact lexical frame selected by the
    /// compiler. Each entry is `[display-key, bare-name, depth, slot]`; a
    /// non-negative slot is live in this frame, while an outer-frame entry is
    /// resolved through the same captured lexical path as `GetOuterVar`.
    // Cost: O(s + v + r), s = selected lexical entries, v = env overlay entries, r = visible routines.
    pub(in crate::vm) fn exec_get_lexical_stash_op(
        &mut self,
        code: &CompiledCode,
        spec_idx: u32,
        routines: LexicalStashRoutines,
    ) {
        let mut entries: ValueMap = ValueMap::default();
        let Some(ValueView::Array(spec, _)) =
            code.constants.get(spec_idx as usize).map(Value::view)
        else {
            let stash = self.pseudo_stash_hash(entries);
            self.stack.push(stash);
            return;
        };
        for item in spec.items() {
            let ValueView::Array(parts, _) = item.view() else {
                continue;
            };
            if parts.items().len() != 4 {
                continue;
            }
            let (
                ValueView::Str(display),
                ValueView::Str(name),
                ValueView::Int(depth),
                ValueView::Int(slot),
            ) = (
                parts.items()[0].view(),
                parts.items()[1].view(),
                parts.items()[2].view(),
                parts.items()[3].view(),
            )
            else {
                continue;
            };
            let depth = depth.max(0) as usize;
            let value = if slot == crate::opcode::LEXICAL_STASH_EXPORT_SLOT {
                Value::package(Symbol::intern("EXPORT"))
            } else if slot == LEXICAL_STASH_ROUTINE_SLOT
                && let Some(routine) = name.strip_prefix('&')
            {
                // A routine the frame declares (`Compiler::scope_routine_decls`).
                self.resolve_code_var(routine)
            } else if depth == 0 {
                if slot >= 0 {
                    self.locals
                        .get(slot as usize)
                        .cloned()
                        .unwrap_or(Value::NIL)
                } else {
                    self.get_env_with_main_alias(&name).unwrap_or(Value::NIL)
                }
            } else {
                let slot = (slot >= 0).then_some(slot as u32);
                self.get_outer_var(code, &name, depth, slot)
            };
            entries.insert(display.to_string(), value);
        }
        // Imported type/package aliases are lexical names too, but they are
        // maintained in the runtime environment rather than in a compiler
        // scope frame. Preserve those visible package bindings while keeping
        // ordinary scalar entries frame-local; the latter are precisely what
        // makes MY:: stop leaking enclosing lexicals.
        // A nested block's pad holds what its own declarations and `use`s put
        // there (#10626): a loaded module's package the unit bound at file
        // level (`use Test`) is the unit's, not the block's. The block's own
        // `my class` stays.
        let block_pad = !matches!(routines, LexicalStashRoutines::All);
        // `LEXICAL::` sees the packages every enclosing scope binds, which a
        // nested block's env overlay alone does not hold.
        let lexical_view = (!block_pad && self.env().is_scoped()).then(|| self.env().flattened());
        let package_env = lexical_view.as_ref().unwrap_or_else(|| self.env());
        let package_entries: Vec<(Symbol, Value)> = package_env
            .iter()
            .filter(|(_, value)| matches!(value.view(), ValueView::Package(_)))
            .map(|(key, value)| (*key, value.clone()))
            .collect();
        for (key, value) in &package_entries {
            let key = key.resolve();
            // A module's package the BEGIN-time preload bound is a pad entry
            // only where the module is merged (ADR-11136, #13802).
            if self.should_hide_from_my_global_stash(&key)
                || !matches!(value.view(), ValueView::Package(_))
                || self.module_name_hidden_here(&key)
                || (block_pad
                    && self
                        .module
                        .world
                        .loaded_modules
                        .read()
                        .contains(key.as_str()))
            {
                continue;
            }
            let display = Self::add_sigil_prefix(&key);
            entries.entry(display).or_insert_with(|| value.clone());
        }
        // Imports are not compiler declarations, but Raku exposes their
        // aliases through the importing scope's lexical pad. Keep them
        // separate from the flattened environment so `MY::` still excludes
        // enclosing lexicals (and `OUTER::MY::` can see the import). A nested
        // block's pad holds only what its own `use`s imported (#10626).
        let own_imports = match routines {
            LexicalStashRoutines::All => None,
            LexicalStashRoutines::None => Some(None),
            LexicalStashRoutines::OwnImports { skip } => {
                Some(self.import_scopes().len().checked_sub(skip as usize + 1))
            }
        };
        // A package block (`module M { use P5chr; MY::.keys }`) imports into
        // its package but opens no run-time import scope of its own, so its
        // imports are read from the package's import aliases instead.
        let package_block_imports = matches!(routines, LexicalStashRoutines::OwnImports { .. })
            && own_imports == Some(None);
        for (key, display) in self.env_imports_here() {
            let key = &key;
            if let Some(scope) = own_imports
                && !scope.is_some_and(|i| {
                    self.env()
                        .import_layer_binds(self.import_scopes()[i].import_layer, *key)
                })
            {
                continue;
            }
            // No `should_hide_from_my_global_stash` filter here: that set hides
            // the types a module load brought along from the GLOBAL-ish
            // stashes, but a name in this table is one the scope explicitly
            // imported, which its pad does hold (`use M <Type>`).
            let key = key.resolve();
            // An imported term or type need not sit in env under its import
            // key: a sigilless constant lives under its term key, and an
            // exported type resolves through the type registry. Read it the
            // way a bare-word read of the name does.
            let value = self.env().get(&key).cloned().or_else(|| {
                (!key.starts_with(['$', '@', '%', '&']))
                    .then(|| self.imported_term_or_type(&key))
                    .flatten()
            });
            if let Some(value) = value {
                entries
                    .entry(display.resolve().to_string())
                    .or_insert(value);
            }
        }
        // A `use` merges the module's packages into the importing scope's
        // pad: the unit's own for a top-level `use`, the block's for one in a
        // block (#13802).
        let merged_modules: Vec<Symbol> = match own_imports {
            None => self.modules_merged_in_view(),
            Some(Some(scope)) => self.import_scopes()[scope]
                .merged_modules
                .iter()
                .copied()
                .collect(),
            Some(None) => Vec::new(),
        };
        self.add_merged_module_packages(merged_modules, &mut entries);
        match own_imports {
            None => {
                self.add_visible_routines_to_pseudo_stash(&mut entries);
                // Every scope still open sees what its imports bound only in
                // their sets (an `EVAL`'s `sub EXPORT` hook routine).
                for scope in (0..self.import_scopes().len()).rev() {
                    self.add_scope_import_set_code(scope, &mut entries);
                }
            }
            Some(Some(scope)) => self.add_scope_imported_routines(scope, &mut entries),
            Some(None) if package_block_imports => self.add_package_imported_routines(&mut entries),
            Some(None) => {}
        }
        let stash = self.pseudo_stash_hash(entries);
        self.stack.push(stash);
    }

    /// The routine `name` the import sets the block owning
    /// `import_scopes()[scope]` filled bind, or Nil.
    // Cost: O(s), s = the scope's import sets.
    fn scope_hook_routine(&self, scope: usize, name: Symbol) -> Value {
        let sets = &self.import_scopes()[scope].lexical_sets;
        let chain: Vec<Symbol> = sets.iter().rev().copied().collect();
        match self.lexical_import_binding(&chain, name) {
            Some(crate::runtime::lexical_imports::LexicalImport::Code(value)) => value,
            Some(crate::runtime::lexical_imports::LexicalImport::Routine(def)) => {
                self.sub_value_from_function_def((*def).clone())
            }
            None => Value::NIL,
        }
    }

    /// Add the routines the block owning `import_scopes()[scope]` imported.
    // Cost: O(a * p), a = routines that block imported, p = bare-name packages.
    fn add_scope_imported_routines(&self, scope: usize, entries: &mut ValueMap) {
        let packages = self.bare_name_packages_syms();
        for &alias in &self.import_scopes()[scope].own_routine_imports {
            let name = crate::qualified::unqualified_part(alias);
            if !packages
                .iter()
                .any(|&package| crate::qualified::qualified(package, name) == alias)
            {
                continue;
            }
            let key = name;
            let name = name.resolve();
            let mut value = self.resolve_code_var(&name);
            if value.is_nil() {
                // A routine bound only in the scope's lexical import sets
                // (a `sub EXPORT` hook's `&name`) has no env alias.
                value = self.scope_hook_routine(scope, key);
            }
            if !value.is_nil() {
                entries.entry(format!("&{name}")).or_insert(value);
            }
        }
        self.add_scope_import_set_code(scope, entries);
    }

    /// Add the code objects a `use` bound only in the import sets of
    /// `import_scopes()[scope]` (an exported `&name` variable, a `sub EXPORT`
    /// hook's routine), which have no routine alias to list them by: the
    /// sets are the record.
    // Cost: O(b), b = bindings in the scope's import sets.
    fn add_scope_import_set_code(&self, scope: usize, entries: &mut ValueMap) {
        for set in self.import_scopes()[scope].lexical_sets.iter().rev() {
            let Some(bindings) = self.module_lexical_import_set(*set) else {
                continue;
            };
            for (name, binding) in bindings.iter() {
                let value = match binding {
                    crate::runtime::lexical_imports::LexicalImport::Code(value) => value.clone(),
                    // A `my sub ... is export` the module declares: the set
                    // records its definition, not a code object.
                    crate::runtime::lexical_imports::LexicalImport::Routine(def) => {
                        self.sub_value_from_function_def((**def).clone())
                    }
                };
                entries
                    .entry(format!("&{}", name.resolve()))
                    .or_insert(value);
            }
        }
    }

    /// Add the routines imported into the current package (`M::chr` for a
    /// `use` inside `module M { ... }`).
    // Cost: O(a), a = routine imports recorded in this run.
    fn add_package_imported_routines(&self, entries: &mut ValueMap) {
        let package = self.current_package_sym();
        let names: Vec<Symbol> = self
            .module
            .imported_routine_aliases
            .iter()
            .filter(|&&alias| {
                crate::qualified::qualified(package, crate::qualified::unqualified_part(alias))
                    == alias
            })
            .map(|&alias| crate::qualified::unqualified_part(alias))
            .collect();
        for name in names {
            let name = name.resolve();
            let value = self.resolve_code_var(&name);
            if !value.is_nil() {
                entries.entry(format!("&{name}")).or_insert(value);
            }
        }
    }
}
