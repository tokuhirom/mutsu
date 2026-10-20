//! A block-scoped `use` that runs again replays its first run's effects
//! instead of importing again (ADR-8751 stage 1 exit, #13906).
//!
//! A `use` in a routine body runs on every call of the routine: its
//! `UseModule` fills the block's lexical import set and its `ImportScope`
//! rolls the dynamic part of the import back on exit. In rakudo the import is
//! compile time only. For a module that exports plain routines alone, the
//! import's whole effect is the set it fills (already there after the first
//! run), the routine aliases it notes for the scope's redeclaration checks
//! and the module-merge key that grants `M::name` inside the block; none of
//! it outlives the scope. Re-importing paid the full `import_module` walk and,
//! through the scope's pop, a flush of every name-keyed call cache, per call.
//!
//! The first run of such a `use` is checked for "nothing else": the routine,
//! proto, class and type-alias tables, the pragmas and the env aliases are as
//! they were, every binding of the set is a plain routine (a `sub EXPORT`
//! hook's code value may differ per run), and the only env keys it wrote are
//! module-merge grants. Then what it wrote is recorded, and a later run of a
//! `use` filling the same set writes exactly that and skips the import.

use super::*;
use crate::runtime::lexical_imports::LexicalImport;
use std::sync::Arc;

/// `ModuleState::imported_env_aliases`.
type EnvAliases = Arc<HashMap<Symbol, Symbol>>;

/// `TypeState::package_type_aliases`.
type TypeAliases = Arc<crate::runtime::package_keyed::PackageKeyed<String>>;

/// What a block-scoped `use` wrote that its scope's exit removes again.
pub(crate) struct LexicalImportReplay {
    /// The module-merge env keys, the values the import bound them to and
    /// the display names `record_import_env_key` gave them, if it did.
    env_keys: Vec<(Symbol, Value, Option<Symbol>)>,
    /// The routine aliases it noted (`GLOBAL::name`).
    routine_aliases: Vec<Symbol>,
    /// Those of them the scope lists as its own imports (`MY::`).
    own_routine_imports: Vec<Symbol>,
    /// The routine-alias set before and after the first run. The scope's
    /// exit puts the very `before` table back, so a later run that finds it
    /// installs `after` instead of copying the set to extend it.
    aliases_memo: (Arc<HashSet<Symbol>>, Arc<HashSet<Symbol>>),
    /// The same for the env-alias display names.
    env_aliases_memo: (EnvAliases, EnvAliases),
    /// The same for the package type aliases an imported type name adds
    /// (`Foo` -> `M::Foo` for the importing package). A replay that finds
    /// any other table imports for real instead.
    type_aliases_memo: (TypeAliases, TypeAliases),
}

/// The state a block-scoped `use` must leave unchanged to be replayable.
pub(crate) struct ImportEffectProbe {
    functions: usize,
    proto_functions: usize,
    proto_subs: usize,
    classes_shared: crate::runtime::registry::ClassTable,
    env_aliases: EnvAliases,
    type_aliases: TypeAliases,
    pragmas: (bool, bool, bool, bool),
    aliases_before: Arc<HashSet<Symbol>>,
    env_keys_before: rustc_hash::FxHashSet<Symbol>,
    own_imports_before: rustc_hash::FxHashSet<Symbol>,
    had_export_args: bool,
}

impl Interpreter {
    /// Snapshot what [`Self::record_lexical_import_replay`] compares against.
    // Cost: O(k), k = env keys the open import scope recorded (a clone);
    // `Arc` clones and lengths otherwise.
    pub(crate) fn import_effect_probe(&self) -> ImportEffectProbe {
        let reg = self.registry();
        ImportEffectProbe {
            functions: reg.functions.len(),
            proto_functions: std::sync::Arc::as_ptr(&reg.proto_functions) as usize,
            proto_subs: std::sync::Arc::as_ptr(&reg.proto_subs_snapshot()) as usize,
            classes_shared: reg.classes.clone(),
            env_aliases: Arc::clone(&self.module.imported_env_aliases),
            type_aliases: Arc::clone(&self.types.package_type_aliases),
            pragmas: (
                self.module.strict_mode,
                self.module.fatal_mode,
                self.module.lexical_fatal_mode,
                self.module.monkey_typing,
            ),
            aliases_before: self.module.imported_routine_aliases.clone(),
            env_keys_before: self
                .module
                .import_scope_stack
                .last()
                .map(|top| top.imported_env_keys.clone())
                .unwrap_or_default(),
            own_imports_before: self
                .module
                .import_scope_stack
                .last()
                .map(|top| top.own_routine_imports.clone())
                .unwrap_or_default(),
            had_export_args: self.module.pending_use_export_args.is_some(),
        }
    }

    /// After the first successful run of the `use` filling set `id`: record
    /// its effects for [`Self::replay_lexical_import`] when they are all the
    /// scope's exit removes again, else forget any earlier record.
    // Cost: O(k + a + b), k = env keys the scope recorded, a = routine
    // aliases, b = the set's bindings; once per run that is not replayed.
    pub(crate) fn record_lexical_import_replay(&mut self, id: Symbol, probe: ImportEffectProbe) {
        let replay = self.replayable_effects(id, &probe);
        let table = crate::runtime::cow_table_mut(&mut self.module.lexical_import_replays);
        match replay {
            Some(replay) => {
                table.insert(id, Arc::new(replay));
            }
            None => {
                table.remove(&id);
            }
        }
    }

    fn replayable_effects(&self, id: Symbol, probe: &ImportEffectProbe) -> Option<LexicalImportReplay> {
        // An `EVAL`'s set: each `EVAL` is a compilation of its own. A set
        // holding a `sub EXPORT` hook's values: the hook runs again, and its
        // run notes what the block hides from its callees.
        if id.resolve().contains(crate::runtime::lexical_imports::EVAL_UNIT_MARK)
            || self.module.lexical_import_hook_values.contains_key(&id)
        {
            return None;
        }
        let reg = self.registry();
        let routines_same = !probe.had_export_args
            && reg.functions.len() == probe.functions
            && std::sync::Arc::as_ptr(&reg.proto_functions) as usize == probe.proto_functions
            && std::sync::Arc::as_ptr(&reg.proto_subs_snapshot()) as usize == probe.proto_subs;
        let classes_same = probe.classes_shared.shares_with(&reg.classes);
        let pragmas_same = probe.pragmas
            == (
                self.module.strict_mode,
                self.module.fatal_mode,
                self.module.lexical_fatal_mode,
                self.module.monkey_typing,
            );
        // The env-alias and type-alias tables may change: the scope's exit
        // puts the very `before` tables back, and the replay installs the
        // `after` ones (`env_aliases_memo`, `type_aliases_memo`).
        let unchanged = routines_same && classes_same && pragmas_same;
        if !unchanged {
            return None;
        }
        let set = self.module.lexical_import_sets.get(&id)?;
        if !set.values().all(|b| matches!(b, LexicalImport::Routine(_))) {
            return None;
        }
        let top = self.module.import_scope_stack.last()?;
        let merge = crate::meta_ns::MetaNs::ModuleMerge.prefix();
        let mut env_keys = Vec::new();
        for key in top.imported_env_keys.difference(&probe.env_keys_before) {
            let value = self.env.get_sym(*key)?;
            // Besides the module-merge grants, an imported type name or enum
            // value is the same object on every run. Any other env alias the
            // import wrote (an exported variable, a term, a `&name` hook
            // value) is not replayed.
            if !key.with_str(|k| k.starts_with(merge)) && !Self::replayable_import_value(value) {
                return None;
            }
            let display = self.module.imported_env_aliases.get(key).copied();
            env_keys.push((*key, value.clone(), display));
        }
        let routine_aliases = self
            .module
            .imported_routine_aliases
            .difference(&probe.aliases_before)
            .copied()
            .collect();
        let own_routine_imports = top
            .own_routine_imports
            .difference(&probe.own_imports_before)
            .copied()
            .collect();
        Some(LexicalImportReplay {
            env_keys,
            routine_aliases,
            own_routine_imports,
            aliases_memo: (
                Arc::clone(&probe.aliases_before),
                Arc::clone(&self.module.imported_routine_aliases),
            ),
            env_aliases_memo: (
                Arc::clone(&probe.env_aliases),
                Arc::clone(&self.module.imported_env_aliases),
            ),
            type_aliases_memo: (
                Arc::clone(&probe.type_aliases),
                Arc::clone(&self.types.package_type_aliases),
            ),
        })
    }

    /// Whether an imported env binding to `value` is the same on every run
    /// of the `use`: a type object (a class, role, subset or package), a
    /// parametric role or an enum value.
    // Cost: O(1).
    fn replayable_import_value(value: &Value) -> bool {
        matches!(
            value.view(),
            ValueView::Package(_) | ValueView::Enum { .. } | ValueView::ParametricRole { .. }
        )
    }

    /// Replay the recorded effects of the `use` filling set `id` instead of
    /// importing again. `false`, having done nothing, without a record or
    /// before the set was filled.
    // Cost: O(k), k = recorded env keys, while the alias tables are the ones
    // the first run started from (the scope's exit restores them); O(k + a)
    // otherwise, a = alias-table entries (one copy-on-write clone).
    pub(crate) fn replay_lexical_import(&mut self, id: Symbol) -> bool {
        if !self.module.lexical_import_sets.contains_key(&id) {
            return false;
        }
        let Some(replay) = self.module.lexical_import_replays.get(&id).map(Arc::clone) else {
            return false;
        };
        let (types_before, types_after) = &replay.type_aliases_memo;
        if !Arc::ptr_eq(types_before, types_after) {
            if !Arc::ptr_eq(&self.types.package_type_aliases, types_before) {
                return false;
            }
            self.types.package_type_aliases = Arc::clone(types_after);
        }
        // What `record_import_env_key` records for a key no enclosing import
        // bound, without re-deriving the display name.
        // A key an import visible when the scope opened already bound is
        // shadowed, and the scope's exit puts that binding back, as
        // `record_import_env_key` arranges on a real import.
        if let Some(top) = self.module.import_scope_stack.last_mut() {
            for (key, ..) in &replay.env_keys {
                if top.imported_env_keys.insert(*key)
                    && top.imported_env_aliases.contains_key(key)
                    && let Some(previous) = self.env.get_sym(*key)
                {
                    top.shadowed_env_values.insert(*key, previous.clone());
                }
            }
            top.own_routine_imports
                .extend(replay.own_routine_imports.iter().copied());
        }
        for (key, value, _) in &replay.env_keys {
            self.env.insert_sym(*key, value.clone());
        }
        let (before, after) = &replay.env_aliases_memo;
        if Arc::ptr_eq(&self.module.imported_env_aliases, before) {
            self.module.imported_env_aliases = Arc::clone(after);
        } else if replay.env_keys.iter().any(|(_, _, display)| display.is_some()) {
            let aliases = Arc::make_mut(&mut self.module.imported_env_aliases);
            for (key, _, display) in &replay.env_keys {
                if let Some(display) = display {
                    aliases.insert(*key, *display);
                }
            }
        }
        let (before, after) = &replay.aliases_memo;
        if Arc::ptr_eq(&self.module.imported_routine_aliases, before) {
            self.module.imported_routine_aliases = Arc::clone(after);
        } else if !replay.routine_aliases.is_empty() {
            let aliases = Arc::make_mut(&mut self.module.imported_routine_aliases);
            aliases.extend(replay.routine_aliases.iter().copied());
        }
        // The names the block binds itself stay hidden from a routine
        // written outside it, as on the first run.
        let id_text = id.resolve();
        for name in crate::runtime::lexical_imports::id_section(
            &id_text,
            crate::runtime::lexical_imports::BLOCK_VARS_MARK,
        ) {
            self.note_block_bound_import(&name.resolve());
        }
        true
    }
}
