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

/// What a block-scoped `use` wrote that its scope's exit removes again.
#[derive(Clone)]
pub(crate) struct LexicalImportReplay {
    /// The module-merge env keys, the values the import bound them to and
    /// the display names `record_import_env_key` gave them, if it did.
    env_keys: Vec<(Symbol, Value, Option<Symbol>)>,
    /// The routine aliases it noted (`GLOBAL::name`).
    routine_aliases: Vec<Symbol>,
}

/// The state a block-scoped `use` must leave unchanged to be replayable.
pub(crate) struct ImportEffectProbe {
    functions: usize,
    proto_functions: usize,
    proto_subs: usize,
    classes_shared: crate::runtime::registry::ClassTable,
    env_aliases: usize,
    type_aliases: usize,
    pragmas: (bool, bool, bool, bool),
    aliases_before: std::sync::Arc<HashSet<Symbol>>,
    env_keys_before: HashSet<Symbol>,
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
            env_aliases: self.module.imported_env_aliases.len(),
            type_aliases: std::sync::Arc::as_ptr(&self.types.package_type_aliases) as usize,
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
                table.insert(id, replay);
            }
            None => {
                table.remove(&id);
            }
        }
    }

    fn replayable_effects(&self, id: Symbol, probe: &ImportEffectProbe) -> Option<LexicalImportReplay> {
        let reg = self.registry();
        let unchanged = !probe.had_export_args
            && reg.functions.len() == probe.functions
            && std::sync::Arc::as_ptr(&reg.proto_functions) as usize == probe.proto_functions
            && std::sync::Arc::as_ptr(&reg.proto_subs_snapshot()) as usize == probe.proto_subs
            && probe.classes_shared.shares_with(&reg.classes)
            && self.module.imported_env_aliases.len() == probe.env_aliases
            && std::sync::Arc::as_ptr(&self.types.package_type_aliases) as usize
                == probe.type_aliases
            && probe.pragmas
                == (
                    self.module.strict_mode,
                    self.module.fatal_mode,
                    self.module.lexical_fatal_mode,
                    self.module.monkey_typing,
                );
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
            // An env alias the import wrote (an exported variable, a term,
            // a `&name` hook value) is not replayed.
            if !key.with_str(|k| k.starts_with(merge)) {
                return None;
            }
            let display = self.module.imported_env_aliases.get(key).copied();
            env_keys.push((*key, self.env.get_sym(*key)?.clone(), display));
        }
        let routine_aliases = self
            .module
            .imported_routine_aliases
            .difference(&probe.aliases_before)
            .copied()
            .collect();
        Some(LexicalImportReplay {
            env_keys,
            routine_aliases,
        })
    }

    /// Replay the recorded effects of the `use` filling set `id` instead of
    /// importing again. `false`, having done nothing, without a record or
    /// before the set was filled.
    // Cost: O(k + a), k = recorded env keys, a = recorded routine aliases.
    pub(crate) fn replay_lexical_import(&mut self, id: Symbol) -> bool {
        if !self.module.lexical_import_sets.contains_key(&id) {
            return false;
        }
        let Some(replay) = self.module.lexical_import_replays.get(&id).cloned() else {
            return false;
        };
        // What `record_import_env_key` records for a key no enclosing import
        // bound, without re-deriving the display name.
        for (key, value, display) in replay.env_keys {
            self.env.insert_sym(key, value);
            if let Some(display) = display {
                self.module.imported_env_aliases.insert(key, display);
            }
            if let Some(top) = self.module.import_scope_stack.last_mut() {
                top.imported_env_keys.insert(key);
            }
        }
        if !replay.routine_aliases.is_empty() {
            let aliases = std::sync::Arc::make_mut(&mut self.module.imported_routine_aliases);
            aliases.extend(replay.routine_aliases);
        }
        true
    }
}
