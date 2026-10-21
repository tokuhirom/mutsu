//! Which unit imported each env key (#14140, ADR-8751 stage 1 exit).
//!
//! An import that still binds through the env (an exported variable, a
//! constant or a type the parse-time scan cannot see, a `sub EXPORT` hook's
//! values) records the key it wrote, so the readers that need provenance can
//! tell an imported binding from a declared one: the `MY::` listing, the
//! `EVAL` term names, the `&name` alias probes, the module-name gate and the
//! own-package type probe. The record used to be one process-wide table, so a
//! program's `use M` made `M`'s names count as imported in every other
//! compunit too. Each entry now names the units that imported the key, and a
//! reader asks about the unit the running code belongs to and the units it is
//! nested in (an `EVAL` unit's parents).

use super::*;

/// What [`UnitEnvImports`] records for one env key.
#[derive(Clone)]
pub(crate) struct EnvImport {
    /// The spelling the key has in a lexical pseudo-stash (`$x` for a scalar
    /// stored sigil-less, the bare name for a sigilless term's `\x`).
    pub(crate) display: Symbol,
    /// The units whose code imported the key, in import order.
    pub(crate) units: Vec<Symbol>,
}

/// `ModuleState::unit_env_imports`: env key -> who imported it.
pub(crate) type UnitEnvImports = std::sync::Arc<HashMap<Symbol, EnvImport>>;

/// The spelling env key `key` has in a lexical pseudo-stash.
// Cost: O(|key|).
pub(crate) fn import_display_name(key: &str) -> Symbol {
    if let Some(term) = key.strip_prefix(crate::runtime::term_names::TERM_PREFIX) {
        // A sigilless term's env key (`\Y`); its pad name is bare.
        Symbol::intern(term)
    } else if key.starts_with(['$', '@', '%', '&'])
        || key.chars().next().is_some_and(|c| c.is_uppercase())
    {
        Symbol::intern(key)
    } else {
        Symbol::intern(&format!("${key}"))
    }
}

impl Interpreter {
    /// Record that the running code's unit imported env key `key`.
    // Cost: O(|key| + u), u = units that imported the key already; one
    // copy-on-write clone of the table the first time it changes while a
    // snapshot shares it.
    pub(crate) fn note_unit_env_import(&mut self, key: Symbol) {
        let unit = self.executing_unit_sym_for_module_load();
        if self
            .module
            .unit_env_imports
            .get(&key)
            .is_some_and(|entry| entry.units.contains(&unit))
        {
            return;
        }
        let display = key.with_str(import_display_name);
        crate::runtime::cow_table_mut(&mut self.module.unit_env_imports)
            .entry(key)
            .or_insert_with(|| EnvImport {
                display,
                units: Vec::new(),
            })
            .units
            .push(unit);
    }

    /// Whether the running code's unit, or a unit it is nested in, imported
    /// env key `key`.
    // Cost: O(1) when no unit imported `key`; otherwise O(d * u), d = EVAL
    // nesting of the two anchor units (at most 64), u = the importing units.
    pub(crate) fn env_key_imported_here(&self, key: Symbol) -> bool {
        self.module
            .unit_env_imports
            .get(&key)
            .is_some_and(|entry| self.any_anchor_unit(|unit| entry.units.contains(&unit)))
    }

    /// The env keys the running code's unit, or a unit it is nested in,
    /// imported, with their pseudo-stash spellings.
    // Cost: O(k * d * u), k = recorded keys, d and u as for
    // `env_key_imported_here`.
    pub(crate) fn env_imports_here(&self) -> Vec<(Symbol, Symbol)> {
        self.module
            .unit_env_imports
            .iter()
            .filter(|(_, entry)| self.any_anchor_unit(|unit| entry.units.contains(&unit)))
            .map(|(key, entry)| (*key, entry.display))
            .collect()
    }

    /// Whether `test` holds for a unit the running code belongs to: the
    /// executing unit or the current one (two anchors, as
    /// `module_merged_in_anchor_units` explains), or an `EVAL` unit's
    /// parents.
    // Cost: O(d) calls of `test`, d = EVAL nesting (at most 64 per anchor).
    fn any_anchor_unit(&self, mut test: impl FnMut(Symbol) -> bool) -> bool {
        for anchor in [self.executing_unit_sym_for_module_load(), self.current_unit] {
            let mut unit = Some(anchor);
            for _ in 0..64 {
                let Some(sym) = unit else { break };
                if test(sym) {
                    return true;
                }
                unit = crate::runtime::eval_unit_parent(sym);
            }
        }
        false
    }
}
