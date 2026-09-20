//! Attaching a deferred body's imports to the package that wrote them.
//!
//! A role's body and an `augment` body are not compunit mainlines: they are
//! recorded at declaration time and re-run later, from whatever scope composes
//! or augments. A `use` inside one therefore misses both halves of the
//! bookkeeping a mainline `use` gets for free:
//!
//! * the module load keys its type aliases by the *compunit* being loaded
//!   (`unit_module_loading_stack`), which names the composing file — handled by
//!   [`crate::runtime::Interpreter::import_target_package`];
//! * `module_scope_lexicals` / `module_imported_lexical_names` are folded in
//!   once, when the importing compunit finishes loading. A role body's `use`
//!   runs *after* its own compunit finished, so its names were never folded in
//!   at all.
//!
//! This module covers the second half. Without it a role that imports an enum
//! and defaults an attribute to one of its values (`use Selkie::Alpha; has
//! AlphaMode $!alpha = AlphaOpaque;`) has no route from the role's package to
//! `AlphaOpaque` at construction time, and the bareword degrades to the plain
//! string `"AlphaOpaque"` (#8842).

use super::Interpreter;

impl Interpreter {
    /// The length of the import log, to be passed back to
    /// [`Self::record_deferred_body_imports`] once the `use` has run.
    pub(crate) fn deferred_body_import_mark(&self) -> usize {
        self.module_imported_names.len()
    }

    /// Fold every name imported since `mark` into `owner`'s package-keyed
    /// lexical scope, exactly as the end of a compunit load folds the mainline's
    /// imports into the packages that compunit declared.
    ///
    /// Additive: the entries stay in `module_imported_names` so the compunit
    /// whose load is composing this body still folds them into its own packages
    /// too. Both scopes really can see the name — the composing file's, because
    /// the import landed in the live `env` there, and the declaring body's,
    /// because that is where the `use` is written.
    pub(crate) fn record_deferred_body_imports(&mut self, owner: &str, mark: usize) {
        if self.module_imported_names.len() <= mark {
            return;
        }
        // `module_imported_names` records the ENV KEY the import landed under;
        // the two scope tables want the name the importing code SPELLS, which
        // for an enum key is its bare form rather than the enum-key namespace
        // key the value lives at (#7914).
        let imported: Vec<(String, crate::value::Value)> = self.module_imported_names[mark..]
            .iter()
            .map(|(name, value, _)| {
                let spelled =
                    match name.strip_prefix(crate::runtime::enum_bare_names::ENUM_BARE_PREFIX) {
                        Some(bare) => bare.to_string(),
                        None => name.clone(),
                    };
                (spelled, value.clone())
            })
            .collect();
        let lexicals = crate::runtime::cow_table_mut(&mut self.module_scope_lexicals)
            .entry(owner.to_string())
            .or_default();
        for (name, value) in &imported {
            lexicals.insert(name.clone(), value.clone());
        }
        let names = crate::runtime::cow_table_mut(&mut self.module_imported_lexical_names)
            .entry(owner.to_string())
            .or_default();
        for (name, _) in &imported {
            let bare = name.strip_prefix(['$', '@', '%']).unwrap_or(name);
            names.entry(bare.to_string()).or_insert(true);
            names.entry(name.clone()).or_insert(true);
        }
    }
}
