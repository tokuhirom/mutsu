//! Re-exporting another module's whole export tag by binding its stash:
//!
//! ```raku
//! CHECK for NativeCall::EXPORT::.keys {
//!     UNIT::EXPORT::{$_} := NativeCall::EXPORT::{$_};
//! }
//! ```
//!
//! (`NativeLibs` re-exports everything `NativeCall` exports this way, so a
//! file that only says `use NativeLibs` can write `size_t` or `is native`.)
//! The binding makes the destination's `EXPORT::<tag>` stash the source's, so
//! every symbol the source exports under that tag becomes one of the
//! loading module's exports under the destination tag.
use super::*;

impl Interpreter {
    /// The `(module, tag)` an `X::EXPORT::<tag>` package value names.
    pub(crate) fn export_stash_source(value: &Value) -> Option<(String, String)> {
        let ValueView::Package(name) = value.view() else {
            return None;
        };
        let name = name.resolve();
        let name = name.as_str();
        let (module, tag) = name.rsplit_once("::EXPORT::")?;
        (!module.is_empty() && !tag.is_empty() && !crate::qualified::is_qualified_str(tag))
            .then(|| (module.to_string(), tag.to_string()))
    }

    /// Make every symbol `src_module` exports under `src_tag` an export of
    /// the module being loaded under `dst_tag`.
    // Cost: O(s + v), s = the source's exported routine names, v = its exported variables.
    pub(crate) fn reexport_module_tag(&mut self, dst_tag: &str, src_module: &str, src_tag: &str) {
        let Some(dst_module) = self.module.module_load_stack.last().cloned() else {
            return;
        };
        if dst_module == src_module {
            return;
        }
        let tagged = |tag: &String| src_tag == "ALL" || tag == src_tag || tag == "ALL";
        let sub_names: Vec<String> = self
            .module
            .exported_subs
            .get(src_module)
            .map(|subs| {
                subs.iter()
                    .filter(|(_, tags)| tags.iter().any(tagged))
                    .map(|(name, _)| name.clone())
                    .collect()
            })
            .unwrap_or_default();
        for name in sub_names {
            self.register_exported_sub(dst_module.clone(), name, vec![dst_tag.to_string()]);
        }
        let var_names: Vec<String> = self
            .module
            .exported_vars
            .get(src_module)
            .map(|vars| {
                vars.iter()
                    .filter(|(_, tags)| tags.iter().any(tagged))
                    .map(|(name, _)| name.clone())
                    .collect()
            })
            .unwrap_or_default();
        for name in var_names {
            // The value stays with its owner: a copy under the loading module's
            // own qualified name is a package symbol an importer's scope would
            // then have to tell apart from the owner's.
            crate::runtime::cow_table_mut(&mut self.module.reexported_var_owners)
                .insert((dst_module.clone(), name.clone()), src_module.to_string());
            self.register_exported_var(dst_module.clone(), name, vec![dst_tag.to_string()]);
        }
    }
}
