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
        let wanted = |tags: &rustc_hash::FxHashSet<String>| {
            src_tag == "ALL" || tags.contains(src_tag) || tags.contains("ALL")
        };
        let sub_names: Vec<String> = self
            .module
            .exported_subs
            .get(src_module)
            .map(|subs| {
                subs.iter()
                    .filter(|(_, tags)| wanted(tags))
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
                    .filter(|(_, tags)| {
                        src_tag == "ALL" || tags.contains(src_tag) || tags.contains("ALL")
                    })
                    .map(|(name, _)| name.clone())
                    .collect()
            })
            .unwrap_or_default();
        for name in var_names {
            let Some(value) = self.exported_var_value(src_module, &name) else {
                continue;
            };
            let key = match name.chars().next() {
                Some(sigil @ ('$' | '@' | '%' | '&')) => {
                    format!("{sigil}{dst_module}::{}", &name[1..])
                }
                _ => crate::qualified::qualified_text(
                    &dst_module,
                    crate::runtime::term_names::term_spelling(&name).unwrap_or(&name),
                )
                    .as_str()
                    .to_string(),
            };
            self.set_our_var(key.clone(), value.clone());
            self.env_mut().insert(key, value);
            self.register_exported_var(dst_module.clone(), name, vec![dst_tag.to_string()]);
        }
    }
}
