//! What a module's own top-level `use`s bound in its mainline's pad
//! (ADR-8751, `lexical_imports::MODULE_UNIT_MARK`), taken over when its load
//! finishes.
//!
//! The module body runs in the loading scope's env, so those bindings sit
//! under the same env keys as the loader's own lexicals. Like the module's
//! own file-scope `my`s, they leave the env when the load finishes and move
//! into the module's compunit lexicals, where its routines and closures read
//! them. A type also joins the module's import record under its bare name,
//! which its attribute defaults resolve bare names through.

use super::*;

impl Interpreter {
    /// Hand the bindings the finished load's top-level `use`s made over to
    /// the module, restoring `module_unit_bound_imports` to the loader's
    /// `saved` list: each joins `unit_lex_names` (with the loader's value to
    /// restore in `saved_unit_lex`), and a type also `imported`, the
    /// module's import record.
    // Cost: O(b), b = the bindings.
    pub(super) fn take_module_unit_bound_imports(
        &mut self,
        saved: Vec<Symbol>,
        saved_plain_env: &HashMap<Symbol, Value>,
        unit_lex_names: &mut Vec<String>,
        saved_unit_lex: &mut Vec<(String, Option<Value>)>,
        imported: &mut Vec<(String, Value, Option<Value>)>,
    ) {
        let bound = std::mem::replace(&mut self.module.module_unit_bound_imports, saved);
        for key in bound {
            let name = key.resolve();
            if unit_lex_names.contains(&name) {
                continue;
            }
            // A type is known by its bare name too, as its import alias was:
            // a bare type name in the module's attribute defaults and
            // signatures resolves through the module's import record.
            if let Some(spelling) = crate::runtime::term_names::term_spelling(&name)
                && let Some(value) = self.env.get_sym(key)
                && matches!(value.view(), ValueView::Package(_))
            {
                imported.push((spelling.to_string(), value.clone(), None));
            }
            saved_unit_lex.push((name.clone(), saved_plain_env.get(&key).cloned()));
            unit_lex_names.push(name);
        }
    }
}
