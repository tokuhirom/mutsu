//! Named languages a slang registers for its productions to parse with
//! (ADR-17741 §2.2).
//!
//! `$*LANG.define_slang('Grammar::BNF', Grammar::BNF, Grammar::BNF-actions)`
//! records the grammar and actions under the name, in the slang host's
//! interpreter (the one that ran the module's `EXPORT`). The host grammar's
//! `FOREIGN-LANG($name, $rule)` looks the language up here and parses `$rule`
//! with it where the production's cursor stands. Nothing here loads a module:
//! the name is one `define_slang` registered, never a module to search for.

use super::*;

impl Interpreter {
    /// `define_slang($name, $grammar, $actions?)` for a named language: a name
    /// other than `MAIN` with a real grammar rather than a `.^mixin` handle
    /// (`define_slang('Grammar::BNF', Grammar::BNF, Grammar::BNF-actions)`).
    /// Records it and answers whether the call was one; it changes no parser
    /// rule, as only a production that names it reaches it.
    // Cost: O(l), l = registered named languages.
    pub(super) fn record_slang_named_language(&mut self, args: &[Value]) -> bool {
        let name = args.first().map(Value::to_string_value).unwrap_or_default();
        let Some(grammar) = args.get(1) else {
            return false;
        };
        if name == "MAIN" || !matches!(grammar.view(), ValueView::Package(_)) {
            return false;
        }
        let actions = args.get(2).cloned().unwrap_or(Value::NIL);
        let languages = &mut self.regex_state.slang_named_languages;
        languages.retain(|(known, _, _)| *known != name);
        languages.push((name, grammar.clone(), actions));
        true
    }

    /// `__mutsu_slang_named_language($name)`: the `(grammar, actions)` the slang
    /// registered as the language `$name`. An unknown name is a compile-time
    /// error naming it (ADR-0026 §2.2).
    // Cost: O(l), l = registered named languages (a handful).
    pub(super) fn builtin_slang_named_language(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let name = args.first().map(Value::to_string_value).unwrap_or_default();
        self.regex_state
            .slang_named_languages
            .iter()
            .find(|(known, _, _)| *known == name)
            .map(|(_, grammar, actions)| Value::array(vec![grammar.clone(), actions.clone()]))
            .ok_or_else(|| {
                RuntimeError::new(format!(
                    "FOREIGN-LANG: no language named '{name}' was registered with define_slang"
                ))
            })
    }
}
