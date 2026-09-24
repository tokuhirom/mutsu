//! Whether a silent subrule call (`<.rule>`) has an action to dispatch.
//!
//! A silent call is hidden from `.hash`, so its match node only matters to the
//! grammar-action walk. A childless one (`<.ws>`, `<.CRLF>`) is by far the most
//! frequent call in a grammar, and building, logging and carrying a hidden node
//! for each of them made a 60-row YAMLish parse cost 2.7x the instructions
//! ([#9286](https://github.com/tokuhirom/mutsu/issues/9286)). The node is kept
//! only when the live parse's actions class can actually receive it.

use super::regex_helpers::NamedRegexLookupSpec;
use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use crate::value::ValueView;

impl Interpreter {
    /// True when the live `Grammar.parse(:actions(...))` declares an action
    /// method for this silent subrule, either under its own name or under the
    /// `:sym<...>` variant it matched (`sym`). Outside an action-driven parse
    /// there is nothing to dispatch, so it is false; an actions value that names
    /// no class is answered conservatively with true.
    // Cost: O(m), m = MRO depth of the actions class (one registry probe per
    // level; a `:sym` variant adds a constant number of name probes).
    pub(super) fn silent_subrule_has_action(
        &mut self,
        spec: &NamedRegexLookupSpec,
        sym: Option<&str>,
    ) -> bool {
        let Some(actions) = self.current_grammar_actions.as_ref() else {
            return false;
        };
        let class_sym: Symbol = match actions.view() {
            ValueView::Instance { class_name, .. } => class_name,
            ValueView::Package(name) => name,
            _ => return true,
        };
        let class_name = class_sym.as_str();
        if self.has_user_method_sym(class_name, spec.lookup_sym) {
            return true;
        }
        let Some(sym) = sym else {
            return false;
        };
        let method = self.variant_action_method_name(Some(class_name), &spec.lookup_name, sym);
        self.has_user_method(class_name, &method)
    }
}
