//! The registration half of `regex_token_closure`: capture the declaring
//! frame's variables each of a grammar's rules names (#17872).

use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// Capture, for every rule `owner` declares, the declaring frame's
    /// variables its pattern names, as shared cells: the rule analogue of
    /// [`Self::capture_declared_method_envs`]. Runs on every entry of the
    /// declaring frame (a kept lexical grammar re-captures), replacing the
    /// rule's previous capture.
    // Cost: O(t + r * (p + s)), t = all registered rules, r = the owner's rules, p = a rule's pattern
    // length, s = the frame's outer lexical slots.
    pub(crate) fn capture_declared_token_envs(
        &mut self,
        code: &CompiledCode,
        owner: &str,
        outer_lexical_slots: &[(Symbol, u32)],
    ) {
        if outer_lexical_slots.is_empty() {
            return;
        }
        let owner_sym = Symbol::intern(owner);
        let rules: Vec<(Symbol, Vec<String>)> = {
            let registry = self.routines();
            registry
                .token_defs
                .iter()
                .filter(|(_, defs)| defs.first().is_some_and(|def| def.package == owner_sym))
                .map(|(_, defs)| {
                    let key = defs
                        .first()
                        .map(|def| crate::qualified::qualified(def.package, def.name));
                    let patterns = defs
                        .iter()
                        .filter_map(|def| {
                            crate::runtime::regex::regex_token_closure::rule_body_pattern(&def.body)
                        })
                        .collect();
                    (key, patterns)
                })
                .filter_map(|(key, patterns)| key.map(|k| (k, patterns)))
                .collect()
        };
        for (key, patterns) in rules {
            self.capture_token_env(code, key, &patterns, outer_lexical_slots);
        }
    }

    /// Capture rule `key`'s declaring scope: the frame variables among
    /// `outer_lexical_slots` that one of its candidate `patterns` names, boxed
    /// into shared cells and kept in `Registry::token_captures`.
    // Cost: O(p * s), p = the patterns' total length, s = the slots.
    pub(crate) fn capture_token_env(
        &mut self,
        code: &CompiledCode,
        key: Symbol,
        patterns: &[String],
        outer_lexical_slots: &[(Symbol, u32)],
    ) {
        {
            let slots: Vec<(Symbol, usize)> = outer_lexical_slots
                .iter()
                .filter(|(sym, _)| {
                    sym.with_str(|name| {
                        // A twigil name (`&?ROUTINE`, `$*dyn`, `$?LINE`) is not
                        // a lexical the rule closes over: its value is the
                        // rule's own or the dynamic caller's.
                        !name
                            .trim_start_matches(['$', '@', '%', '&'])
                            .starts_with(['?', '*', '!', '.', '^', '=', '~'])
                            && crate::env::is_plain_user_lexical(name)
                            && patterns.iter().any(|pattern| {
                                crate::runtime::regex::regex_token_closure::pattern_names_var(
                                    pattern, name,
                                )
                            })
                    })
                })
                .map(|(sym, slot)| (*sym, *slot as usize))
                .collect();
            if slots.is_empty() {
                return;
            }
            // Live, not a snapshot: the rule may run while the frame still
            // does, and a write on either side must reach the other.
            for (_, slot) in &slots {
                self.box_decl_local_cell_any_sigil(code, *slot);
            }
            let entries: Vec<(String, Value)> = slots
                .iter()
                .filter_map(|(sym, slot)| {
                    self.locals
                        .get(*slot)
                        .map(|value| (sym.resolve().to_string(), value.clone()))
                })
                .collect();
            if entries.is_empty() {
                return;
            }
            crate::runtime::regex::regex_token_closure::note_token_capture();
            self.routines_mut()
                .token_captures
                .insert(key, std::sync::Arc::new(entries));
        }
    }
}
