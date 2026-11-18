//! A slang module activates its slang in the program interpreter when the
//! parser reaches its `use` (ADR-17558 slice 4).
//!
//! The load runs with the parse's `$*LANG` bound, so the module's `EXPORT`
//! hook (Slangify's, or one that calls `$*LANG.define_slang` itself) registers
//! its slang exactly once, in the interpreter that will run the unit. What the
//! registration recorded — the grammar-rule overrides, the package declarators
//! and the `EXPORTHOW` declarators the hook bound — is kept per `use`
//! ([`ParseTimeSlangActivation`]) for the parser to read.
//!
//! A slang whose productions run on a slang host (ADR-15762) has this
//! interpreter as its host (slice 4b, `slang_program_host.rs`).

use super::regex_grammar_state::ParseTimeSlangActivation;
use super::*;
use crate::slang_link::{SlangActivation, SlangUseArg};

impl Interpreter {
    /// Run `load`, the parse-time load of the slang module `module` for a `use`
    /// with the literal arguments `args`, with `$*LANG` bound, and record what
    /// the slang registered for the parse.
    // Cost: the load, plus O(r + d + s) to read the registration, r = slang
    // rules, d = slang declarators, s = `EXPORTHOW` stash entries.
    pub(super) fn load_activating_slang(
        &mut self,
        module: &str,
        args: Option<&[Value]>,
        load: impl FnOnce(&mut Self) -> Result<(), RuntimeError>,
    ) -> Result<(), RuntimeError> {
        let rules_before = self.regex_state.defined_slang_rules.len();
        let grammar_roles_before = self.regex_state.slang_grammar_roles.len();
        let action_roles_before = self.regex_state.slang_action_roles.len();
        let declarators_before = self.regex_state.defined_slang_declarators.clone();
        let declares_before = self.exporthow_stash_keywords("DECLARE");
        let supersedes_before = self.exporthow_stash_keywords("SUPERSEDE");
        let lang = Symbol::intern("*LANG");
        let saved_lang = self.env.get_sym(lang).cloned();
        self.env
            .insert_sym(lang, super::slang_activation::comp_lang_instance());
        let saved_depth = self
            .regex_state
            .slang_activation_depth
            .replace(self.module.module_load_stack.len() + 1);
        let result = load(self);
        self.regex_state.slang_activation_depth = saved_depth;
        match saved_lang {
            Some(value) => {
                self.env.insert_sym(lang, value);
            }
            None => {
                self.env.remove_sym(lang);
            }
        }
        result?;
        // What this load's own `define_slang` registered. A slang `use`d by the
        // module's own compile registered for that compile, through a nested
        // call of this function that recorded it and took it out again; the
        // registrations leave the shared lists so an enclosing activation
        // does not count them as its own.
        let rules = self
            .regex_state
            .defined_slang_rules
            .split_off(rules_before.min(self.regex_state.defined_slang_rules.len()));
        let grammar_roles = self
            .regex_state
            .slang_grammar_roles
            .split_off(grammar_roles_before.min(self.regex_state.slang_grammar_roles.len()));
        let action_roles = self
            .regex_state
            .slang_action_roles
            .split_off(action_roles_before.min(self.regex_state.slang_action_roles.len()));
        // A production the slang adds runs on this interpreter, with the roles
        // this load registered (`slang_program_host`).
        let hooked = rules.iter().any(|rule| {
            crate::parser::slang_hook_rule(&rule.name).is_some()
                || crate::parser::slang_action_rule(&rule.name)
                || crate::parser::slang_wrapper_rule(&rule.name)
        });
        let host = hooked.then_some(crate::slang_link::ProgramHost {
            grammar_roles,
            action_roles,
        });
        let mut declarators: Vec<_> = self
            .regex_state
            .defined_slang_declarators
            .iter()
            .filter(|decl| !declarators_before.contains(decl))
            .cloned()
            .collect();
        // What the hook bound into `LEXICAL::<EXPORTHOW>` for the importer.
        for keyword in self.exporthow_stash_keywords("DECLARE") {
            if !declares_before.contains(&keyword)
                && !declarators.iter().any(|d| d.keyword == keyword)
            {
                declarators.push(crate::slang_link::SlangDeclarator {
                    keyword,
                    kind: "class".to_string(),
                    how_type: String::new(),
                });
            }
        }
        let supersede_keywords = self
            .exporthow_stash_keywords("SUPERSEDE")
            .into_iter()
            .filter(|keyword| !supersedes_before.contains(keyword))
            .collect();
        let args = match args {
            Some(values) => match values.iter().map(SlangUseArg::from_literal).collect() {
                Some(args) => Some(args),
                // An argument the activation key cannot spell: the parser
                // finds no activation and falls back to the activation
                // interpreter, as before.
                None => return Ok(()),
            },
            None => None,
        };
        self.regex_state
            .parse_time_slang_activations
            .push(ParseTimeSlangActivation {
                module: module.to_string(),
                args,
                rules,
                declarators,
                supersede_keywords,
                host,
            });
        Ok(())
    }

    /// The slang a parse-time load of `module` activated for a `use` with the
    /// literal arguments `args`.
    // Cost: O(a), a = slangs activated at parse time so far.
    pub(super) fn parse_time_slang_activation(
        &self,
        module: &str,
        args: Option<&[SlangUseArg]>,
    ) -> Option<SlangActivation> {
        let (bare, _) = Self::split_dist_selectors(module);
        let found = self
            .regex_state
            .parse_time_slang_activations
            .iter()
            .find(|a| a.module == bare && a.args.as_deref() == args)?;
        Some(SlangActivation {
            rules: found.rules.clone(),
            declarators: found.declarators.clone(),
            supersede_keywords: found.supersede_keywords.clone(),
            host: found
                .host
                .clone()
                .map(crate::slang_link::SlangHost::Program),
            exports: Vec::new(),
        })
    }
}
