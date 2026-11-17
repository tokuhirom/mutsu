//! A `token` / `rule` / `regex` declared in a grammar closes over the scope
//! the grammar is declared in (#17872).
//!
//! ```raku
//! sub mk() { my $m = "q"; my grammar K { token TOP { x <?{ $m eq "q" }> } }; K }
//! say so mk().parse("x");   # True
//! ```
//!
//! A grammar does not match a rule's body value: it resolves the rule by name,
//! re-parses its pattern text and matches the result in the env of whoever
//! runs the match, so a lexical of the declaring routine or block is unbound
//! there once that scope has returned. Methods have the same problem and solve
//! it at registration (`capture_declared_method_envs`): the declaring frame's
//! slots a method reads are boxed into shared cells and kept on the method.
//! Rules get the same capture, kept per rule in [`Registry::token_captures`],
//! and installed for the rule's resolve-and-match window next to its qq
//! thunks ([`Interpreter::install_subrule_token_capture`]), so the pattern's
//! interpolation, its code blocks and its match-time `$var` atoms all read the
//! declaring scope's variables, and a write reaches them.
//!
//! [`Registry::token_captures`]: crate::runtime::registry::Registry::token_captures

use std::sync::atomic::{AtomicBool, Ordering};

use super::super::*;
use super::regex_dynparams::SavedDynParams;

/// Set the first time a rule's declaring scope is captured, so every other
/// program skips the per-subrule lookup behind one relaxed load.
static ANY_TOKEN_CAPTURE: AtomicBool = AtomicBool::new(false);

/// Note that a rule capture exists (see [`ANY_TOKEN_CAPTURE`]).
pub(crate) fn note_token_capture() {
    ANY_TOKEN_CAPTURE.store(true, Ordering::Relaxed);
}

/// The pattern text of a rule body: the regex value its declaration
/// evaluates to.
pub(crate) fn rule_body_pattern(body: &[Stmt]) -> Option<String> {
    body.iter().rev().find_map(|stmt| match stmt {
        Stmt::Expr(Expr::Literal(v)) | Stmt::Return(Expr::Literal(v)) => match v.view() {
            ValueView::Regex(pat) => Some(pat.to_string()),
            ValueView::RegexWithAdverbs(a) => Some(a.pattern.to_string()),
            _ => None,
        },
        _ => None,
    })
}

/// Whether `pattern` names the variable filed under the env key `key` (a
/// scalar's key carries no sigil; `@a`, `%h`, `&c` keep theirs): the sigiled
/// spelling followed by a character that cannot continue the name.
// Cost: O(n), n = the pattern's length.
pub(crate) fn pattern_names_var(pattern: &str, key: &str) -> bool {
    let spelled = if key.starts_with(['@', '%', '&', '$']) {
        std::borrow::Cow::Borrowed(key)
    } else {
        std::borrow::Cow::Owned(format!("${key}"))
    };
    pattern.match_indices(spelled.as_ref()).any(|(at, _)| {
        let rest = &pattern[at + spelled.len()..];
        let mut tail = rest.chars();
        match tail.next() {
            None => true,
            Some(c) if c.is_alphanumeric() || c == '_' => false,
            // `$a-b` is the kebab name `a-b`; `$a-1` is `$a` minus one.
            Some('-' | '\'') => !tail.next().is_some_and(|n| n.is_alphabetic() || n == '_'),
            Some(_) => true,
        }
    })
}

impl Interpreter {
    /// Bind the captured declaring scope of rule candidate `def` in `env`,
    /// for a caller that restores `env` afterwards (the body render in
    /// `eval_token_def`).
    // Cost: O(c), c = the captured variables; one relaxed load when no rule
    // has a capture.
    pub(crate) fn install_token_def_capture(&mut self, def: &FunctionDef) {
        if !ANY_TOKEN_CAPTURE.load(Ordering::Relaxed) {
            return;
        }
        let captured = self
            .registry()
            .token_captures
            .get(&crate::qualified::qualified(def.package, def.name))
            .cloned();
        for (key, value) in captured.iter().flat_map(|c| c.iter()) {
            self.env.insert(key.clone(), value.clone());
        }
    }

    /// Bind rule `name`'s captured declaring scope for its resolve-and-match
    /// window, appending the shadowed bindings to `prior` (restored by
    /// [`Self::restore_subrule_dynamic_params`]). Each captured variable is
    /// the declaring frame's shared cell, so a write in the rule reaches it.
    // Cost: O(c) to bind, c = the captured variables of the rule's
    // candidates, plus the (memoized) candidate lookup; one relaxed load when
    // no rule has a capture.
    pub(crate) fn install_subrule_token_capture(
        &mut self,
        name: &str,
        pkg: Symbol,
        prior: Option<SavedDynParams>,
    ) -> Option<SavedDynParams> {
        if !ANY_TOKEN_CAPTURE.load(Ordering::Relaxed) {
            return prior;
        }
        let defs = self.resolve_token_defs_in_pkg(name, pkg);
        let captured: Vec<(String, Value)> = {
            let registry = self.registry();
            defs.iter()
                .filter_map(|def| {
                    registry
                        .token_captures
                        .get(&crate::qualified::qualified(def.package, def.name))
                        .cloned()
                })
                .flat_map(|entries| entries.iter().cloned().collect::<Vec<_>>())
                .collect()
        };
        if captured.is_empty() {
            return prior;
        }
        let mut saved = prior.unwrap_or_default();
        for (key, value) in captured {
            saved.push((key.clone(), self.env.get(&key).cloned()));
            self.env.insert(key, value);
        }
        Some(saved)
    }
}

#[cfg(test)]
mod tests {
    use super::pattern_names_var;

    #[test]
    fn finds_whole_variable_names() {
        assert!(pattern_names_var("x <?{ $m eq 1 }>", "m"));
        assert!(pattern_names_var("x $m", "m"));
        assert!(!pattern_names_var("x $mm", "m"));
        assert!(pattern_names_var("$a-1", "a"));
        assert!(!pattern_names_var("$a-b", "a"));
        assert!(pattern_names_var("<@w>", "@w"));
        assert!(!pattern_names_var("x", "m"));
    }
}
