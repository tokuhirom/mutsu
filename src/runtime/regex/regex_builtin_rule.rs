//! The built-in grammar rules (`ws`, `ww`, `wb`, `ident` and the POSIX-ish
//! character-class rules) as cursor METHODS.
//!
//! The regex engine matches `<.ws>` / `<alpha>` inline, but in Rakudo they are
//! ordinary methods of `Match` (the cursor class every grammar inherits), so a
//! grammar that overrides one can defer to the built-in with `callsame`:
//!
//! ```raku
//! method ws() { $*HIGHWATER = self.pos if self.pos > $*HIGHWATER; callsame }
//! ```
//!
//! (the "good parse errors" idiom of Moritz Lenz's grammar book, used verbatim
//! by `DSL::Shared::Roles::ErrorHandling`). mutsu's grammar MRO holds no
//! `MethodDef` for them, so this module is the final candidate such a deferral
//! reaches.
use super::super::*;
use super::regex_cursor::CURSOR_FAIL_POS;
use super::regex_helpers::{is_word_char, matches_named_builtin, ws_rule_end};

/// Where built-in rule `name` ends when matched at `pos` of `chars`. The outer
/// `None` means `name` is not a built-in rule; `Some(None)` means it is one and
/// does not match here. The one implementation shared by the regex engine's
/// named-subrule fallback and the cursor-method form.
// Cost: O(k), k = the chars the rule consumes (O(1) for the zero-width and
// single-char rules).
pub(crate) fn builtin_rule_end(name: &str, chars: &[char], pos: usize) -> Option<Option<usize>> {
    let before_is_word = || pos > 0 && is_word_char(chars[pos - 1]);
    let after_is_word = || pos < chars.len() && is_word_char(chars[pos]);
    Some(match name {
        "ws" => ws_rule_end(chars, pos),
        "wb" => (before_is_word() != after_is_word()).then_some(pos),
        "ww" => (before_is_word() && after_is_word()).then_some(pos),
        "ident" => chars
            .get(pos)
            .filter(|c| matches_named_builtin("ident", **c))
            .map(|_| {
                let mut end = pos + 1;
                while end < chars.len() && matches_named_builtin("alnum", chars[end]) {
                    end += 1;
                }
                end
            }),
        "alpha" | "upper" | "lower" | "digit" | "xdigit" | "space" | "alnum" | "blank"
        | "cntrl" | "punct" | "graph" | "print" => chars
            .get(pos)
            .filter(|c| matches_named_builtin(name, **c))
            .map(|_| pos + 1),
        _ => return None,
    })
}

impl Interpreter {
    /// Run built-in rule `name` on grammar cursor `invocant` (an instance of a
    /// grammar carrying `orig` and `pos`) and return the resulting cursor: a
    /// copy of the invocant with `from` at the old position and `pos`/`to` at
    /// the end, or at [`CURSOR_FAIL_POS`] when the rule does not match --
    /// rakudo's cursor shape. `None` when `invocant` is no such cursor or
    /// `name` is no built-in rule.
    // Cost: O(k) for the rule (see `builtin_rule_end`) plus O(a) to copy the
    // cursor's a attributes; the subject's chars come from the match-target
    // cache, so a parse in progress does not re-collect them per call.
    pub(crate) fn grammar_builtin_rule_on_cursor(
        &mut self,
        invocant: &Value,
        name: &str,
    ) -> Option<Value> {
        let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = invocant.view()
        else {
            return None;
        };
        if !self.class_is_grammar(&class_name.resolve()) {
            return None;
        }
        let (orig, pos) = {
            let attrs = attributes.as_map();
            let orig = attrs.get("orig")?.clone();
            let pos = attrs
                .get("pos")
                .and_then(|p| p.as_int())
                .filter(|p| *p >= 0)?;
            (orig, pos as usize)
        };
        orig.as_str()?;
        let subject = MatchTarget::primed_subject(&orig);
        let target = MatchTarget::of_subject(&subject);
        let chars = target.chars();
        if pos > chars.len() {
            return None;
        }
        let end = builtin_rule_end(name, chars, pos)?;
        let end = end.map_or(CURSOR_FAIL_POS, |e| e as i64);
        let cursor = Value::make_instance(class_name, attributes.as_map().clone());
        if let ValueView::Instance { attributes, .. } = cursor.view() {
            attributes.insert("from", Value::int(pos as i64));
            attributes.insert("pos", Value::int(end));
            attributes.insert("to", Value::int(end));
        }
        Some(cursor)
    }
}
