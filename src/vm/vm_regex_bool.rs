//! `Regex.Bool`: boolifying a regex value matches it against a topic.
//!
//! Rakudo's `Regex.Bool` matches against the `$_` of the scope the regex
//! literal was written in, not the caller's: `my &f = { /foo/ }; so f("foo")`
//! is `True`, because the block's `$_` (bound to the argument) is the regex's
//! lexical topic. A literal that escapes a callable body carries a snapshot of
//! that `$_` (`RegexClosure::topic`, filled by `OpCode::LoadRegexClosure`);
//! any other regex falls back to the `$_` visible where it is boolified.
//!
//! Every boolification site — `?$re` / `if $re` (`eval_truthy`), `$re.Bool` /
//! `$re.so` through either method-call opcode — goes through
//! [`Interpreter::regex_bool`], so they cannot disagree about which topic wins.

use crate::runtime::Interpreter;
use crate::value::{Value, ValueView};

impl Interpreter {
    /// Whether `val` is a regex value whose truthiness is a match against a
    /// topic (a regex literal, an adverbed one, or a `regex`/`token` routine).
    pub(crate) fn is_boolifying_regex(val: &Value) -> bool {
        matches!(
            val.view(),
            ValueView::Regex(_)
                | ValueView::RegexWithAdverbs { .. }
                | ValueView::Routine { is_regex: true, .. }
        )
    }

    /// Boolify a regex value (see the module doc). `quiet` coerces an
    /// undefined *implicit* topic silently, as a bare `/regex/` does (see
    /// `quiet_topic_for_regex_match`).
    // Cost: O(m), m = one regex match against the topic.
    pub(crate) fn regex_bool(&mut self, regex: &Value, quiet: bool) -> bool {
        let topic = regex
            .regex_captured_topic()
            .or_else(|| self.env().get("_").cloned())
            .unwrap_or(Value::NIL);
        let topic = if quiet {
            self.quiet_topic_for_regex_match(topic)
        } else {
            topic
        };
        self.vm_smart_match(&topic, regex)
    }

    /// The `$re.Bool` / `$re.so` method forms, shared by `CallMethod` and
    /// `CallMethodMut`. `None` when `method`/`target` is not that call.
    // Cost: O(m), m = one regex match against the topic (O(1) when declined).
    pub(crate) fn try_regex_bool_method(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Value> {
        if !matches!(method, "Bool" | "so")
            || !args.is_empty()
            || !Self::is_boolifying_regex(target)
        {
            return None;
        }
        Some(Value::truth(self.regex_bool(target, false)))
    }
}
