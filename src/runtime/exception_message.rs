//! Deriving an exception instance's human-readable message.
//!
//! raku has exactly one answer to "what does this exception say": `$exc.message`.
//! `Exception.message` merely *defaults* to the stored `$!message` attribute, so
//! a class that overrides it — `method message { $!message //= "…" }`, the
//! common way to compute a message from other attributes — must win everywhere.
//!
//! mutsu used to read the `message` attribute directly in each of the four
//! places that render an exception (`.throw`, `.rethrow`, the native `.throw`
//! fast path and `throws-like`'s matcher). A class that declares `has $.message`
//! and fills it in from its own `method message` therefore reported the literal
//! text `(Any)`: the attribute exists but is undefined until the method runs,
//! and none of those paths ran it. This module is the single place that answers
//! the question for all of them.

use super::Interpreter;
use crate::value::{Value, ValueView};

impl Interpreter {
    /// The message of an exception instance, derived the way raku derives it: a
    /// user-defined `method message` wins over the stored attribute, which is
    /// only the default implementation.
    ///
    /// `None` when the exception carries no message at all (no user method, an
    /// absent or undefined `message` attribute, and no class-specific formatted
    /// message), so callers can fall back to raku's "Died with `<class>`"
    /// rendering instead of stringifying `Any`.
    pub(crate) fn exception_message_text(&mut self, target: &Value) -> Option<String> {
        let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = target.view()
        else {
            return None;
        };
        let cn = class_name.resolve();
        // A user `method message` is the authority: it may compute the text from
        // the exception's other attributes and cache it into `$!message`.
        if self.has_user_method(&cn, "message")
            && let Ok(v) = self.call_method_with_values(target.clone(), "message", vec![])
            && !v.is_nil()
            && !matches!(v.view(), ValueView::Package(_))
        {
            return Some(v.to_string_value());
        }
        {
            let map = attributes.as_map();
            // A declared-but-undefined `has $.message` is not a message — that is
            // the state a computing `method message` starts from.
            if let Some(msg) = map.get("message")
                && !msg.is_nil()
                && !matches!(msg.view(), ValueView::Package(_))
            {
                let text = msg.to_string_value();
                if !text.is_empty() {
                    return Some(text);
                }
            }
            // `X::AdHoc` carries `die`'s argument in `payload`, not `message`.
            if cn == "X::AdHoc"
                && let Some(payload) = map.get("payload")
            {
                let text = payload.to_string_value();
                if !text.is_empty() {
                    return Some(text);
                }
            }
            if let Some(formatted) =
                crate::builtins::exception_message::format_exception_message(&cn, &map)
            {
                return Some(formatted);
            }
        }
        None
    }

    /// True when rendering `method` on this exception instance must go through
    /// the interpreter instead of the pure-value native fast path.
    ///
    /// The native path (`native_method_0arg` → `Value::to_string_value`) can only
    /// read the stored `message` attribute. That is the wrong answer twice over:
    /// a class that COMPUTES its message leaves the attribute undefined until its
    /// `method message` runs, and an exception with no message at all renders as
    /// the literal `(Any)`. Both need the class registry, which only the
    /// interpreter has.
    ///
    /// Kept cheap for the hot `.gist` path: the caller has already matched the
    /// method name, and an instance that is neither named like an exception nor
    /// carries a `message` attribute is rejected on two map/string checks, before
    /// any MRO or registry work.
    pub(crate) fn exception_render_needs_interpreter(&mut self, target: &Value, cn: &str) -> bool {
        let ValueView::Instance { attributes, .. } = target.view() else {
            return false;
        };
        let stored = {
            let map = attributes.as_map();
            let looks_like_exception = cn == "Exception"
                || cn.starts_with("X::")
                || cn.starts_with("CX::")
                || cn.ends_with("Exception")
                || map.contains_key("message");
            if !looks_like_exception {
                return false;
            }
            map.get("message").cloned()
        };
        // A declared-but-UNDEFINED `message` attribute is the one thing the
        // native path reads as a message and must not: it renders `(Any)`. An
        // ABSENT one is fine there — it falls through to `X::AdHoc`'s payload,
        // the class-specific formatted message and the placeholder texts, all of
        // which are pure-value decisions.
        matches!(&stored, Some(v) if v.is_nil() || matches!(v.view(), ValueView::Package(_)))
            || self.has_user_method(cn, "message")
    }

    /// [`Self::exception_message_text`] with raku's fallback for an exception
    /// that has nothing to say: `Exception.gist` renders `Died with <class>`
    /// rather than stringifying the undefined `message` attribute.
    pub(crate) fn exception_message_or_died_with(&mut self, target: &Value) -> String {
        if let Some(msg) = self.exception_message_text(target) {
            return msg;
        }
        match target.view() {
            ValueView::Instance { class_name, .. } => {
                format!("Died with {}", class_name.resolve())
            }
            _ => target.to_string_value(),
        }
    }
}
