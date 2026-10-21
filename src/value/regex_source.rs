//! Regex declaration provenance shared by captures and value rendering.
use super::{Value, ValueView};
use std::sync::Arc;

impl Value {
    /// Attach the verbatim declaration text of a parsed `token`/`rule`/`regex`
    /// to a plain regex value (see [`crate::value::RegexClosure::declared_source`]).
    // Cost: O(p + s), p = pattern length, s = source length.
    pub(crate) fn with_regex_declared_source(&self, text: &str) -> Self {
        match self.view() {
            ValueView::Regex(pattern) => {
                Value::RegexCaptured(Arc::new(crate::value::RegexClosure {
                    pattern: Arc::new(pattern.to_string()),
                    scope: None,
                    source_tree: self.regex_source_tree_arc(),
                    signature: self.regex_signature(),
                    topic: None,
                    declared_source: Some(Arc::from(text)),
                    id: Default::default(),
                    name: Default::default(),
                }))
            }
            _ => self.clone(),
        }
    }

    /// The verbatim declaration text of a grammar `token`/`rule`/`regex`, when
    /// the parser recorded it.
    pub(crate) fn regex_declared_source(&self) -> Option<&str> {
        self.0.regex_declared_source()
    }

    /// Share declaration provenance when a regex captures its defining scope.
    // Cost: O(1).
    pub(crate) fn regex_declared_source_arc(&self) -> Option<Arc<str>> {
        self.0.regex_closure_payload()?.declared_source.clone()
    }

    // Cost: O(n), n = displayed source length.
    pub(super) fn regex_display_source(&self, pattern: &str) -> String {
        match self.regex_declared_source() {
            Some(source) => source.to_string(),
            None => format!("/{pattern}/"),
        }
    }
}
