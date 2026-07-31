//! ADR-0016 P3: the shared, immutable match subject.
//!
//! A regex/grammar match runs against one `MatchTarget`, created at the
//! engine entry point and shared (two refcount bumps) by the accumulator,
//! the lazy `Match` tree, and every consumer that derives captured text.
//! Recorded spans (`from`, `to`) are absolute char indices into `chars`;
//! captured text is derived on demand via [`MatchTarget::span_str`] instead
//! of being stored per capture node.

use std::sync::Arc;

/// The subject of a regex match: the same string in both the forms consumers
/// need. `text` answers `.orig` with an `Arc` bump; `chars` is the char-index
/// space every recorded span points into, sliced without re-collecting the
/// subject.
#[derive(Clone)]
pub(crate) struct MatchTarget {
    text: Arc<String>,
    chars: Arc<[char]>,
    /// Whole subject is ASCII: char index == byte index, so a span reads as
    /// a byte slice of `text` (a straight memcpy) instead of re-encoding
    /// chars one by one.
    ascii: bool,
}

impl MatchTarget {
    pub(crate) fn new(text: &str) -> Self {
        Self {
            text: Arc::new(text.to_string()),
            chars: text.chars().collect(),
            ascii: text.is_ascii(),
        }
    }

    /// A target for a derived match space — the mark-stripped (`:m`) or
    /// case-folded (`:i`) subject the engine actually matched against. The
    /// chars ARE the space the engine's spans index into; the text form is
    /// synthesized from them.
    pub(crate) fn from_chars(chars: &[char]) -> Self {
        let text: String = chars.iter().collect();
        let ascii = text.is_ascii();
        Self {
            text: Arc::new(text),
            chars: chars.into(),
            ascii,
        }
    }

    /// The whole subject as a string (`.orig`).
    pub(crate) fn text(&self) -> &Arc<String> {
        &self.text
    }

    /// The subject as absolute char positions.
    pub(crate) fn chars(&self) -> &[char] {
        &self.chars
    }

    /// The text of a recorded span, clamped to the subject bounds.
    pub(crate) fn span_str(&self, from: usize, to: usize) -> String {
        let len = self.chars.len();
        let a = from.min(len);
        let b = to.clamp(a, len);
        if self.ascii {
            return self.text[a..b].to_string();
        }
        self.chars[a..b].iter().collect()
    }
}

impl std::fmt::Debug for MatchTarget {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MatchTarget")
            .field("len", &self.chars.len())
            .finish()
    }
}
