//! ADR-0016 P3: the shared, immutable match subject.
//!
//! A regex/grammar match runs against one `MatchTarget`, created at the
//! engine entry point and shared (two refcount bumps) by the accumulator,
//! the lazy `Match` tree, and every consumer that derives captured text.
//! Recorded spans (`from`, `to`) are absolute char indices into `chars`;
//! captured text is derived on demand via [`MatchTarget::span_str`] instead
//! of being stored per capture node.

use crate::symbol::Symbol;
use std::sync::Arc;
use std::sync::atomic::{AtomicU32, Ordering};

/// A mark-stripped view of one match subject. `stripped_to_original` maps
/// engine positions back to the original subject, while
/// `original_to_stripped` maps an original boundary into the derived space so
/// a scoped `:ignoremark` match can start at the right place without scanning
/// the map on every atom invocation.
pub(crate) struct StrippedMatchTarget {
    chars: Arc<[char]>,
    stripped_to_original: Arc<[usize]>,
    original_to_stripped: Arc<[usize]>,
}

impl StrippedMatchTarget {
    pub(crate) fn chars(&self) -> &[char] {
        &self.chars
    }

    pub(crate) fn stripped_to_original(&self, pos: usize) -> usize {
        self.stripped_to_original
            .get(pos)
            .copied()
            .unwrap_or_else(|| self.original_to_stripped.len().saturating_sub(1))
    }

    pub(crate) fn original_to_stripped(&self, pos: usize) -> usize {
        self.original_to_stripped
            .get(pos)
            .copied()
            .unwrap_or_else(|| self.original_to_stripped.last().copied().unwrap_or(0))
    }

    pub(crate) fn stripped_map(&self) -> &[usize] {
        &self.stripped_to_original
    }
}

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
    /// The grammar class every cursor of this parse reports as its type, held
    /// as the interned `Symbol` id BIASED BY ONE so that 0 means "none" (a
    /// plain `Match`) — symbol id 0 is itself a legitimate symbol.
    ///
    /// In raku a `Grammar` IS a `Match` subclass and every cursor a parse mints
    /// — the top-level result AND every nested capture, including one produced
    /// by a token inherited from a parent grammar — is of the *invoked*
    /// grammar's type (`H.parse(...)<a>.^name` is `H`, not `G`). That is
    /// exactly a per-parse-run property, which is what a `MatchTarget` already
    /// is: one target per engine entry, shared by every node of the resulting
    /// tree.
    ///
    /// It lives behind a shared `Arc<AtomicU32>` rather than a plain field so
    /// the grammar entry point can stamp it on the finished result *after* the
    /// engine ran, with every already-cloned child target seeing the same
    /// value. Threading a class down through the regex engine instead would
    /// touch every matcher entry point for a value the engine never uses.
    cursor_class: Arc<AtomicU32>,
    /// Lazily materialized once per subject, then shared by all scoped
    /// `:ignoremark` entries and repeated scans against this target.
    stripped: Arc<std::sync::OnceLock<StrippedMatchTarget>>,
}

impl MatchTarget {
    pub(crate) fn new(text: &str) -> Self {
        crate::vm::vm_stats::record_regex_match_target_built();
        Self {
            text: Arc::new(text.to_string()),
            chars: text.chars().collect(),
            ascii: text.is_ascii(),
            cursor_class: Arc::new(AtomicU32::new(0)),
            stripped: Arc::new(std::sync::OnceLock::new()),
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
            cursor_class: Arc::new(AtomicU32::new(0)),
            stripped: Arc::new(std::sync::OnceLock::new()),
        }
    }

    /// Return the subject with combining marks removed, building it only on
    /// the first `:ignoremark` use for this match target.
    pub(crate) fn stripped(&self) -> &StrippedMatchTarget {
        self.stripped.get_or_init(|| {
            let (chars, stripped_to_original) =
                crate::runtime::regex::regex_helpers::strip_marks_text(&self.chars);
            let stripped_len = chars.len();
            let mut original_to_stripped = Vec::with_capacity(self.chars.len() + 1);
            let mut stripped_pos = 0usize;
            for original_pos in 0..=self.chars.len() {
                while stripped_pos < stripped_to_original.len()
                    && stripped_to_original[stripped_pos] < original_pos
                {
                    stripped_pos += 1;
                }
                original_to_stripped.push(stripped_pos.min(stripped_len));
            }
            StrippedMatchTarget {
                chars: chars.into(),
                stripped_to_original: stripped_to_original.into(),
                original_to_stripped: original_to_stripped.into(),
            }
        })
    }

    /// The grammar class cursors of this parse report, or `None` for a plain
    /// regex match (a bare `Match`).
    pub(crate) fn cursor_class(&self) -> Option<Symbol> {
        match self.cursor_class.load(Ordering::Relaxed) {
            0 => None,
            biased => Some(Symbol::from_raw(biased - 1)),
        }
    }

    /// Stamp the grammar class on this parse run. Shared with every target
    /// already cloned from this one, so the whole cursor tree retags at once.
    /// A target that already carries a class keeps it: a grammar whose token
    /// delegates to another grammar must not have its inner cursors relabelled
    /// by the outer parse (and vice versa — first stamp wins, which is the
    /// innermost completed parse).
    pub(crate) fn set_cursor_class(&self, class: Symbol) {
        let _ = self.cursor_class.compare_exchange(
            0,
            class.raw() + 1,
            Ordering::Relaxed,
            Ordering::Relaxed,
        );
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
