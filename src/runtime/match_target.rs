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
    text: Arc<crate::value::StrBody>,
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

/// How many recently matched subjects [`MatchTarget::of_subject`] remembers.
const SUBJECT_CACHE_SLOTS: usize = 4;

/// Subjects shorter than this are copied on the spot rather than cached: the
/// copy is cheaper than a probe, and not holding a `Weak` keeps a short
/// accumulator on the in-place `~=` path (the same trade as
/// `builtins::grapheme_index`'s `CACHE_MIN_BYTES`).
const SUBJECT_CACHE_MIN_BYTES: usize = 256;

/// One remembered subject: the `Str` payload it was built from, held weakly,
/// and the derived forms built for it. The payload is never held strongly,
/// so a cached subject that is dropped frees its text at once.
struct CachedSubject {
    subject: std::sync::Weak<crate::value::StrBody>,
    /// `subject`'s byte pointer and length when it was cached. Compared
    /// against an incoming `&str` only after `subject` upgrades, so they name
    /// a live, immutable buffer (see [`MatchTarget::new`]).
    ptr: *const u8,
    len: usize,
    chars: Arc<[char]>,
    ascii: bool,
    stripped: Arc<std::sync::OnceLock<StrippedMatchTarget>>,
}

thread_local! {
    /// The subjects most recently primed by [`MatchTarget::of_subject`],
    /// most recent first.
    static SUBJECT_CACHE: std::cell::RefCell<Vec<CachedSubject>> =
        const { std::cell::RefCell::new(Vec::new()) };
}

impl MatchTarget {
    /// The target for `text`, reusing the one already built for the same
    /// `Str` payload when an entry point primed it with
    /// [`MatchTarget::of_subject`].
    ///
    /// A hit is an identity test, never a content comparison: the cache holds
    /// each payload's `Weak<StrBody>`, and a remembered pointer is compared only
    /// after the `Weak` upgrades. A `Str` payload is never mutated behind a
    /// live `Weak` — the in-place append path (`str_appended_nfc`) uses
    /// `Arc::get_mut`, which refuses when a `Weak` exists and copies instead —
    /// so an equal pointer and length name exactly the bytes the target was
    /// built from. A `&str` that is not a primed payload misses and is copied, as
    /// before.
    // Cost: O(1) on a hit (a scan of SUBJECT_CACHE_SLOTS entries); O(n) on a
    // miss, n = chars of `text`, to copy and collect the subject.
    pub(crate) fn new(text: &str) -> Self {
        if let Some(hit) = Self::cached(text) {
            return hit;
        }
        Self::build(Arc::new(text.to_string().into()))
    }

    /// The target for a `Str` payload, built at most once while the payload
    /// stays among the last [`SUBJECT_CACHE_SLOTS`] subjects matched on this
    /// thread. Its `.orig` IS `subject` (a refcount bump, no copy), and every
    /// later [`MatchTarget::new`] on the same payload's `&str` shares it —
    /// that is what makes `$s ~~ /rx/` and a `.match(rx, :p($pos))` tokenizer
    /// loop O(1) setup per call instead of copying the whole subject each time.
    // Cost: O(1) when `subject` is cached; otherwise O(n), n = chars of
    // `subject`, to collect its chars once.
    pub(crate) fn of_subject(subject: &Arc<crate::value::StrBody>) -> Self {
        if subject.len() < SUBJECT_CACHE_MIN_BYTES {
            return Self::build(Arc::clone(subject));
        }
        if let Some(hit) = Self::cached(subject.as_str()) {
            return hit;
        }
        let target = Self::build(Arc::clone(subject));
        SUBJECT_CACHE.with(|cache| {
            let mut cache = cache.borrow_mut();
            cache.retain(|entry| entry.subject.strong_count() > 0);
            if cache.len() >= SUBJECT_CACHE_SLOTS {
                cache.pop();
            }
            cache.insert(
                0,
                CachedSubject {
                    subject: Arc::downgrade(subject),
                    ptr: subject.as_ptr(),
                    len: subject.len(),
                    chars: Arc::clone(&target.chars),
                    ascii: target.ascii,
                    stripped: Arc::clone(&target.stripped),
                },
            );
        });
        target
    }

    /// The string a regex entry point matches `subject` against, primed in
    /// the subject cache so the engine's [`MatchTarget::new`] on it shares one
    /// target. A `Str` hands back its own payload (a refcount bump), which is
    /// what lets repeated matches on one string find the same entry.
    // Cost: O(1) for a cached `Str`; otherwise O(n), n = chars of the
    // subject's string form, to build it once.
    pub(crate) fn primed_subject(subject: &crate::value::Value) -> Arc<crate::value::StrBody> {
        let arc = match subject.view() {
            crate::value::ValueView::Str(arc) => Arc::clone(&arc),
            _ => Arc::new(subject.to_string_value().into()),
        };
        Self::of_subject(&arc);
        arc
    }

    fn cached(text: &str) -> Option<Self> {
        if text.len() < SUBJECT_CACHE_MIN_BYTES {
            return None;
        }
        SUBJECT_CACHE.with(|cache| {
            let cache = cache.borrow();
            cache.iter().find_map(|entry| {
                if entry.ptr != text.as_ptr() || entry.len != text.len() {
                    return None;
                }
                let live = entry.subject.upgrade()?;
                if live.as_ptr() != text.as_ptr() || live.len() != text.len() {
                    return None;
                }
                // The text, chars and mark-stripped view are shared; the
                // grammar cursor class is per engine run, so a parse stamping
                // its class cannot relabel a later plain match of this string.
                Some(Self {
                    text: live,
                    chars: Arc::clone(&entry.chars),
                    ascii: entry.ascii,
                    cursor_class: Arc::new(AtomicU32::new(0)),
                    stripped: Arc::clone(&entry.stripped),
                })
            })
        })
    }

    fn build(text: Arc<crate::value::StrBody>) -> Self {
        crate::vm::vm_stats::record_regex_match_target_built();
        Self {
            chars: text.chars().collect(),
            ascii: text.is_ascii(),
            text,
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
            text: Arc::new(text.into()),
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
    pub(crate) fn text(&self) -> &Arc<crate::value::StrBody> {
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
