//! A regex parse that splices a Regex value (`<$rx>`), memoized by the
//! identity of the values it read (#10716).
//!
//! The `<$var>` arm reads the variable's value while it parses, so its tree is
//! not a function of the pattern text and the text-keyed memos refuse it
//! (`PARSE_CONSULTED_AMBIENT_STATE`). That made `$s ~~ / <$rx> /` in a loop
//! parse the outer pattern afresh on every match — and with it compile a new
//! `RxProgram`, probe its ASCII tables and build its prefilter, all of which
//! are "once per pattern" state hung off the fresh `RegexPattern`.
//!
//! What the parse depended on is known exactly, though: the text, the package,
//! the token generation, and the bindings of the variables the arm read. This
//! module records those reads while a top-level parse runs (a *window*) and
//! keeps the result keyed by them, so a later parse whose variables are still
//! bound to the very same values reuses the tree (and everything derived from
//! it). A read is recorded as *keyed* only when its whole effect on the tree
//! is a function of the value's identity: a Regex value whose own pattern text
//! is static (`regex_pattern_is_static`), so its parse reads nothing more. Any
//! other ambient read in the window — a Str value, an array, `<~~>` — makes the
//! window *unkeyed*, and the parse is not stored, as before.

use super::*;
use crate::symbol::Symbol;
use std::cell::RefCell;
use std::sync::Arc;

/// One `<$var>` read: the variable name as the arm looked it up, and the value
/// it was bound to. Holding the value keeps its allocation alive, so identity
/// (`Value::same_binding`) can never be confused by address reuse.
#[derive(Clone)]
pub(crate) struct ValueRead {
    name: String,
    value: Value,
}

/// The reads of one window.
#[derive(Default)]
pub(crate) struct ParseReads {
    /// Some read in the window is not keyed by identity.
    unkeyed: bool,
    reads: Vec<ValueRead>,
}

struct KeyedEntry {
    tok_gen: u64,
    reads: Box<[ValueRead]>,
    pattern: Arc<RegexPattern>,
}

type KeyedCache = rustc_hash::FxHashMap<Symbol, rustc_hash::FxHashMap<String, KeyedEntry>>;

/// Per-package entry cap; reaching it clears the bucket (an optimization only).
const VALUE_KEYED_PARSE_CACHE_MAX: usize = 1024;

thread_local! {
    /// The reads of the window in progress; `None` outside any window, where a
    /// read has nothing to be recorded for.
    static PARSE_READS: RefCell<Option<ParseReads>> = const { RefCell::new(None) };

    /// Interpolated pattern text → the tree parsed from it, with the reads it
    /// depended on. One entry per text: a variable rebound to a new value
    /// replaces it.
    static VALUE_KEYED_PARSE_CACHE: RefCell<KeyedCache> =
        RefCell::new(rustc_hash::FxHashMap::default());
}

/// Record a read the window cannot key by (any ambient read other than
/// [`note_value_read`]'s).
// Cost: O(1).
pub(crate) fn note_unkeyed_read() {
    PARSE_READS.with(|r| {
        if let Some(reads) = r.borrow_mut().as_mut() {
            reads.unkeyed = true;
        }
    });
}

/// Record that the parse read `name` bound to `value`.
// Cost: O(n), n = the name's length (one copy).
pub(crate) fn note_value_read(name: &str, value: &Value) {
    PARSE_READS.with(|r| {
        if let Some(reads) = r.borrow_mut().as_mut() {
            reads.reads.push(ValueRead {
                name: name.to_owned(),
                value: value.clone(),
            });
        }
    });
}

/// Can a `<$var>` read of `value`, whose pattern text is `pat_str`, be keyed by
/// the value's identity? Only a Regex value with a static pattern: a Str is
/// re-read as pattern text (and may splice further variables), and a Regex
/// whose text interpolates reads more of the environment as it is parsed.
// Cost: O(p), p = the pattern text's length.
pub(crate) fn is_keyable_read(value: &Value, pat_str: &str) -> bool {
    matches!(
        value.view(),
        crate::value::ValueView::Regex(_) | crate::value::ValueView::RegexWithAdverbs(_)
    ) && super::regex_parse::regex_pattern_is_static(pat_str)
}

/// Open a window; the returned token restores the enclosing one.
// Cost: O(1).
pub(crate) fn begin_window() -> Option<ParseReads> {
    PARSE_READS.with(|r| r.borrow_mut().replace(ParseReads::default()))
}

/// Close the window `begin_window` opened, returning its reads. They are also
/// added to the enclosing window, if any: the enclosing parse depends on them
/// too.
// Cost: O(r), r = the reads recorded in the window.
pub(crate) fn end_window(enclosing: Option<ParseReads>) -> ParseReads {
    PARSE_READS.with(|r| {
        let mut slot = r.borrow_mut();
        let this = slot.take().unwrap_or_default();
        *slot = enclosing.map(|mut outer| {
            outer.unkeyed |= this.unkeyed;
            outer.reads.extend(this.reads.iter().cloned());
            outer
        });
        this
    })
}

impl Interpreter {
    /// The value `<$var_name>` interpolates: the `${name}` fallback and the
    /// deref mirror the bare-`$name` interpolation path (a defining-scope
    /// capture may have boxed the scalar into a shared cell).
    // Cost: O(1) expected, two env probes.
    pub(super) fn regex_value_var(&self, var_name: &str) -> Option<Value> {
        self.env
            .get(var_name)
            .cloned()
            .or_else(|| self.env.get(&format!("${var_name}")).cloned())
            .map(Value::into_deref)
    }

    /// The tree stored for `text` in `package`, if the variables it read are
    /// still bound to the same values. A hit inside an enclosing window records
    /// those reads there.
    // Cost: O(r) expected, r = the entry's reads (an env lookup each).
    pub(super) fn value_keyed_parse_lookup(
        &self,
        package: Symbol,
        text: &str,
        tok_gen: u64,
    ) -> Option<Arc<RegexPattern>> {
        let (pattern, reads) = VALUE_KEYED_PARSE_CACHE.with(|c| {
            let cache = c.borrow();
            let entry = cache.get(&package)?.get(text)?;
            if entry.tok_gen != tok_gen
                || !entry.reads.iter().all(|read| {
                    self.regex_value_var(&read.name)
                        .is_some_and(|now| now.same_binding(&read.value))
                })
            {
                return None;
            }
            Some((Arc::clone(&entry.pattern), entry.reads.clone()))
        })?;
        PARSE_READS.with(|r| {
            if let Some(window) = r.borrow_mut().as_mut() {
                window.reads.extend(reads.iter().cloned());
            }
        });
        Some(pattern)
    }

    /// Store a parse whose window recorded only keyed reads.
    // Cost: O(1) amortized.
    pub(super) fn value_keyed_parse_store(
        package: Symbol,
        text: String,
        tok_gen: u64,
        reads: ParseReads,
        pattern: &Arc<RegexPattern>,
    ) {
        if reads.unkeyed || reads.reads.is_empty() {
            return;
        }
        VALUE_KEYED_PARSE_CACHE.with(|c| {
            let mut cache = c.borrow_mut();
            let bucket = cache.entry(package).or_default();
            if bucket.len() >= VALUE_KEYED_PARSE_CACHE_MAX {
                bucket.clear();
            }
            bucket.insert(
                text,
                KeyedEntry {
                    tok_gen,
                    reads: reads.reads.into_boxed_slice(),
                    pattern: Arc::clone(pattern),
                },
            );
        });
    }
}
