//! The source text a compilation unit's RakuAST origins index
//! (ADR-10723 §4, "Source positions").
//!
//! A node's `RakuAST::Origin` is `{ from, to }`, character offsets into the
//! unit's source, as in Rakudo. The source is held once per unit as a
//! `RakuAST::Origin::Source` instance that every origin of the unit shares, with
//! a line-start index so `Source.original-line($pos)` is a binary search. The
//! parser records byte ranges ([`crate::ast::SrcSpan`]); the conversion turns
//! them into character offsets here.

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::SrcSpan;
use crate::symbol::Symbol;
use crate::value::Value;

/// The attributes of an `Origin::Source` instance.
pub(crate) const ORIG_ATTR: &str = "orig";
pub(crate) const LINE_STARTS_ATTR: &str = "line-starts";

/// The hidden attribute of an `Origin` instance that holds the line its node
/// began on: what lowering turns back into a line marker.
pub(crate) const LINE_ATTR: &str = "line";

/// One unit's source as the conversion sees it.
pub(super) struct UnitSource {
    /// The shared `RakuAST::Origin::Source` instance.
    source: Value,
    /// Character offset of the first character of each line, ascending.
    line_starts: Vec<i64>,
    /// Byte offset of each character, when the text is not pure ASCII (a byte
    /// offset is then not a character offset). Empty for ASCII text.
    char_starts: Vec<u32>,
    /// Characters in the text.
    chars: i64,
}

thread_local! {
    static CURRENT: RefCell<Option<Rc<UnitSource>>> = const { RefCell::new(None) };
}

/// While alive, `text` is the source the conversion's origins index.
pub(super) struct Scope {
    previous: Option<Rc<UnitSource>>,
}

impl Scope {
    /// Install `text` as the current unit's source.
    // Cost: O(n), n = length of `text` (one pass for the line index).
    pub(super) fn enter(text: &str) -> Scope {
        let unit = Rc::new(UnitSource::new(text));
        let previous = CURRENT.with(|c| c.replace(Some(unit)));
        Scope { previous }
    }
}

impl Drop for Scope {
    fn drop(&mut self) {
        CURRENT.with(|c| *c.borrow_mut() = self.previous.take());
    }
}

impl UnitSource {
    // Cost: O(n), n = length of `text`.
    fn new(text: &str) -> UnitSource {
        // Offsets count graphemes, as Rakudo's do: `\r\n` is one character,
        // and so is a letter with its combining marks.
        let ascii = text.is_ascii() && !text.contains('\r');
        let mut line_starts = vec![0i64];
        let mut char_starts = Vec::new();
        let mut chars = 0i64;
        if ascii {
            for byte in text.bytes() {
                chars += 1;
                if byte == b'\n' {
                    line_starts.push(chars);
                }
            }
        } else {
            use unicode_segmentation::UnicodeSegmentation;
            for (byte, grapheme) in text.grapheme_indices(true) {
                char_starts.push(u32::try_from(byte).unwrap_or(u32::MAX));
                chars += 1;
                if grapheme == "\n" || grapheme == "\r\n" {
                    line_starts.push(chars);
                }
            }
        }
        let mut attrs = HashMap::new();
        attrs.insert(ORIG_ATTR.to_string(), Value::str_from(text));
        attrs.insert(
            LINE_STARTS_ATTR.to_string(),
            Value::array(line_starts.iter().map(|n| Value::int(*n)).collect()),
        );
        UnitSource {
            source: Value::make_instance(Symbol::intern("RakuAST::Origin::Source"), attrs),
            line_starts,
            char_starts,
            chars,
        }
    }

    /// The character offset of byte offset `byte`.
    // Cost: O(1) for ASCII text, O(log n) otherwise, n = characters.
    fn char_of(&self, byte: u32) -> i64 {
        if self.char_starts.is_empty() {
            return i64::from(byte).min(self.chars);
        }
        self.char_starts.partition_point(|&start| start < byte) as i64
    }

    /// The character offset where 1-based `line` begins.
    // Cost: O(1).
    fn line_start(&self, line: i64) -> i64 {
        usize::try_from(line - 1)
            .ok()
            .and_then(|at| self.line_starts.get(at))
            .copied()
            .unwrap_or(self.chars)
    }
}

/// The `RakuAST::Origin` of a statement that began on `line` and occupies
/// `span`. Without a unit source (a frontend round trip that never asks for the
/// origin) or without a recorded span, the range collapses to the start of the
/// line, so the line stays right and nothing is invented.
// Cost: O(1) for ASCII text, O(log n) otherwise, n = characters of the unit.
pub(super) fn origin_value(line: i64, span: SrcSpan) -> Value {
    let unit = CURRENT.with(|c| c.borrow().clone());
    let mut attrs = HashMap::new();
    attrs.insert(LINE_ATTR.to_string(), Value::int(line));
    match unit {
        Some(unit) => {
            let (from, to) = if span.is_some() {
                (unit.char_of(span.from), unit.char_of(span.to))
            } else {
                let at = unit.line_start(line);
                (at, at)
            };
            attrs.insert("from".to_string(), Value::int(from));
            attrs.insert("to".to_string(), Value::int(to));
            attrs.insert("source".to_string(), unit.source.clone());
        }
        None => {
            attrs.insert("from".to_string(), Value::int(line));
            attrs.insert("to".to_string(), Value::int(line));
        }
    }
    Value::make_instance(Symbol::intern("RakuAST::Origin"), attrs)
}
