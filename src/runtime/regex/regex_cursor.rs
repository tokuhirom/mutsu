//! The NQP cursor protocol, as far as mutsu exposes it to user code (#7883).
//!
//! Rakudo's regex engine drives a regex by hand through a *cursor*: a `Match`
//! that has not finished matching yet. Ecosystem code reaches for the same
//! protocol to match at a known position and read the resulting cursor's
//! `$!from` / `$!pos` instead of going through `~~` and a `Match` object
//! (`String::Utils`'s `replace`/`replace-all` are written entirely in it):
//!
//! ```raku
//! my $cursor-init = Match.^lookup("!cursor_init");
//! my $cursor := /foo/($cursor-init(Match, "xfoox", :0c));
//! say $cursor.pos;   # 4
//! ```
//!
//! Three pieces make that work, and this module owns all three:
//!
//! 1. `Match.^lookup("!cursor_init")` must find a method whose name begins
//!    with `!`. Those are not Raku private methods — in NQP a leading `!` is
//!    an ordinary part of the identifier, and rakudo lists `!cursor_init`
//!    among `Match.^methods` — so they are exposed here as a small named set
//!    rather than through the `is_private` machinery.
//! 2. `!cursor_init` itself, which builds the cursor.
//! 3. Invoking a `Regex` (or a grammar token method) **on a cursor**, which
//!    means "match at this cursor and report where you got to".
//!
//! ## Cursor shape
//!
//! A cursor is a `Match`, not a separate type — verified against rakudo:
//! `Match.^lookup("!cursor_init")(Match, "xfoox", :0c).^name` is `Match`. So
//! mutsu reuses its own `Match` representation, with rakudo's two NQP-level
//! attributes mapped onto it:
//!
//! | rakudo | mutsu `Match` attribute |
//! | --- | --- |
//! | `$!orig` | `orig` |
//! | `$!from` | `from` |
//! | `$!pos`  | `to` (`.pos` already reads `to`) |
//!
//! `$!from == -1` is rakudo's own "this cursor has not started matching"
//! marker, and it is what selects scanning over anchoring — measured against
//! rakudo 2026.07:
//!
//! | init | `$!from` | `$!pos` | `/foo/` on `"xfoox"` |
//! | --- | --- | --- | --- |
//! | `:c(0)` | -1 | 0 | from=1 pos=4 (scanned) |
//! | `:p(0)` | 0 | 0 | from=0 pos=-3 (anchored, failed) |
//! | `:p(1)` | 1 | 1 | from=1 pos=4 (anchored, matched) |
//!
//! A failed cursor is a *failed* `Match` (`.defined` is `True`, `.Bool` is
//! `False`, gists as `#<failed match>`) whose `$!from` is the position the
//! attempt started at and whose `$!pos` is [`CURSOR_FAIL_POS`].
//!
//! ## What this does NOT adopt
//!
//! Only the entry point the idiom needs. The rest of the protocol
//! (`!cursor_start`, `!cursor_pass`, `!cursor_capture`, the `$!shared` /
//! `$!braid` state NQP threads through a parse) stays internal to mutsu's own
//! engine: a cursor here is produced complete, never advanced step by step by
//! user code. One consequence is that a cursor mutsu returns carries its
//! captures, where rakudo's carries none until `!reduce` builds the Match —
//! strictly more information, and not something the idiom reads.

use super::super::*;

/// `$!pos` of a cursor whose match failed. rakudo reports -3 (an NQP
/// backtracking-state marker, not merely "negative"), and consumers test
/// `$pos >= 0`, so any negative value works — this one matches the oracle.
pub(crate) const CURSOR_FAIL_POS: i64 = -3;

/// `$!from` of a cursor that has not started matching (`:c`). Selects
/// scanning; a `:p` cursor carries its anchor position here instead.
pub(crate) const CURSOR_NOT_STARTED: i64 = -1;

/// The cursor-protocol methods mutsu answers for. `.^lookup` / `.^find_method`
/// consult this so `Match.^lookup("!cursor_init")` returns a callable instead
/// of `Nil`.
pub(crate) const CURSOR_PROTOCOL_METHODS: &[&str] = &["!cursor_init"];

/// Is `method_name` one of them? The receiver's own `Match`-ness is the
/// caller's check — `Match` itself, or any grammar, since a grammar IS a
/// `Match` subclass.
pub(crate) fn is_cursor_protocol_method(method_name: &str) -> bool {
    CURSOR_PROTOCOL_METHODS.contains(&method_name)
}

impl Interpreter {
    /// `Type.!cursor_init($target, :c($pos))` / `:p($pos)` — build a fresh
    /// cursor over `$target`. `None` when `method` is not a cursor-protocol
    /// method mutsu answers for, so the caller falls through to normal
    /// dispatch.
    pub(crate) fn try_cursor_protocol_method(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if method != "!cursor_init" {
            return None;
        }
        // The receiver is the type object the cursor should have: `Match` for
        // a plain regex, the grammar's own type for a parse (rakudo: a
        // grammar's cursors are Match objects of the grammar's own type).
        let cursor_class = match target.view() {
            ValueView::Package(name) => name.resolve(),
            ValueView::Instance { class_name, .. } => class_name.resolve(),
            _ => "Match".to_string(),
        };
        if cursor_class != "Match"
            && !self
                .class_mro(&cursor_class)
                .iter()
                .any(|c| c.as_str() == "Match")
        {
            return None;
        }
        let Some(orig) = args
            .iter()
            .find(|a| !matches!(a.view(), ValueView::Pair(..) | ValueView::ValuePair(..)))
        else {
            return Some(Err(RuntimeError::new(
                "!cursor_init requires a target string",
            )));
        };
        let orig = orig.to_string_value();
        let mut from = CURSOR_NOT_STARTED;
        let mut pos = 0i64;
        for arg in args {
            if let ValueView::Pair(key, value) = arg.view() {
                match key.as_str() {
                    // `:c` — continue/scan from here: the cursor has not
                    // started, so calling a regex on it searches forward.
                    "c" => pos = value.to_f64() as i64,
                    // `:p` — anchored at here: the cursor is already at its
                    // start, so calling a regex on it matches exactly there.
                    "p" => {
                        pos = value.to_f64() as i64;
                        from = pos;
                    }
                    _ => {}
                }
            }
        }
        Some(Ok(Self::make_cursor_value(
            &cursor_class,
            &orig,
            from,
            pos,
            false,
        )))
    }

    /// Build a cursor `Match` value directly from its NQP-level attributes.
    /// `failed` marks it as a failed match (`.Bool` is `False`), which is what
    /// a cursor whose regex did not match is.
    fn make_cursor_value(
        cursor_class: &str,
        orig: &str,
        from: i64,
        pos: i64,
        failed: bool,
    ) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("str".to_string(), Value::str_from(""));
        attrs.insert("from".to_string(), Value::int(from));
        attrs.insert("to".to_string(), Value::int(pos));
        // `make_failed_match_value` keeps a redundant `pos` alongside `to`;
        // mirror that shape so every eager Match instance looks the same.
        attrs.insert("pos".to_string(), Value::int(pos));
        attrs.insert("orig".to_string(), Value::str(orig.to_string()));
        attrs.insert("list".to_string(), Value::array(Vec::new()));
        attrs.insert("named".to_string(), Value::hash_bare_values(HashMap::new()));
        if failed {
            attrs.insert("__failed_match__".to_string(), Value::TRUE);
        }
        if cursor_class != "Match" {
            attrs.insert(
                crate::value::match_view::CURSOR_MATCH_MARKER.to_string(),
                Value::TRUE,
            );
        }
        Value::make_instance(Symbol::intern(cursor_class), attrs)
    }

    /// The `(orig, start, anchored)` triple a cursor argument denotes, or
    /// `None` when `value` is not a cursor. `anchored` is rakudo's
    /// `$!from != -1`: a `:p` cursor matches exactly at its position, a `:c`
    /// cursor scans forward from it.
    ///
    /// Only an EXPLICIT `$!from == -1` unanchors. Anchoring is what every
    /// pre-existing caller of a Match-as-cursor means (`regex_token_method`'s
    /// custom-HOW subrule dispatch above all), so a Match that carries no
    /// `from` at all must keep it rather than silently start scanning.
    pub(crate) fn cursor_call_position(value: &Value) -> Option<(String, usize, bool)> {
        if !value.is_match_instance() {
            return None;
        }
        let orig = value.match_orig()?.to_string_value();
        let pos = value.match_to().unwrap_or(0);
        let start = usize::try_from(pos).ok()?;
        let anchored = value.match_from() != Some(CURSOR_NOT_STARTED);
        Some((orig, start, anchored))
    }

    /// `$regex($cursor)` — run a `Regex` value against the cursor and return
    /// the resulting cursor. `None` when the argument is not a cursor, so an
    /// ordinary (unsupported) `Regex.CALL-ME` still reports itself as such.
    pub(crate) fn try_call_regex_on_cursor(
        &mut self,
        regex: &Value,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let pattern = match regex.view() {
            ValueView::Regex(p) => p.to_string(),
            ValueView::RegexWithAdverbs(a) => a.pattern.to_string(),
            _ => return None,
        };
        let cursor = args.first()?;
        let (orig, start, anchored) = Self::cursor_call_position(cursor)?;
        let cursor_class = cursor.match_dispatch_class().to_string();
        Some(Ok(self.run_regex_at_cursor(
            &pattern,
            &orig,
            start,
            anchored,
            &cursor_class,
        )))
    }

    /// Shared body of a cursor call: match `pattern` against `orig` at (or
    /// from) `start`, and wrap the outcome as a cursor.
    pub(crate) fn run_regex_at_cursor(
        &mut self,
        pattern: &str,
        orig: &str,
        start: usize,
        anchored: bool,
        cursor_class: &str,
    ) -> Value {
        let caps = if anchored {
            self.regex_match_with_captures_at(pattern, orig, start)
        } else {
            self.regex_match_with_captures_from(pattern, orig, start)
        };
        match caps {
            Some(caps) => {
                let target = caps
                    .target
                    .clone()
                    .unwrap_or_else(|| MatchTarget::new(orig));
                Value::make_match_object_full(
                    caps.from as i64,
                    caps.to as i64,
                    &caps.positional,
                    &caps.named,
                    target,
                )
            }
            None => Self::cursor_failure(cursor_class, orig, start),
        }
    }

    /// The cursor a failed match yields: rakudo leaves `$!from` at the position
    /// the attempt started from and sets `$!pos` to [`CURSOR_FAIL_POS`].
    pub(crate) fn cursor_failure(cursor_class: &str, orig: &str, start: usize) -> Value {
        Self::make_cursor_value(cursor_class, orig, start as i64, CURSOR_FAIL_POS, true)
    }
}
