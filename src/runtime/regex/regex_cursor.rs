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
//! Four pieces make that work, and this module owns all four:
//!
//! 1. `Match.^lookup("!cursor_init")` must find a method whose name begins
//!    with `!`. Those are not Raku private methods — in NQP a leading `!` is
//!    an ordinary part of the identifier, and rakudo lists `!cursor_init`
//!    among `Match.^methods` — so they are exposed here as a small named set
//!    rather than through the `is_private` machinery.
//! 2. `!cursor_init` itself, which builds the cursor.
//! 3. Invoking a `Regex` (or a grammar token method) **on a cursor**, which
//!    means "match at this cursor and report where you got to".
//! 4. `CURSOR_MORE`, which advances a cursor that just matched to the next
//!    match of the *same* regex — the `while` half of the idiom
//!    (`String::Utils`'s `replace-all`, #7931).
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
//! ## `CURSOR_MORE`
//!
//! rakudo's `Cursor.CURSOR_MORE` re-invokes the cursor's own `$!regexsub` on a
//! fresh un-started cursor placed just past the last match, so a caller can
//! walk every match without re-scanning the prefix:
//!
//! ```raku
//! my $global = Match.^lookup("CURSOR_MORE");
//! my $c := /o/($cursor-init(Match, "foo boo", :0c));
//! $c := $global($c) while $c.pos >= 0;   # [1,2] [2,3] [5,6] [6,7] [7,-3]
//! ```
//!
//! Two details of it were measured against rakudo 2026.07 and are load-bearing:
//!
//! - the resumption position is `$!pos`, bumped by one when the last match was
//!   **zero-width** (`$!from == $!pos`) — otherwise `/x*/` would find the same
//!   empty match forever;
//! - the fresh cursor is un-started (`$!from == -1`), so the re-invocation
//!   *scans* forward rather than anchoring, and a run that finds nothing more
//!   yields the ordinary failed cursor (`$!pos == -3`).
//!
//! mutsu therefore has to remember which callable produced a cursor. That is
//! [`crate::value::match_view::CURSOR_REGEXSUB_ATTR`], stamped on the result of
//! every cursor-protocol regex call and re-invoked through the same
//! `Regex.CALL-ME` path the original call took — so an `rx:i//`'s adverbs, or
//! any later fix to how they are honoured, are inherited rather than
//! reconstructed from a pattern string.
//!
//! ## What this does NOT adopt
//!
//! Only the entry points the idiom needs. The rest of the protocol
//! (`!cursor_start`, `!cursor_pass`, `!cursor_capture`, the `$!shared` /
//! `$!braid` state NQP threads through a parse) stays internal to mutsu's own
//! engine: a cursor here is produced complete, never advanced step by step by
//! user code. One consequence is that a cursor mutsu returns carries its
//! captures, where rakudo's carries none until `!reduce` builds the Match —
//! strictly more information, and not something the idiom reads.
//!
//! `CURSOR_MORE` resumes a cursor produced by **a `Regex` called on a cursor**,
//! and only that. Two narrower surfaces are deliberately left out:
//!
//! - an ordinary `"abc".match(/b/)`. In rakudo every `Match` *is* a spent
//!   `Cursor` and carries its `$!regexsub`, so `CURSOR_MORE` works on one;
//!   here the stamp would have to go on every match the engine produces, and
//!   since it rebuilds the Match eagerly that would cost ADR-0016 P5's
//!   laziness on the hottest path in the interpreter for a surface no known
//!   consumer reaches for. Making it free instead means threading the invoked
//!   regex down to the engine entry points — the same objection the
//!   `cursor_class` stamp records, and a change worth its own issue if a
//!   consumer ever needs it.
//! - a grammar *token method* called on a cursor. Its result feeds the
//!   custom-HOW subrule side channel by instance identity
//!   (`regex_token_method`), so stamping the regexsub on it would perturb a
//!   path that has nothing to do with this protocol.
//!
//! `CURSOR_MORE` on either reports that rather than guessing — as it does for
//! a cursor that never ran, or one whose match failed (rakudo dies with a null
//! `$!regexsub` on both of those too).
//!
//! Finally, `.^lookup`'s *return value* is still a plain mutsu `Method`:
//! rakudo answers an `NQPRoutine` for `!cursor_init` and a
//! `Method+{is-implementation-detail}` for `CURSOR_MORE`. Both are callable
//! and that is all the idiom uses; mutsu has no NQP-level routine type to
//! report.

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
pub(crate) const CURSOR_PROTOCOL_METHODS: &[&str] = &["!cursor_init", "CURSOR_MORE"];

/// Is `method_name` one of them? The receiver's own `Match`-ness is the
/// caller's check — `Match` itself, or any grammar, since a grammar IS a
/// `Match` subclass.
pub(crate) fn is_cursor_protocol_method(method_name: &str) -> bool {
    CURSOR_PROTOCOL_METHODS.contains(&method_name)
}

impl Interpreter {
    /// Dispatch a cursor-protocol method. `None` when `method` is not one
    /// mutsu answers for, or when the receiver is not the kind of thing it
    /// applies to, so the caller falls through to normal dispatch.
    pub(crate) fn try_cursor_protocol_method(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        match method {
            "!cursor_init" => self.cursor_init(target, args),
            // `CURSOR_MORE` is an instance method on the cursor itself, so a
            // receiver that is not a Match is somebody else's `CURSOR_MORE`.
            "CURSOR_MORE" if target.is_match_instance() => Some(self.cursor_more(target)),
            _ => None,
        }
    }

    /// `Type.!cursor_init($target, :c($pos))` / `:p($pos)` — build a fresh
    /// cursor over `$target`.
    fn cursor_init(
        &mut self,
        target: &Value,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
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
        let cursor_class = Self::cursor_class_of(cursor);
        let next = self.run_regex_at_cursor(&pattern, &orig, start, anchored, &cursor_class);
        // Remember what produced this cursor, so `CURSOR_MORE` can resume it.
        Some(Ok(Self::stamp_cursor_regexsub(next, regex)))
    }

    /// Record `regexsub` (rakudo's `$!regexsub`) on a cursor. Identity is
    /// preserved: a cursor is handed straight to user code, and the rebuild
    /// the stamp forces must not mint a new `Match` id under it.
    fn stamp_cursor_regexsub(cursor: Value, regexsub: &Value) -> Value {
        cursor
            .match_with_attrs_keeping_id(vec![(
                crate::value::match_view::CURSOR_REGEXSUB_ATTR,
                regexsub.clone(),
            )])
            .unwrap_or(cursor)
    }

    /// The class a cursor reports. `match_dispatch_class` answers only for a
    /// still-lazy Match, and a stamped cursor is an eager `Instance`, so read
    /// the instance's own class first.
    fn cursor_class_of(cursor: &Value) -> String {
        match cursor.view() {
            ValueView::Instance { class_name, .. } => class_name.resolve(),
            _ => cursor.match_dispatch_class().to_string(),
        }
    }

    /// `$cursor.CURSOR_MORE` — the next match of the regex that produced
    /// `cursor`, as a new cursor. See the module doc for the two measured
    /// details (the zero-width bump, and resuming un-started so the
    /// re-invocation scans).
    fn cursor_more(&mut self, cursor: &Value) -> Result<Value, RuntimeError> {
        let from = cursor.match_from().unwrap_or(CURSOR_NOT_STARTED);
        let pos = cursor.match_to().unwrap_or(CURSOR_FAIL_POS);
        let Some(regexsub) = cursor.match_cursor_regexsub() else {
            return Err(RuntimeError::new(
                "CURSOR_MORE: no regex to resume (only a cursor a Regex was called on can be advanced)",
            ));
        };
        if pos < 0 {
            return Err(RuntimeError::new(
                "CURSOR_MORE: cannot advance a cursor whose match failed",
            ));
        }
        let orig = cursor
            .match_orig()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        // A zero-width match would otherwise be found again in the same
        // place, forever; rakudo bumps past it by one.
        let next = if from == pos { pos + 1 } else { pos };
        let cursor_class = Self::cursor_class_of(cursor);
        let fresh = Self::make_cursor_value(&cursor_class, &orig, CURSOR_NOT_STARTED, next, false);
        // Through `CALL-ME`, i.e. the same path `$regex($cursor)` takes, so
        // the resumed call and the original one cannot drift apart.
        self.call_method_with_values(regexsub, "CALL-ME", vec![fresh])
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
