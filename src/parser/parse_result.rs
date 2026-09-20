//! Custom parse result types to replace nom dependency.

pub(super) type PResult<'a, T> = Result<(&'a str, T), PError>;

#[derive(Debug, Clone)]
pub(super) struct PError {
    /// Expected-alternative descriptions (without "expected " prefix).
    /// Display joins them as "expected A or B or C".
    ///
    /// `Cow<'static, str>`, not `String`: essentially every one of the 442
    /// construction sites names its alternative with a string literal
    /// (`PError::expected("closing paren")`), and a parse of one ~700-line
    /// module built 525,110 heap copies of those literals -- 28% of every
    /// allocation the process made -- only to drop them again when an
    /// alternative that did match made the error irrelevant (#8830). A parser
    /// that backtracks constructs a failure per rejected alternative, so this
    /// is the one place where the cost of describing an error is paid on the
    /// SUCCESS path.
    pub messages: Vec<std::borrow::Cow<'static, str>>,
    pub remaining_len: Option<usize>,
    /// Optional structured exception (e.g., X::Attribute::Regex) to propagate through parsing.
    pub exception: Option<Box<crate::value::Value>>,
}

/// Sentinel prefix for fatal (non-recoverable) parse errors.
pub(super) const FATAL_PREFIX: &str = "FATAL:";

/// The diagnosis rakudo gives whenever a block was required and not found —
/// `X::Syntax::Missing` with `what => 'block'`, rendered "Missing block". It
/// covers the opening brace (`if 1; 2`, `sub foo-($x) {}`) and the closing one
/// (`{my $x = 2;`) alike. Spelled in the `"X::Type: text"` convention so the
/// class survives to `$!`
/// (`news/2026-08/parse-error-keeps-its-exception-class.md`), and treated
/// specially by [`PError::typed_convention_message`].
pub(crate) const MISSING_BLOCK: &str = "X::Syntax::Missing: Missing block";

/// The diagnosis `check_two_terms_across_lines` (`parser::stmt::modifier`)
/// raises. Shared with `render_parse_error` (#8329): rakudo's `------>`
/// snippet for this specific failure points at the *end of the previous
/// line* (the statement that is actually missing its semicolon), not at the
/// second term that triggered detection -- a position `render_parse_error`
/// has to compute itself from this exact message, since nothing else about a
/// `fatal_at` error distinguishes "echo a different line than the one
/// reported" from the ordinary case.
pub(crate) const TWO_TERMS_ACROSS_LINES: &str =
    "Confused. Two terms in a row across lines (missing semicolon or comma?)";

impl PError {
    /// Check if this is a fatal (non-recoverable) parse error.
    pub fn is_fatal(&self) -> bool {
        self.messages
            .first()
            .is_some_and(|m| m.starts_with(FATAL_PREFIX))
    }
}

impl PError {
    pub fn expected(what: impl Into<std::borrow::Cow<'static, str>>) -> Self {
        PError {
            messages: vec![what.into()],
            remaining_len: None,
            exception: None,
        }
    }

    pub fn expected_at(what: impl Into<std::borrow::Cow<'static, str>>, input: &str) -> Self {
        PError {
            messages: vec![what.into()],
            remaining_len: Some(input.len()),
            exception: None,
        }
    }

    /// Build a PError from a pre-formatted full message (no "expected " prefix added by Display).
    pub fn raw(
        message: impl Into<std::borrow::Cow<'static, str>>,
        remaining_len: Option<usize>,
    ) -> Self {
        PError {
            messages: vec![message.into()],
            remaining_len,
            exception: None,
        }
    }

    /// [`Self::raw`], plus the `what` attribute rakudo's exception carries.
    ///
    /// The `"X::Type: text"` message convention preserves the *class* but
    /// nothing else, so a `throws-like …, X::UnitScope::Invalid, what => "sub"`
    /// matched the class and then died on `No such method 'what'`, aborting the
    /// file (`roast/S06-other/main-semicolon.t`). Stays SOFT — these sites are
    /// best-error candidates the statement dispatcher may still back out of, so
    /// they must not become fatal.
    pub fn raw_with_what(
        message: String,
        remaining_len: Option<usize>,
        class_name: &str,
        what: &str,
    ) -> Self {
        let text = crate::value::RuntimeError::split_typed_message_convention(&message)
            .map(|(_, t)| t)
            .unwrap_or(message.as_str());
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("message".to_string(), crate::value::Value::str_from(text));
        attrs.insert("what".to_string(), crate::value::Value::str_from(what));
        let exception =
            crate::value::Value::make_instance(crate::symbol::Symbol::intern(class_name), attrs);
        PError {
            messages: vec![message.into()],
            remaining_len,
            exception: Some(Box::new(exception)),
        }
    }

    /// Build the SOFT `X::Syntax::InfixInTermPosition` error rakudo raises
    /// when a recognized infix operator token appears where a term was
    /// expected (`my @a = 1, => 2` — `infix` is the operator's literal
    /// spelling, e.g. `"=>"`).
    ///
    /// Deliberately SOFT (unlike [`Self::malformed`]), because term parsing
    /// runs inside many speculative alternatives that must still be free to
    /// abandon a wrong hypothesis. Concretely: `keyword_literal`'s
    /// `BEGIN`/`END`/… phaser-prefix hypothesis calls the term parser on
    /// whatever follows the keyword, so `END => 1` first tries "the `END`
    /// phaser applied to the term `=> 1`" — a hypothesis this function's
    /// caller must be able to reject softly, so the *next* alternative
    /// (`identifier_or_call`, reading plain `END` as a bareword) still gets a
    /// chance to build the correct `BareWord("END") => 1` Pair. A fatal error
    /// here would abort that fallback outright
    /// (`news/2026-08/infix-in-term-position-diagnosis.md`).
    ///
    /// The message follows the `"X::Type: text"` convention so
    /// [`Self::typed_convention_message`] still promotes this diagnosis over
    /// the generic "Confused." fallback once every alternative — including
    /// the correct one — has actually failed.
    pub fn infix_in_term_position(op: &str, input: &str) -> Self {
        let text = format!("Preceding context expects a term, but found infix {op} instead.");
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("infix".to_string(), crate::value::Value::str_from(op));
        attrs.insert("message".to_string(), crate::value::Value::str_from(&text));
        let exception = crate::value::Value::make_instance(
            crate::symbol::Symbol::intern("X::Syntax::InfixInTermPosition"),
            attrs,
        );
        PError {
            messages: vec![format!("X::Syntax::InfixInTermPosition: {text}").into()],
            remaining_len: Some(input.len()),
            exception: Some(Box::new(exception)),
        }
    }

    /// Build a fatal (non-recoverable) parse error.
    /// Fatal errors are not swallowed by the statement dispatcher.
    pub fn fatal(message: String) -> Self {
        PError {
            messages: vec![format!("{}{}", FATAL_PREFIX, message).into()],
            remaining_len: None,
            exception: None,
        }
    }

    /// Build a fatal parse error carrying the failure position (`input` is the
    /// unconsumed rest at the error site), so `parse_program` can report the
    /// source line/column like it does for recoverable errors.
    pub fn fatal_at(message: String, input: &str) -> Self {
        PError {
            messages: vec![format!("{}{}", FATAL_PREFIX, message).into()],
            remaining_len: Some(input.len()),
            exception: None,
        }
    }

    /// Build a fatal parse error with a structured exception.
    pub fn fatal_with_exception(message: String, exception: Box<crate::value::Value>) -> Self {
        PError {
            messages: vec![format!("{}{}", FATAL_PREFIX, message).into()],
            remaining_len: None,
            exception: Some(exception),
        }
    }

    /// [`PError::fatal_with_exception`] carrying the failure position, the way
    /// [`PError::fatal_at`] does for a plain fatal (`input` is the unconsumed
    /// rest at the error site).
    ///
    /// A structured exception does not otherwise record where it was raised, so
    /// without this the diagnosis renders with no `line`/`column` and no
    /// `------>` echo — and `render_parse_error` also copies the computed
    /// position onto the exception's own attributes, so `$!.line` / `$!.column`
    /// stay unset too.
    pub fn fatal_with_exception_at(
        message: String,
        exception: Box<crate::value::Value>,
        input: &str,
    ) -> Self {
        PError {
            messages: vec![format!("{}{}", FATAL_PREFIX, message).into()],
            remaining_len: Some(input.len()),
            exception: Some(exception),
        }
    }

    /// Build the fatal `X::Syntax::Malformed` rakudo throws when a construct is
    /// recognised but its body cannot be read — `Malformed initializer`,
    /// `Malformed class-qualified postfix call`, ... `what` is both the tail of
    /// the message and the exception's `.what` attribute, which the roast tests
    /// match on.
    ///
    /// These are always *fatal*: the construct's opener is the commit point, so
    /// letting the alternative backtrack only loses the diagnosis to the
    /// parser's generic "Confused."
    pub fn malformed(what: &str) -> Self {
        let message = format!("X::Syntax::Malformed: Malformed {}", what);
        let mut attrs = std::collections::HashMap::new();
        attrs.insert(
            "message".to_string(),
            crate::value::Value::str(format!("Malformed {}", what)),
        );
        attrs.insert(
            "what".to_string(),
            crate::value::Value::str(what.to_string()),
        );
        let exception = crate::value::Value::make_instance(
            crate::symbol::Symbol::intern("X::Syntax::Malformed"),
            attrs,
        );
        PError::fatal_with_exception(message, Box::new(exception))
    }

    /// Build the fatal `X::Comp::Group` rakudo throws when one construct draws
    /// *two* complaints: a specific diagnosis plus the fatal one it leads to.
    ///
    /// rakudo's compiler accumulates worries, sorrows and at most one panic, and
    /// only collapses to a single exception when it collected exactly one thing
    /// (a lone panic, or a lone sorrow with no worries). Anything else is an
    /// `X::Comp::Group` — which is why `throws-like 'say', X::Comp::Group` is the
    /// right expectation for a bare `say`: the "Unsupported use of bare say"
    /// advice is a worry and the parse then panics on the missing argument.
    ///
    /// Use this only where rakudo genuinely collects two complaints. A site
    /// reproducing a lone rakudo panic (or a lone sorrow) must keep throwing
    /// that exception directly: `my Int $a of Str` is
    /// `X::Syntax::Variable::ConflictingTypes`, and only the double-`of` form,
    /// which sorrows twice, is a group.
    pub fn comp_group(
        complaint: crate::value::Value,
        is_worry: bool,
        panic_message: &str,
        message: String,
    ) -> Self {
        Self::comp_group_with_panic(
            complaint,
            is_worry,
            crate::value::Value::make_exception(
                "X::Comp::AdHoc",
                &[
                    (
                        "message",
                        crate::value::Value::str(panic_message.to_string()),
                    ),
                    (
                        "payload",
                        crate::value::Value::str(panic_message.to_string()),
                    ),
                ],
            ),
            message,
        )
    }

    /// [`Self::comp_group`] for a panic that has a more specific class than
    /// `X::Comp::AdHoc` — rakudo raises `X::Comp::FailGoal` when it ran off the
    /// end of the input looking for a closing delimiter, and carries the goal it
    /// was after.
    pub fn comp_group_with_panic(
        complaint: crate::value::Value,
        is_worry: bool,
        panic: crate::value::Value,
        message: String,
    ) -> Self {
        let (sorrows, worries) = if is_worry {
            (Vec::new(), vec![complaint])
        } else {
            (vec![complaint], Vec::new())
        };
        let group =
            crate::value::Value::make_comp_group(message.clone(), Some(panic), sorrows, worries);
        Self::fatal_with_exception(message, Box::new(group))
    }

    /// Build the fatal `X::Obsolete` parse error for a Perl 5 construct.
    ///
    /// `old` names the construct and `replacement` the Raku spelling; rakudo
    /// renders both into the message *and* exposes them as `.old`/`.replacement`,
    /// which `throws-like 'qr/a/', X::Obsolete, old => …, replacement => …`
    /// reads. Every obsolete-syntax rejection goes through here so none of them
    /// arrives as a bare message with no attributes to match on.
    pub fn obsolete(old: &str, replacement: &str) -> Self {
        Self::from_typed(crate::value::RuntimeError::obsolete(old, replacement))
    }

    /// Turn a typed [`crate::value::RuntimeError`] into a fatal parse error that
    /// keeps its exception object.
    ///
    /// The `RuntimeError` constructors in `src/value/error_typed.rs` are the one
    /// place a given `X::` class's attributes and message are spelled out; a
    /// parse-time raise of the same class goes through here instead of
    /// re-deriving them, so the two cannot drift apart. A caller must pass a
    /// *typed* error — an untyped one degrades to a plain fatal message.
    pub fn from_typed(err: crate::value::RuntimeError) -> Self {
        let message = err.message.to_string();
        match err.exception {
            Some(exception) => Self::fatal_with_exception(message, exception),
            None => Self::fatal(message),
        }
    }

    /// [`Self::obsolete`] that also records the failure position (`input` is the
    /// unconsumed rest at the error site), like [`Self::fatal_at`].
    pub fn obsolete_at(old: &str, replacement: &str, input: &str) -> Self {
        let mut err = Self::obsolete(old, replacement);
        err.remaining_len = Some(input.len());
        err
    }

    /// The first alternative written in the `"X::Type: text"` convention, if
    /// any. Such a message is a *diagnosis* — the parser recognised the
    /// construct and knows which Raku exception class rejects it — so a caller
    /// that would otherwise flatten this error into a generic "expected …"
    /// description should propagate it instead. Losing it downgrades the
    /// exception to `X::Syntax::Confused`.
    ///
    /// [`MISSING_BLOCK`] is special-cased twice over, because "a block was
    /// required here" is the weakest diagnosis the parser has — a block is an
    /// alternative almost everywhere:
    ///
    /// * any *other* named class describes the construct better and wins;
    /// * on its own it counts only when the block was the *primary* expectation
    ///   at this position, i.e. the first alternative. `say 1 ]` fails with a
    ///   hundred alternatives of which "block" is merely one, and rakudo calls
    ///   that `X::Syntax::Confused`, not `X::Syntax::Missing`.
    pub fn typed_convention_message(&self) -> Option<&str> {
        fn typed(m: &str) -> Option<&str> {
            crate::value::RuntimeError::split_typed_message_convention(m).map(|_| m)
        }
        self.messages
            .iter()
            .filter(|m| m.as_ref() != MISSING_BLOCK)
            .find_map(|m| typed(m))
            .or_else(|| {
                self.messages
                    .first()
                    .filter(|m| m.as_ref() == MISSING_BLOCK)
                    .map(|m| m.as_ref())
            })
    }

    /// Get the formatted message string (used by tests).
    #[allow(dead_code)]
    pub fn message(&self) -> String {
        format!("{}", self)
    }

    pub fn consumed_from(&self, total_len: usize) -> Option<usize> {
        self.remaining_len
            .map(|remaining| total_len.saturating_sub(remaining.min(total_len)))
    }
}

pub(super) fn error_score(err: &PError, input_len: usize) -> usize {
    err.consumed_from(input_len).unwrap_or(0)
}

fn strip_expected_prefix(s: &str) -> &str {
    s.strip_prefix("expected ").unwrap_or(s)
}

/// Merge a context description with existing message parts.
/// `context` may optionally have an "expected " prefix (which is stripped).
///
/// A **fatal** error is returned verbatim. Its message is a diagnosis, not one
/// alternative among many, and [`PError::is_fatal`] only inspects the first
/// message — pushing a context description in front of it would both bury the
/// diagnosis inside an "expected A or B or FATAL:…" list and silently demote
/// the error to a recoverable one, so the enclosing alternation would go on to
/// try other productions and report something unrelated.
pub(super) fn merge_expected_messages(
    context: impl Into<std::borrow::Cow<'static, str>>,
    existing: &[std::borrow::Cow<'static, str>],
) -> Vec<std::borrow::Cow<'static, str>> {
    if existing
        .first()
        .is_some_and(|m| m.starts_with(FATAL_PREFIX))
    {
        return existing.to_vec();
    }
    // A `&'static str` context (which is what all but three call sites pass)
    // keeps borrowing after the prefix strip and the trim, because a subslice
    // of a `'static` str is itself `'static` -- so the merged message copies
    // nothing, exactly as `PError::expected` no longer does. Only a context
    // built with `format!` owns, and only that case can allocate here.
    let key: std::borrow::Cow<'static, str> = match context.into() {
        std::borrow::Cow::Borrowed(s) => {
            std::borrow::Cow::Borrowed(strip_expected_prefix(s).trim())
        }
        std::borrow::Cow::Owned(s) => {
            let stripped = strip_expected_prefix(&s).trim();
            if stripped.len() == s.len() {
                std::borrow::Cow::Owned(s)
            } else {
                std::borrow::Cow::Owned(stripped.to_string())
            }
        }
    };
    let mut result: Vec<std::borrow::Cow<'static, str>> = Vec::with_capacity(1 + existing.len());
    if !key.is_empty() {
        result.push(key);
    }
    for msg in existing {
        if !result.iter().any(|p| p == msg) {
            result.push(msg.clone());
        }
    }
    result
}

pub(super) fn update_best_error(
    best: &mut Option<(usize, PError)>,
    candidate: PError,
    input_len: usize,
) {
    let candidate_score = error_score(&candidate, input_len);
    match best {
        None => *best = Some((candidate_score, candidate)),
        Some((best_score, best_err)) => {
            if candidate_score > *best_score {
                *best = Some((candidate_score, candidate));
            } else if candidate_score == *best_score {
                // Merge message lists directly — no split/join overhead
                for msg in candidate.messages {
                    if !best_err.messages.iter().any(|p| p == &msg) {
                        best_err.messages.push(msg);
                    }
                }
            }
        }
    }
}

impl std::fmt::Display for PError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.messages.is_empty() {
            write!(f, "expected parseable input")
        } else if self.is_fatal() {
            // Fatal errors have pre-formatted messages; strip the FATAL: prefix
            let msg = self.messages[0]
                .strip_prefix(FATAL_PREFIX)
                .unwrap_or(&self.messages[0]);
            write!(f, "{}", msg)
        } else {
            write!(f, "expected {}", self.messages.join(" or "))
        }
    }
}

/// Match a literal string tag at the beginning of input.
pub(super) fn parse_tag<'a>(input: &'a str, tag: &'static str) -> PResult<'a, &'a str> {
    if let Some(rest) = input.strip_prefix(tag) {
        Ok((rest, &input[..tag.len()]))
    } else {
        Err(PError::expected_at(tag, input))
    }
}

/// Match a single character at the beginning of input.
pub(super) fn parse_char(input: &str, c: char) -> PResult<'_, char> {
    if input.starts_with(c) {
        Ok((&input[c.len_utf8()..], c))
    } else {
        Err(PError::expected_at(format!("'{}'", c), input))
    }
}

/// Consume one or more characters matching the predicate.
pub(super) fn take_while1(input: &str, pred: impl Fn(char) -> bool) -> PResult<'_, &str> {
    let end = input.find(|c: char| !pred(c)).unwrap_or(input.len());
    if end == 0 {
        Err(PError::expected_at(
            "at least one matching character",
            input,
        ))
    } else {
        Ok((&input[end..], &input[..end]))
    }
}

/// Consume zero or more characters matching the predicate. Always succeeds.
pub(super) fn take_while_opt(input: &str, pred: impl Fn(char) -> bool) -> (&str, &str) {
    let end = input.find(|c: char| !pred(c)).unwrap_or(input.len());
    (&input[end..], &input[..end])
}

/// Try to match a single character. Returns the remaining input and Some(c) on success.
pub(super) fn opt_char(input: &str, c: char) -> (&str, Option<char>) {
    if input.starts_with(c) {
        (&input[c.len_utf8()..], Some(c))
    } else {
        (input, None)
    }
}
