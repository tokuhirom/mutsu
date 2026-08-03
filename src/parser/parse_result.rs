//! Custom parse result types to replace nom dependency.

pub(super) type PResult<'a, T> = Result<(&'a str, T), PError>;

#[derive(Debug, Clone)]
pub(super) struct PError {
    /// Expected-alternative descriptions (without "expected " prefix).
    /// Display joins them as "expected A or B or C".
    pub messages: Vec<String>,
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

impl PError {
    /// Check if this is a fatal (non-recoverable) parse error.
    pub fn is_fatal(&self) -> bool {
        self.messages
            .first()
            .is_some_and(|m| m.starts_with(FATAL_PREFIX))
    }
}

impl PError {
    pub fn expected(what: &str) -> Self {
        PError {
            messages: vec![what.to_string()],
            remaining_len: None,
            exception: None,
        }
    }

    pub fn expected_at(what: &str, input: &str) -> Self {
        PError {
            messages: vec![what.to_string()],
            remaining_len: Some(input.len()),
            exception: None,
        }
    }

    /// Build a PError from a pre-formatted full message (no "expected " prefix added by Display).
    pub fn raw(message: String, remaining_len: Option<usize>) -> Self {
        PError {
            messages: vec![message],
            remaining_len,
            exception: None,
        }
    }

    /// Build a fatal (non-recoverable) parse error.
    /// Fatal errors are not swallowed by the statement dispatcher.
    pub fn fatal(message: String) -> Self {
        PError {
            messages: vec![format!("{}{}", FATAL_PREFIX, message)],
            remaining_len: None,
            exception: None,
        }
    }

    /// Build a fatal parse error carrying the failure position (`input` is the
    /// unconsumed rest at the error site), so `parse_program` can report the
    /// source line/column like it does for recoverable errors.
    pub fn fatal_at(message: String, input: &str) -> Self {
        PError {
            messages: vec![format!("{}{}", FATAL_PREFIX, message)],
            remaining_len: Some(input.len()),
            exception: None,
        }
    }

    /// Build a fatal parse error with a structured exception.
    pub fn fatal_with_exception(message: String, exception: Box<crate::value::Value>) -> Self {
        PError {
            messages: vec![format!("{}{}", FATAL_PREFIX, message)],
            remaining_len: None,
            exception: Some(exception),
        }
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
        let message = err.message.clone();
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
            .filter(|m| m.as_str() != MISSING_BLOCK)
            .find_map(|m| typed(m))
            .or_else(|| {
                self.messages
                    .first()
                    .filter(|m| m.as_str() == MISSING_BLOCK)
                    .map(|m| m.as_str())
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
pub(super) fn merge_expected_messages(context: &str, existing: &[String]) -> Vec<String> {
    let key = strip_expected_prefix(context).trim();
    let mut result: Vec<String> = Vec::with_capacity(1 + existing.len());
    if !key.is_empty() {
        result.push(key.to_string());
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
pub(super) fn parse_tag<'a>(input: &'a str, tag: &str) -> PResult<'a, &'a str> {
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
        Err(PError::expected_at(&format!("'{}'", c), input))
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
