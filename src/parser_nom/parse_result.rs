//! Custom parse result types to replace nom dependency.

pub(super) type PResult<'a, T> = Result<(&'a str, T), PError>;

#[derive(Debug, Clone)]
pub(super) struct PError {
    /// Expected-alternative descriptions (without "expected " prefix).
    /// Display joins them as "expected A or B or C".
    pub messages: Vec<String>,
    pub remaining_len: Option<usize>,
}

impl PError {
    pub fn expected(what: &str) -> Self {
        PError {
            messages: vec![what.to_string()],
            remaining_len: None,
        }
    }

    pub fn expected_at(what: &str, input: &str) -> Self {
        PError {
            messages: vec![what.to_string()],
            remaining_len: Some(input.len()),
        }
    }

    /// Build a PError from a pre-formatted full message (no "expected " prefix added by Display).
    pub fn raw(message: String, remaining_len: Option<usize>) -> Self {
        PError {
            messages: vec![message],
            remaining_len,
        }
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
