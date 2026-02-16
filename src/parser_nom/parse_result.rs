//! Custom parse result types to replace nom dependency.

pub(super) type PResult<'a, T> = Result<(&'a str, T), PError>;

#[derive(Debug, Clone)]
pub(super) struct PError {
    pub message: String,
    pub remaining_len: Option<usize>,
}

impl PError {
    pub fn expected(what: &str) -> Self {
        PError {
            message: format!("expected {}", what),
            remaining_len: None,
        }
    }

    pub fn expected_at(what: &str, input: &str) -> Self {
        PError {
            message: format!("expected {}", what),
            remaining_len: Some(input.len()),
        }
    }

    pub fn consumed_from(&self, total_len: usize) -> Option<usize> {
        self.remaining_len
            .map(|remaining| total_len.saturating_sub(remaining.min(total_len)))
    }
}

pub(super) fn error_score(err: &PError, input_len: usize) -> usize {
    err.consumed_from(input_len).unwrap_or(0)
}

fn expected_message_key(message: &str) -> &str {
    message.strip_prefix("expected ").unwrap_or(message)
}

pub(super) fn merge_expected_messages(existing: &str, incoming: &str) -> String {
    let mut parts: Vec<String> = Vec::new();
    let mut push_unique = |text: &str| {
        let normalized = expected_message_key(text).trim();
        if normalized.is_empty() {
            return;
        }
        if !parts.iter().any(|p| p == normalized) {
            parts.push(normalized.to_string());
        }
    };

    for item in existing.split(" or ") {
        push_unique(item);
    }
    for item in incoming.split(" or ") {
        push_unique(item);
    }

    if parts.is_empty() {
        "expected parseable input".to_string()
    } else {
        format!("expected {}", parts.join(" or "))
    }
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
                best_err.message = merge_expected_messages(&best_err.message, &candidate.message);
            }
        }
    }
}

impl std::fmt::Display for PError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
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
