//! Custom parse result types to replace nom dependency.

pub(super) type PResult<'a, T> = Result<(&'a str, T), PError>;

#[derive(Debug, Clone)]
pub(super) struct PError {
    pub message: String,
}

impl PError {
    pub fn expected(what: &str) -> Self {
        PError {
            message: format!("expected {}", what),
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
        Err(PError::expected(tag))
    }
}

/// Match a single character at the beginning of input.
pub(super) fn parse_char(input: &str, c: char) -> PResult<'_, char> {
    if input.starts_with(c) {
        Ok((&input[c.len_utf8()..], c))
    } else {
        Err(PError::expected(&format!("'{}'", c)))
    }
}

/// Consume one or more characters matching the predicate.
pub(super) fn take_while1(input: &str, pred: impl Fn(char) -> bool) -> PResult<'_, &str> {
    let end = input.find(|c: char| !pred(c)).unwrap_or(input.len());
    if end == 0 {
        Err(PError::expected("at least one matching character"))
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
