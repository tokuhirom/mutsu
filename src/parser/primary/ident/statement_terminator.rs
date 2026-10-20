/// Return a full statement parser's consumed terminator to an enclosing
/// statement-prefix expression. Some statement forms consume a semicolon or
/// the newline after a block; the outer expression must still stop there.
// Cost: O(w), w = trailing whitespace bytes in the consumed source.
pub(super) fn restore_statement_terminator<'a>(orig: &'a str, after: &'a str) -> &'a str {
    // A heredoc can return a rewritten buffer. If its terminator is already
    // present, keep that remainder instead of inferring a span from its length
    // (which could point into a UTF-8 end marker in the original buffer).
    if after.starts_with(';') {
        return after;
    }
    let consumed = orig.len().saturating_sub(after.len());
    let Some(consumed_source) = orig.get(..consumed) else {
        return after;
    };
    if consumed_source.ends_with(';') {
        &orig[consumed - 1..]
    } else if consumed > 0 {
        let trailing_ws_start = consumed_source
            .char_indices()
            .rev()
            .find(|(_, ch)| !ch.is_whitespace())
            .map_or(0, |(idx, ch)| idx + ch.len_utf8());
        if consumed_source[trailing_ws_start..].contains(['\n', '\r']) {
            return &orig[trailing_ws_start..];
        }
        after
    } else {
        after
    }
}
