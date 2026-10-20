/// Return a full statement parser's consumed terminator to an enclosing
/// statement-prefix expression. Some statement forms consume a semicolon or
/// the newline after a block; the outer expression must still stop there.
// Cost: O(h + w), h = heredoc buffers, w = trailing whitespace bytes.
pub(super) fn restore_statement_terminator<'a>(orig: &'a str, after: &'a str) -> &'a str {
    // A heredoc can return a rewritten buffer. If its terminator is already
    // present, keep that remainder instead of inferring a span from its length
    // (which could point into a UTF-8 end marker in the original buffer).
    if after.starts_with(';') {
        return after;
    }
    let Some(tail) = crate::parser::primary::source_tail(orig, after) else {
        return after;
    };
    let consumed_source = tail.before();
    let consumed = consumed_source.len();
    if consumed_source.ends_with(';') {
        tail.restore_from(consumed - 1).unwrap_or(after)
    } else if consumed > 0 {
        let trailing_ws_start = consumed_source
            .char_indices()
            .rev()
            .find(|(_, ch)| !ch.is_whitespace())
            .map_or(0, |(idx, ch)| idx + ch.len_utf8());
        if consumed_source[trailing_ws_start..].contains(['\n', '\r']) {
            return tail.restore_from(trailing_ws_start).unwrap_or(after);
        }
        after
    } else {
        after
    }
}
