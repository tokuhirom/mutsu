//! The spelling of a hyper operator (`>>op<<`, `»op«`, `>>[&f]<<`, ...):
//! recognising its markers and extracting the base operator, split out of
//! `hyper_concat` so that file stays a parser of hyper *expressions*.

/// The right-delimiter markers a hyper operator may close with, paired with the
/// `dwim_right` they imply and their byte length. An "outward pointing" closer
/// (`>>` / `\u{00BB}`) is the DWIM one; an inward `<<` / `\u{00AB}` is strict.
const HYPER_RIGHT_MARKERS: [(&str, bool, usize); 4] = [
    (">>", true, 2),
    ("<<", false, 2),
    ("\u{00BB}", true, 2),
    ("\u{00AB}", false, 2),
];

/// Consume a hyper *left* delimiter, returning `(dwim_left, len, remainder)`.
fn strip_hyper_left(input: &str) -> Option<(bool, usize, &str)> {
    if let Some(r) = input.strip_prefix('\u{00BB}') {
        // \u{00BB} = >> (non-DWIM left)
        Some((false, '\u{00BB}'.len_utf8(), r))
    } else if let Some(r) = input.strip_prefix('\u{00AB}') {
        // \u{00AB} = << (DWIM left)
        Some((true, '\u{00AB}'.len_utf8(), r))
    } else if let Some(r) = input.strip_prefix(">>") {
        Some((false, 2, r))
    } else if let Some(r) = input.strip_prefix("<<") {
        Some((true, 2, r))
    } else {
        None
    }
}

/// Parse hyper operator: `>>op<<`, `>>op>>`, `<<op<<`, `<<op>>`
/// Also supports Unicode variants: \u{00BB}op\u{00AB}, \u{00BB}op\u{00BB}, \u{00AB}op\u{00AB}, \u{00AB}op\u{00BB}
/// and mixed forms like >>op\u{00AB}, \u{00BB}op<<, etc.
pub(super) fn parse_hyper_op(input: &str) -> Option<(String, bool, bool, usize)> {
    // A user-declared infix spelled with hyper markers (OneSeq's
    // `infix:«>>>»`) wins over a hyper reading: that reading is either
    // shorter, or spans whitespace (`>>` + `> @b ` + `>>`), which no hyper
    // operator can. Without this `@a >>> @b >>> @c` failed to parse.
    let hyper = parse_hyper_op_spelling(input)?;
    if crate::parser::stmt::simple::match_user_declared_infix_symbol_op(input)
        .is_some_and(|(_, len)| len >= hyper.3 || hyper.0.contains(char::is_whitespace))
    {
        return None;
    }
    Some(hyper)
}

fn parse_hyper_op_spelling(input: &str) -> Option<(String, bool, bool, usize)> {
    // Determine left delimiter and dwim_left
    let (dwim_left, left_len, after_left) = strip_hyper_left(input)?;

    // A hyper operator's *base* operator may itself be spelled as a hyper
    // operator: `@a \u{00BB}>>+<<\u{00BB} @b` wraps `>>+<<` in an outer
    // `\u{00BB}...\u{00BB}` pair (Language/operators.rakudoc). rakudo builds a
    // `MetaInfix::Hyper` whose `infix` is another `MetaInfix::Hyper`, but the
    // nesting is semantically inert — hyper already descends into nested
    // structures, and only the OUTERMOST pair's dwim flags govern the
    // dimension-mismatch rules (verified against raku: `((1,2),(3,4))
    // \u{00BB}>>+<<\u{00BB} ((10,20),(30,40,50))` behaves exactly like
    // `\u{00BB}+\u{00BB}`, and `\u{00BB}<<+>>\u{00AB}` throws
    // X::HyperOp::NonDWIM exactly like `\u{00BB}+\u{00AB}`). So unwrap the
    // inner spelling recursively and keep the outer dwim flags. Without this the
    // candidate scan below took the inner `<<` as the closing marker and produced
    // the bogus operator `>>+`.
    if strip_hyper_left(after_left).is_some()
        && let Some((inner_op, _, _, inner_len)) = parse_hyper_op_spelling(after_left)
        && !inner_op.is_empty()
        && let Some((_, dwim_right, right_len)) = HYPER_RIGHT_MARKERS
            .iter()
            .find(|(marker, ..)| after_left[inner_len..].starts_with(*marker))
    {
        return Some((
            inner_op,
            dwim_left,
            *dwim_right,
            left_len + inner_len + right_len,
        ));
    }

    // Search for right delimiter within the operator string
    let mut search_limit = after_left.len().min(10);
    // Ensure we don't slice in the middle of a multi-byte UTF-8 character
    while search_limit > 0 && !after_left.is_char_boundary(search_limit) {
        search_limit -= 1;
    }
    let search = &after_left[..search_limit];

    // Collect every candidate right-delimiter occurrence (any of >>, <<,
    // \u{00BB}, \u{00AB}) in ascending byte-offset order. An operator whose
    // last character is itself `>` (e.g. `=>`) can make the closing `>>`
    // marker overlap with the operator text — e.g. `<<=>>>` (`<<` + `=>` +
    // `>>`) has a spurious `>>` match one byte earlier (`=` + the first two
    // `>`s), which would wrongly yield the reserved bare `=` operator. Try
    // candidates in order and skip any that resolve to an invalid op instead
    // of bailing out on the first (possibly overlapping) match.
    let mut candidates: Vec<(usize, bool, usize)> = Vec::new(); // (byte_offset, dwim_right, marker_len)
    for &(marker, dwim_right, marker_len) in HYPER_RIGHT_MARKERS.iter() {
        let mut from = 0;
        while let Some(rel_pos) = search[from..].find(marker) {
            let pos = from + rel_pos;
            if pos > 0 {
                candidates.push((pos, dwim_right, marker_len));
            }
            // Step past only the first char of this match (not the whole
            // marker) so overlapping matches — e.g. the two `>>` markers
            // that share a byte in `=>>>` — are still found on the next
            // iteration. Stepping by a full char keeps `from` on a UTF-8
            // boundary for multi-byte markers like `\u{00BB}`.
            from = pos + search[pos..].chars().next().map_or(1, |c| c.len_utf8());
        }
    }
    candidates.sort_by_key(|&(pos, ..)| pos);

    let candidate = candidates
        .iter()
        .copied()
        .find(|(end, ..)| &after_left[..*end] != "=")
        .or_else(|| candidates.first().copied());
    if let Some((end, dwim_right, right_marker_len)) = candidate {
        let op = &after_left[..end];
        return Some((
            op.to_string(),
            dwim_left,
            dwim_right,
            left_len + end + right_marker_len,
        ));
    }
    None
}

/// Parse hyper operator with function reference: `>>[&func]<<`, `<<[&func]>>`, etc.
/// Returns (func_name, dwim_left, dwim_right, total_consumed_length)
pub(super) fn parse_hyper_func_op(input: &str) -> Option<(String, bool, bool, usize)> {
    // Determine left delimiter and dwim_left
    let (dwim_left, left_len, after_left) = if let Some(r) = input.strip_prefix('\u{00BB}') {
        (false, '\u{00BB}'.len_utf8(), r)
    } else if let Some(r) = input.strip_prefix('\u{00AB}') {
        (true, '\u{00AB}'.len_utf8(), r)
    } else if let Some(r) = input.strip_prefix(">>") {
        (false, 2, r)
    } else if let Some(r) = input.strip_prefix("<<") {
        (true, 2, r)
    } else {
        return None;
    };

    // Check for [&func] pattern (e.g. [&infix:<+>])
    if !after_left.starts_with("[&") {
        return None;
    }
    let bracket_content = &after_left[2..]; // skip "[&"
    let end = bracket_content.find(']')?;
    let name = &bracket_content[..end];
    // Allow alphanumeric, hyphens, underscores, and operator-style names like infix:<+>
    if name.is_empty()
        || !name.chars().all(|c| {
            c.is_alphanumeric()
                || matches!(
                    c,
                    '-' | '_'
                        | ':'
                        | '<'
                        | '>'
                        | '+'
                        | '*'
                        | '/'
                        | '~'
                        | '%'
                        | '!'
                        | '?'
                        | '|'
                        | '^'
                        | '='
                        | '.'
                        | '&'
                        | ','
                        | '\\'
                )
        })
    {
        return None;
    }
    let after_bracket = &after_left[2 + end + 1..]; // skip "[&name]"
    let bracket_len = 2 + end + 1;

    // Determine right delimiter and dwim_right
    let (dwim_right, right_len) = if let Some(_r) = after_bracket.strip_prefix('\u{00BB}') {
        (true, '\u{00BB}'.len_utf8())
    } else if let Some(_r) = after_bracket.strip_prefix('\u{00AB}') {
        (false, '\u{00AB}'.len_utf8())
    } else if after_bracket.starts_with(">>") {
        (true, 2)
    } else if after_bracket.starts_with("<<") {
        (false, 2)
    } else {
        return None;
    };

    let total_len = left_len + bracket_len + right_len;
    Some((name.to_string(), dwim_left, dwim_right, total_len))
}
