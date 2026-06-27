fn find_matching_bracket(input: &str) -> Option<usize> {
    if !input.starts_with('[') {
        return None;
    }
    let mut depth = 0usize;
    for (i, ch) in input.char_indices() {
        if ch == '[' {
            depth += 1;
        } else if ch == ']' {
            depth = depth.saturating_sub(1);
            if depth == 0 {
                return Some(i);
            }
        }
    }
    None
}

fn flatten_bracket_op(s: &str) -> String {
    if s.is_empty() {
        return String::new();
    }
    let first = s.as_bytes()[0];
    if first == b'['
        && let Some(end) = find_matching_bracket(s)
    {
        let inner = &s[1..end];
        return flatten_bracket_op(inner);
    }
    if (first == b'R' || first == b'X' || first == b'Z')
        && s.len() > 1
        && s.as_bytes()[1] == b'['
        && let Some(end) = find_matching_bracket(&s[1..])
    {
        let inner = &s[2..1 + end];
        let flattened_inner = flatten_bracket_op(inner);
        let rest = &s[1 + end + 1..];
        return format!("{}{}{}", first as char, flattened_inner, rest);
    }
    s.to_string()
}

pub(crate) fn parse_bracket_meta_assign_op(input: &str) -> Option<(&str, String, String)> {
    if !input.starts_with('[') {
        return None;
    }
    let end = find_matching_bracket(input)?;
    let inner = &input[1..end];
    let after_bracket = &input[end + 1..];
    if !after_bracket.starts_with('=') || after_bracket.starts_with("==") {
        return None;
    }
    // Distinguish a reduction compound assignment (`[+]=`, `[max]=`, `[,]=`,
    // `[R,]=`) from a subscript-then-assign (`@a[0]=1`, `@a[$i]=…`, `@a[-1]=…`,
    // `@a[1,2]=…`, `@a[*-1]=…`). A reduction operator begins with an operator
    // symbol/word and never with a value: a leading digit, sigil, quote,
    // negative-index `-N`, or `*`-Whatever end-index marks a subscript/slice, so
    // bail and let the caller parse the `[...]` as a postfix subscript on the
    // lvalue. (A value-slice like `@a[1,2]` already trips the leading-digit/sigil
    // test, so the comma-reductions `[,]`/`[R,]` are left intact.)
    {
        let t = inner.trim_start();
        let subscript_like = match t.bytes().next() {
            None => false, // empty `[]` keeps the prior (degenerate-reduce) path
            Some(c) if c.is_ascii_digit() => true,
            Some(b'$' | b'@' | b'%' | b'&' | b'"' | b'\'') => true,
            Some(b'-') => t[1..]
                .trim_start()
                .bytes()
                .next()
                .is_some_and(|c| c.is_ascii_digit()),
            // A `*`-Whatever end-index (`@a[*-1]`, `@a[* - 1]`, `@a[*/2]`) — but
            // not the bare reduction ops `[*]` (multiply) / `[**]` (power).
            Some(b'*') => matches!(
                t[1..].trim_start().bytes().next(),
                Some(b'-' | b'+' | b'/' | b'%')
            ),
            _ => false,
        };
        if subscript_like {
            return None;
        }
    }
    let flattened = flatten_bracket_op(inner);
    let (meta, op) = if let Some(op) = flattened.strip_prefix('R') {
        ("R", op)
    } else if let Some(op) = flattened.strip_prefix('X') {
        ("X", op)
    } else if let Some(op) = flattened.strip_prefix('Z') {
        ("Z", op)
    } else {
        // Plain reduction compound assignment: [+]= is like +=
        // The reduction of a binary op on two values is just the op itself.
        let rest = &after_bracket[1..];
        return Some((rest, "reduce".to_string(), flattened));
    };
    let rest = &after_bracket[1..];
    Some((rest, meta.to_string(), op.to_string()))
}
