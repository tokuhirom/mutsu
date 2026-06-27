use super::*;

pub(crate) fn is_user_declared_prefix_sub(symbol: &str) -> bool {
    let op_name = format!("prefix:<{}>", symbol);
    is_user_declared_sub(&op_name)
}

/// Match a user-declared prefix operator against the current input.
/// Returns `(full_name, consumed_len)` when input begins with an in-scope
/// `prefix:<...>` operator symbol.
pub(crate) fn match_user_declared_prefix_op(input: &str) -> Option<(String, usize)> {
    SCOPES.with(|s| {
        let scopes = s.borrow();
        let mut best: Option<(String, usize)> = None;

        for scope in scopes.iter().rev() {
            for name in &scope.user_subs {
                let Some(op) = name
                    .strip_prefix("prefix:<")
                    .and_then(|s| s.strip_suffix('>'))
                else {
                    continue;
                };
                if !input.starts_with(op) {
                    continue;
                }
                if scope
                    .infix_assoc
                    .get(name)
                    .is_some_and(|assoc| assoc == "looser")
                {
                    continue;
                }
                let consumed = op.len();
                // For word-like operators, require identifier boundary.
                if op
                    .as_bytes()
                    .last()
                    .copied()
                    .is_some_and(|b| crate::parser::helpers::is_ident_char(Some(b)))
                    && input
                        .as_bytes()
                        .get(consumed)
                        .copied()
                        .is_some_and(|b| crate::parser::helpers::is_ident_char(Some(b)))
                {
                    continue;
                }
                if best
                    .as_ref()
                    .is_none_or(|(_, best_len)| consumed > *best_len)
                {
                    best = Some((name.clone(), consumed));
                }
            }
        }
        best
    })
}

/// Match a user-declared postfix operator against the current input.
/// Returns `(full_name, consumed_len)` when input begins with an in-scope
/// `postfix:<...>` operator symbol.
pub(crate) fn match_user_declared_postfix_op(input: &str) -> Option<(String, usize)> {
    SCOPES.with(|s| {
        let scopes = s.borrow();
        let mut best: Option<(String, usize)> = None;

        for scope in scopes.iter().rev() {
            for name in &scope.user_subs {
                let Some(op) = name
                    .strip_prefix("postfix:<")
                    .and_then(|s| s.strip_suffix('>'))
                else {
                    continue;
                };
                if !input.starts_with(op) {
                    continue;
                }
                let consumed = op.len();
                // For word-like operators, require identifier boundary.
                if op
                    .as_bytes()
                    .last()
                    .copied()
                    .is_some_and(|b| crate::parser::helpers::is_ident_char(Some(b)))
                    && input
                        .as_bytes()
                        .get(consumed)
                        .copied()
                        .is_some_and(|b| crate::parser::helpers::is_ident_char(Some(b)))
                {
                    continue;
                }
                if best
                    .as_ref()
                    .is_none_or(|(_, best_len)| consumed > *best_len)
                {
                    best = Some((name.clone(), consumed));
                }
            }
        }
        best
    })
}

/// Check if an operator symbol would conflict with core language syntax.
/// These operators are registered as subs but should not be matched as infix
/// operators during expression parsing because they have fixed syntactic meaning.
fn is_syntax_conflicting_infix(op: &str) -> bool {
    matches!(
        op,
        ";" | ")" | "]" | "}" | "," | ":" | "=" | "=>" | "{" | "(" | "["
    )
}

/// Check whether a given operator symbol is registered as a user-defined
/// `infix:<...>` in the current lexical scope.
pub(crate) fn is_user_defined_infix(symbol: &str) -> bool {
    SCOPES.with(|s| {
        let scopes = s.borrow();
        let canonical = format!("infix:<{symbol}>");
        scopes
            .iter()
            .rev()
            .any(|scope| scope.user_subs.contains(&canonical))
    })
}

/// Match a user-declared infix operator (symbol form) against the current input.
/// Returns `(symbol, consumed_len)` when input begins with an in-scope
/// `infix:<...>` operator symbol. This handles both non-alphabetic symbols
/// (e.g. `©`, `×`) and mixed symbols containing non-word characters (e.g. `_<_`).
pub(crate) fn match_user_declared_infix_symbol_op(input: &str) -> Option<(String, usize)> {
    SCOPES.with(|s| {
        let scopes = s.borrow();
        let mut best: Option<(String, usize)> = None;

        for scope in scopes.iter().rev() {
            for name in &scope.user_subs {
                let Some(op) = name
                    .strip_prefix("infix:<")
                    .and_then(|s| s.strip_suffix('>'))
                else {
                    continue;
                };
                if op.is_empty() {
                    continue;
                }
                // Skip operators whose symbols conflict with language syntax
                if is_syntax_conflicting_infix(op) {
                    continue;
                }
                if !input.starts_with(op) {
                    continue;
                }
                let consumed = op.len();
                // For word-like operators, require identifier boundary.
                if op
                    .as_bytes()
                    .last()
                    .copied()
                    .is_some_and(|b| crate::parser::helpers::is_ident_char(Some(b)))
                    && input
                        .as_bytes()
                        .get(consumed)
                        .copied()
                        .is_some_and(|b| crate::parser::helpers::is_ident_char(Some(b)))
                {
                    continue;
                }
                if best
                    .as_ref()
                    .is_none_or(|(_, best_len)| consumed > *best_len)
                {
                    best = Some((op.to_string(), consumed));
                }
            }
        }
        best
    })
}

/// Register a user-declared term symbol.
/// The canonical name can be in `term:<...>` form or a plain identifier
/// (for sigilless variables declared with `my \name`).
pub(crate) fn register_user_term_symbol(name: &str) {
    let symbol = if let Some(s) = name
        .strip_prefix("term:<")
        .and_then(|s| s.strip_suffix('>'))
    {
        s.to_string()
    } else if !name.starts_with('$')
        && !name.starts_with('@')
        && !name.starts_with('%')
        && !name.starts_with('&')
    {
        // Plain identifier (sigilless variable): use name as both symbol and canonical name
        name.to_string()
    } else {
        // Sigiled variables ($x, @x, %x) are not term symbols
        return;
    };
    SCOPES.with(|s| {
        let mut scopes = s.borrow_mut();
        let current = scopes
            .last_mut()
            .expect("scope stack should never be empty");
        current
            .term_symbols
            .insert(symbol, TermBinding::Value(name.to_string()));
    });
}

pub(crate) fn register_user_callable_term_symbol(name: &str) {
    let Some(symbol) = name
        .strip_prefix("term:<")
        .and_then(|s| s.strip_suffix('>'))
    else {
        return;
    };
    SCOPES.with(|s| {
        let mut scopes = s.borrow_mut();
        let current = scopes
            .last_mut()
            .expect("scope stack should never be empty");
        current
            .term_symbols
            .insert(symbol.to_string(), TermBinding::Callable(name.to_string()));
    });
}

/// Resolve an in-scope term symbol from the current input.
/// Returns `(canonical_name, consumed_len)` when the input begins with a declared symbol.
pub(crate) fn match_user_declared_term_symbol(input: &str) -> Option<(String, usize, bool)> {
    SCOPES.with(|s| {
        let scopes = s.borrow();
        let mut best: Option<(String, usize, bool)> = None;

        for scope in scopes.iter().rev() {
            for (symbol, binding) in &scope.term_symbols {
                if !input.starts_with(symbol) {
                    continue;
                }
                let consumed = symbol.len();
                // For word-like symbols, require identifier boundary.
                if symbol
                    .as_bytes()
                    .last()
                    .copied()
                    .is_some_and(|b| crate::parser::helpers::is_ident_char(Some(b)))
                    && input
                        .as_bytes()
                        .get(consumed)
                        .copied()
                        .is_some_and(|b| crate::parser::helpers::is_ident_char(Some(b)))
                {
                    continue;
                }
                let candidate = match binding {
                    TermBinding::Value(canonical) => (canonical.clone(), consumed, false),
                    TermBinding::Callable(canonical) => (canonical.clone(), consumed, true),
                };
                let replace = match &best {
                    None => true,
                    Some((_, best_len, best_callable)) => {
                        consumed > *best_len
                            || (consumed == *best_len && candidate.2 && !*best_callable)
                    }
                };
                if replace {
                    best = Some(candidate);
                }
            }
        }
        best
    })
}

/// Match a user-declared circumfix operator against the current input.
/// Returns `(full_name, open_len, close_delim)` when input begins with an in-scope
/// `circumfix:<open close>` operator's opening delimiter.
pub(crate) fn match_user_declared_circumfix_op(input: &str) -> Option<(String, usize, String)> {
    SCOPES.with(|s| {
        let scopes = s.borrow();
        let mut best: Option<(String, usize, String)> = None;

        for scope in scopes.iter().rev() {
            for name in &scope.user_subs {
                let Some(delims) = name
                    .strip_prefix("circumfix:<")
                    .and_then(|s| s.strip_suffix('>'))
                else {
                    continue;
                };
                // Split into open and close delimiters (space-separated)
                let parts: Vec<&str> = delims.split_whitespace().collect();
                if parts.len() != 2 {
                    continue;
                }
                let open = parts[0];
                let close = parts[1];
                if !input.starts_with(open) {
                    continue;
                }
                let consumed = open.len();
                if best
                    .as_ref()
                    .is_none_or(|(_, best_len, _)| consumed > *best_len)
                {
                    best = Some((name.clone(), consumed, close.to_string()));
                }
            }
        }
        best
    })
}

/// Match a user-declared postcircumfix operator against the current input.
/// Returns `(full_name, open_len, close_delim)` when input begins with an in-scope
/// `postcircumfix:<open close>` operator's opening delimiter.
pub(crate) fn match_user_declared_postcircumfix_op(input: &str) -> Option<(String, usize, String)> {
    SCOPES.with(|s| {
        let scopes = s.borrow();
        let mut best: Option<(String, usize, String)> = None;

        for scope in scopes.iter().rev() {
            for name in &scope.user_subs {
                let Some(delims) = name
                    .strip_prefix("postcircumfix:<")
                    .and_then(|s| s.strip_suffix('>'))
                else {
                    continue;
                };
                // Split into open and close delimiters (space-separated)
                let parts: Vec<&str> = delims.split_whitespace().collect();
                if parts.len() != 2 {
                    continue;
                }
                let open = parts[0];
                let close = parts[1];
                if !input.starts_with(open) {
                    continue;
                }
                let consumed = open.len();
                if best
                    .as_ref()
                    .is_none_or(|(_, best_len, _)| consumed > *best_len)
                {
                    best = Some((name.clone(), consumed, close.to_string()));
                }
            }
        }
        best
    })
}

/// Check if the input starts with a registered circumfix or postcircumfix closing delimiter.
/// Used to prevent the postfix operator parser from consuming closing delimiters.
pub(crate) fn is_circumfix_close_delimiter(input: &str) -> bool {
    SCOPES.with(|s| {
        let scopes = s.borrow();
        for scope in scopes.iter().rev() {
            for name in &scope.user_subs {
                let delims = if let Some(d) = name
                    .strip_prefix("circumfix:<")
                    .and_then(|s| s.strip_suffix('>'))
                {
                    d
                } else if let Some(d) = name
                    .strip_prefix("postcircumfix:<")
                    .and_then(|s| s.strip_suffix('>'))
                {
                    d
                } else {
                    continue;
                };
                let parts: Vec<&str> = delims.split_whitespace().collect();
                if parts.len() == 2 && input.starts_with(parts[1]) {
                    return true;
                }
            }
        }
        false
    })
}
