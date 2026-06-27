use crate::ast::Expr;
use crate::symbol::Symbol;
use crate::value::Value;

/// Try to parse a postcircumfix call on an interpolated variable: "$var()" or "$var(args)".
/// In Raku, `"$callable(args)"` invokes the callable during interpolation. Only triggers
/// when `(` immediately follows the variable (or its index/postcircumfix). The argument
/// text must not contain a nested string delimiter, since the surrounding string scanner
/// does not balance quotes inside interpolation.
pub(crate) fn try_parse_interp_call(input: &str, target: Expr) -> (Expr, &str) {
    if !input.starts_with('(') {
        return (target, input);
    }
    // Find the matching close paren, respecting nesting.
    let mut depth = 0usize;
    let mut paren_end = None;
    for (idx, ch) in input.char_indices() {
        if ch == '(' {
            depth += 1;
        } else if ch == ')' {
            depth -= 1;
            if depth == 0 {
                paren_end = Some(idx);
                break;
            }
        }
    }
    let Some(pe) = paren_end else {
        return (target, input);
    };
    let args_str = &input[1..pe];
    let after = &input[pe + 1..];
    let args = if args_str.trim().is_empty() {
        vec![]
    } else {
        let mut args = vec![];
        for arg in args_str.split(',') {
            let arg = arg.trim();
            if let Ok((_, e)) = crate::parser::expr::expression(arg) {
                args.push(e);
            }
        }
        args
    };
    (
        Expr::CallOn {
            target: Box::new(target),
            args,
        },
        after,
    )
}

/// Try to parse a method call chain on an interpolated variable: "$var.method()" or "$var.method".
/// Only recognizes simple identifier method names followed by `()` (no args).
/// Try to parse a method call chain on an interpolated variable: "$var.method()" or "$var.method".
/// Supports chained methods: "$var.flip.chars()" — intermediate methods need no parens,
/// but the chain is only interpolated if the LAST method has parens.
/// Also supports methods with arguments: "$var.substr(0,1)".
/// Also supports indirect (quoted) method names: "$var.'method'()" or "$var."method"()".
pub(crate) fn try_parse_interp_method_call(input: &str, target: Expr) -> (Expr, &str) {
    let mut expr = target;
    let mut rest = input;
    // Collect chain of .method parts; only commit if chain ends with parens
    // Each entry is (method_name, is_quoted, modifier) where modifier is
    // the meta-method prefix character (^, ?, !) if present.
    let mut chain: Vec<(String, bool, Option<char>)> = Vec::new();
    let mut chain_rest = rest;
    while let Some(after_dot) = chain_rest.strip_prefix('.') {
        if after_dot.is_empty() {
            break;
        }
        // Check for meta-method prefix: .^name() .?method() .!method()
        let (method_prefix, after_prefix) = if let Some(stripped) = after_dot.strip_prefix('^') {
            (Some('^'), stripped)
        } else if let Some(stripped) = after_dot.strip_prefix('?') {
            (Some('?'), stripped)
        } else if let Some(stripped) = after_dot.strip_prefix('!') {
            (Some('!'), stripped)
        } else {
            (None, after_dot)
        };
        let after_dot = after_prefix;
        if after_dot.is_empty() {
            break;
        }
        // Check for quoted method name: .'method'() or ."method"()
        let (method_name, after_name, is_quoted) =
            if let Some(after_q) = after_dot.strip_prefix('\'') {
                if let Some(end) = after_q.find('\'') {
                    let name = &after_q[..end];
                    // Reject quoted method names containing whitespace
                    if name.contains(char::is_whitespace) {
                        break;
                    }
                    let rest = &after_q[end + 1..];
                    (name, rest, true)
                } else {
                    break;
                }
            } else if let Some(after_q) = after_dot.strip_prefix('\u{201C}') {
                // Unicode left double quote
                if let Some(end) = after_q.find('\u{201D}') {
                    let name = &after_q[..end];
                    let rest = &after_q[end + '\u{201D}'.len_utf8()..];
                    (name, rest, true)
                } else {
                    break;
                }
            } else if let Some(after_q) = after_dot.strip_prefix('"') {
                if let Some(end) = after_q.find('"') {
                    let name = &after_q[..end];
                    // Reject quoted method names containing whitespace
                    if name.contains(char::is_whitespace) {
                        break;
                    }
                    let rest = &after_q[end + 1..];
                    (name, rest, true)
                } else {
                    break;
                }
            } else {
                let first = after_dot.as_bytes()[0];
                if !(first.is_ascii_alphabetic() || first == b'_') {
                    break;
                }
                let end = after_dot
                    .find(|c: char| !c.is_alphanumeric() && c != '_' && c != '-')
                    .unwrap_or(after_dot.len());
                (&after_dot[..end], &after_dot[end..], false)
            };
        chain.push((method_name.to_string(), is_quoted, method_prefix));
        // Check if followed by (...) — parse args
        if after_name.starts_with('(') {
            // Find matching closing paren
            let mut depth = 0usize;
            let mut paren_end = None;
            for (idx, ch) in after_name.char_indices() {
                if ch == '(' {
                    depth += 1;
                } else if ch == ')' {
                    depth -= 1;
                    if depth == 0 {
                        paren_end = Some(idx);
                        break;
                    }
                }
            }
            if let Some(pe) = paren_end {
                let args_str = &after_name[1..pe];
                let after_parens = &after_name[pe + 1..];
                // Build the full chain
                for (i, (name, quoted, modifier)) in chain.iter().enumerate() {
                    let args = if i == chain.len() - 1 {
                        // Last method gets the args
                        if args_str.trim().is_empty() {
                            vec![]
                        } else {
                            let mut args = vec![];
                            for arg in args_str.split(',') {
                                let arg = arg.trim();
                                if let Ok((_, e)) = crate::parser::expr::expression(arg) {
                                    args.push(e);
                                }
                            }
                            args
                        }
                    } else {
                        vec![]
                    };
                    expr = Expr::MethodCall {
                        target: Box::new(expr),
                        name: Symbol::intern(name),
                        args,
                        modifier: *modifier,
                        quoted: *quoted,
                    };
                }
                rest = after_parens;
                chain.clear();
                // Continue looking for more chained method calls after parens
                // e.g., "$var.absolute().raku()" should parse both methods
                chain_rest = after_parens;
                continue;
            }
            break;
        }
        chain_rest = after_name;
    }
    (expr, rest)
}

/// Parse shell-word content from `<<...>>` or `«...»` subscripts.
/// Handles variable interpolation ($var) and bare words.
pub(crate) fn parse_shell_words_index(content: &str) -> Expr {
    let trimmed = content.trim();
    // Check for variable interpolation ($var)
    if let Some(var_name) = trimmed.strip_prefix('$')
        && !var_name.is_empty()
        && var_name
            .chars()
            .all(|c| c.is_alphanumeric() || c == '_' || c == '-')
    {
        return Expr::Var(var_name.to_string());
    }
    // Otherwise treat as literal string key
    Expr::Literal(Value::str(trimmed.to_string()))
}

/// Handle double-sigil interpolation patterns like `@$name[]`, `%$name{}`, `$$name[]`, etc.
/// The outer sigil determines the context and expected postcircumfix, the inner sigil+name
/// identifies the variable. Returns `Some(remaining_input)` on success.
pub(crate) fn try_double_sigil_interp<'a, F>(
    rest: &'a str,
    parts: &mut Vec<Expr>,
    current: &mut String,
    parse_postcircumfix_index: F,
) -> Option<&'a str>
where
    F: Fn(&'a str, Expr) -> (Expr, &'a str),
{
    let bytes = rest.as_bytes();
    if bytes.len() < 3 {
        return None;
    }
    let outer_sigil = bytes[0] as char;
    let inner_sigil = bytes[1] as char;
    if !matches!(outer_sigil, '$' | '@' | '%' | '&') {
        return None;
    }
    if !matches!(inner_sigil, '$' | '@' | '%' | '&') {
        return None;
    }
    // The char after the inner sigil must start a variable name
    let after_inner = &rest[2..];
    let first_name_char = after_inner.as_bytes().first().copied().unwrap_or(0) as char;
    if !first_name_char.is_alphabetic() && first_name_char != '_' {
        return None;
    }
    // Parse the variable name
    let end = after_inner
        .find(|c: char| !c.is_alphanumeric() && c != '_' && c != '-')
        .unwrap_or(after_inner.len());
    let name = &after_inner[..end];
    let after_name = &after_inner[end..];

    // Build the inner variable expression
    // For &name, use BareWord so the undeclared-name check catches it
    let inner_expr = match inner_sigil {
        '$' => Expr::Var(name.to_string()),
        '@' => Expr::ArrayVar(name.to_string()),
        '%' => Expr::HashVar(name.to_string()),
        '&' => Expr::BareWord(name.to_string()),
        _ => unreachable!(),
    };

    // Check for postcircumfix that matches the outer sigil's context
    let mut remainder = after_name;
    let mut consumed = false;
    match outer_sigil {
        '$' | '@' => {
            // Expect [] for zen-slice or [expr]
            if let Some(r) = remainder.strip_prefix("[]") {
                remainder = r;
                consumed = true;
            } else if remainder.starts_with('[') {
                let (expr, r) = parse_postcircumfix_index(remainder, inner_expr.clone());
                if r.len() < remainder.len() {
                    if !current.is_empty() {
                        parts.push(Expr::Literal(Value::str(std::mem::take(current))));
                    }
                    parts.push(expr);
                    return Some(r);
                }
            }
        }
        '%' => {
            // Expect {} for zen-slice or {expr}
            if let Some(r) = remainder.strip_prefix("{}") {
                remainder = r;
                consumed = true;
            } else if remainder.starts_with('{') {
                let (expr, r) = parse_postcircumfix_index(remainder, inner_expr.clone());
                if r.len() < remainder.len() {
                    if !current.is_empty() {
                        parts.push(Expr::Literal(Value::str(std::mem::take(current))));
                    }
                    parts.push(expr);
                    return Some(r);
                }
            }
        }
        '&' => {
            // Expect () for call
            if let Some(r) = remainder.strip_prefix("()") {
                remainder = r;
                consumed = true;
            } else if remainder.starts_with('(') {
                let mut depth = 0usize;
                let mut paren_end = None;
                for (idx, ch) in remainder.char_indices() {
                    if ch == '(' {
                        depth += 1;
                    } else if ch == ')' {
                        depth -= 1;
                        if depth == 0 {
                            paren_end = Some(idx);
                            break;
                        }
                    }
                }
                if let Some(pe) = paren_end {
                    consumed = true;
                    remainder = &remainder[pe + 1..];
                }
            }
        }
        _ => {}
    }

    // For @/$ context without postcircumfix, also check for {} and method calls
    if !consumed && matches!(outer_sigil, '$' | '@') {
        let (expr, r) = parse_postcircumfix_index(remainder, inner_expr.clone());
        if r.len() < remainder.len() {
            if !current.is_empty() {
                parts.push(Expr::Literal(Value::str(std::mem::take(current))));
            }
            parts.push(expr);
            return Some(r);
        }
        return None;
    }

    if !consumed {
        return None;
    }

    if !current.is_empty() {
        parts.push(Expr::Literal(Value::str(std::mem::take(current))));
    }
    parts.push(inner_expr);
    Some(remainder)
}

pub(crate) fn has_malformed_angle_interpolation(rest: &str) -> bool {
    let mut chars = rest.chars();
    let Some(sigil) = chars.next() else {
        return false;
    };
    if !matches!(sigil, '$' | '@' | '%') {
        return false;
    }
    let mut after_name = chars.as_str();
    let mut saw_name = false;
    while let Some(ch) = after_name.chars().next() {
        if ch.is_alphanumeric() || ch == '_' || ch == '-' {
            saw_name = true;
            after_name = &after_name[ch.len_utf8()..];
        } else {
            break;
        }
    }
    if !saw_name {
        return false;
    }
    if let Some(after_open) = after_name.strip_prefix("<<") {
        return !after_open.contains(">>");
    }
    if let Some(after_open) = after_name.strip_prefix('«') {
        return !after_open.contains('»');
    }
    if let Some(after_open) = after_name.strip_prefix('<') {
        return !after_open.contains('>');
    }
    false
}
