use super::*;
use crate::ast::Expr;
use crate::parser::expr::expression;
use crate::parser::primary::var::parse_var_name_from_str;
use crate::symbol::Symbol;
use crate::value::Value;

/// Try to interpolate a `$var` or `@var` at the current position.
/// Returns `Some(remaining_input)` if interpolation was performed, `None` otherwise.
pub(crate) fn try_interpolate_var<'a>(
    rest: &'a str,
    parts: &mut Vec<Expr>,
    current: &mut String,
) -> Option<&'a str> {
    let parse_postcircumfix_index = |input: &'a str, target: Expr| -> (Expr, &'a str) {
        // Double angle bracket indexing: $var<<key>> (must be checked before single <)
        if let Some(after_dlt) = input.strip_prefix("<<")
            && let Some(end) = after_dlt.find(">>")
        {
            let content = &after_dlt[..end];
            let index = parse_shell_words_index(content);
            return (
                Expr::Index {
                    target: Box::new(target),
                    index: Box::new(index),
                    is_positional: false,
                },
                &after_dlt[end + 2..],
            );
        }
        // French guillemet indexing: $var«key»
        if let Some(after_guillemet) = input.strip_prefix('\u{00AB}')
            && let Some(end) = after_guillemet.find('\u{00BB}')
        {
            let content = &after_guillemet[..end];
            let index = parse_shell_words_index(content);
            return (
                Expr::Index {
                    target: Box::new(target),
                    index: Box::new(index),
                    is_positional: false,
                },
                &after_guillemet[end + '\u{00BB}'.len_utf8()..],
            );
        }
        // Angle bracket indexing: $var<key>
        if let Some(after_lt) = input.strip_prefix('<')
            && let Some(end) = after_lt.find('>')
        {
            let content = &after_lt[..end];
            let words: Vec<&str> = content.split_whitespace().collect();
            let index = if words.len() <= 1 {
                Expr::Literal(Value::str(words.first().copied().unwrap_or("").to_string()))
            } else {
                Expr::ArrayLiteral(
                    words
                        .into_iter()
                        .map(|w| Expr::Literal(Value::str(w.to_string())))
                        .collect(),
                )
            };
            return (
                Expr::Index {
                    target: Box::new(target),
                    index: Box::new(index),
                    is_positional: false,
                },
                &after_lt[end + 1..],
            );
        }
        // Square bracket indexing: $var[expr]
        if let Some(after_bracket) = input.strip_prefix('[')
            && let Some(end) = after_bracket.find(']')
        {
            let content = after_bracket[..end].trim();
            // Parse the index expression (integer literal or simple expression)
            let index = if let Ok(n) = content.parse::<i64>() {
                Expr::Literal(Value::Int(n))
            } else {
                // Try parsing as an expression
                if let Ok((_, expr)) = crate::parser::expr::expression(content) {
                    expr
                } else {
                    Expr::Literal(Value::str(content.to_string()))
                }
            };
            return (
                Expr::Index {
                    target: Box::new(target),
                    index: Box::new(index),
                    is_positional: true,
                },
                &after_bracket[end + 1..],
            );
        }
        // Curly brace indexing: %hash{expr}
        if let Some(after_brace) = input.strip_prefix('{') {
            // Find matching closing brace respecting nesting
            let mut depth = 1usize;
            let mut brace_end = None;
            for (idx, ch) in after_brace.char_indices() {
                if ch == '{' {
                    depth += 1;
                } else if ch == '}' {
                    depth -= 1;
                    if depth == 0 {
                        brace_end = Some(idx);
                        break;
                    }
                }
            }
            if let Some(end) = brace_end {
                let content = after_brace[..end].trim();
                if !content.is_empty()
                    && let Ok((_, expr)) = crate::parser::expr::expression(content)
                {
                    return (
                        Expr::Index {
                            target: Box::new(target),
                            index: Box::new(expr),
                            is_positional: false,
                        },
                        &after_brace[end + 1..],
                    );
                }
            }
        }
        (target, input)
    };

    if rest.starts_with('$') && rest.len() > 1 {
        let next = rest.as_bytes()[1] as char;
        // Special variable $/ (match variable)
        if next == '/' {
            if !current.is_empty() {
                parts.push(Expr::Literal(Value::str(std::mem::take(current))));
            }
            let var_expr = Expr::Var("/".to_string());
            let (expr, var_rest) = parse_postcircumfix_index(&rest[2..], var_expr);
            let (expr, var_rest) = try_parse_interp_method_call(var_rest, expr);
            parts.push(expr);
            return Some(var_rest);
        }
        // $<key> is sugar for $/<key> (named capture shorthand)
        if next == '<'
            && let Some(after_lt) = rest.strip_prefix("$<")
            && let Some(end) = after_lt.find('>')
        {
            if !current.is_empty() {
                parts.push(Expr::Literal(Value::str(std::mem::take(current))));
            }
            let key = &after_lt[..end];
            let var_expr = Expr::Var("/".to_string());
            let index = Expr::Literal(Value::str(key.to_string()));
            let expr = Expr::Index {
                target: Box::new(var_expr),
                index: Box::new(index),
                is_positional: false,
            };
            let var_rest = &after_lt[end + 1..];
            let (expr, var_rest) = try_parse_interp_method_call(var_rest, expr);
            parts.push(expr);
            return Some(var_rest);
        }
        // Numeric capture variables: $0, $1, $2, ...
        if next.is_ascii_digit() {
            if !current.is_empty() {
                parts.push(Expr::Literal(Value::str(std::mem::take(current))));
            }
            let var_rest = &rest[1..];
            let end = var_rest
                .find(|c: char| !c.is_ascii_digit())
                .unwrap_or(var_rest.len());
            let digits = &var_rest[..end];
            // $0, $1 etc. are positional captures — Index into $/
            let index_val: i64 = digits.parse().unwrap_or(0);
            let expr = Expr::Index {
                target: Box::new(Expr::Var("/".to_string())),
                index: Box::new(Expr::Literal(Value::Int(index_val))),
                is_positional: true,
            };
            let (expr, var_rest) = try_parse_interp_method_call(&var_rest[end..], expr);
            parts.push(expr);
            return Some(var_rest);
        }
        // $(...) expression interpolation
        if next == '(' {
            let after_dollar = &rest[1..];
            // Find matching closing paren
            let mut depth = 0usize;
            let mut paren_end = None;
            for (idx, ch) in after_dollar.char_indices() {
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
                let inner = &after_dollar[1..pe];
                let remainder = &after_dollar[pe + 1..];
                // Try parsing as statement list first (supports if/for modifiers)
                let parsed = if let Ok((leftover, stmts)) =
                    crate::parser::stmt::stmt_list_pub(inner.trim())
                    && leftover.trim().is_empty()
                    && !stmts.is_empty()
                {
                    // When there's a single statement, use DoStmt so that
                    // For/If/etc. get expression-level compilation that
                    // collects results (e.g. list comprehensions).
                    if stmts.len() == 1 {
                        Some(Expr::DoStmt(Box::new(stmts.into_iter().next().unwrap())))
                    } else {
                        Some(Expr::DoBlock {
                            body: stmts,
                            label: None,
                        })
                    }
                } else if let Ok((leftover, expr)) = expression(inner.trim())
                    && leftover.trim().is_empty()
                {
                    Some(expr)
                } else {
                    None
                };
                if let Some(expr) = parsed {
                    if !current.is_empty() {
                        parts.push(Expr::Literal(Value::str(std::mem::take(current))));
                    }
                    parts.push(expr);
                    return Some(remainder);
                }
            }
        }
        // ${...} is Perl 5 scalar dereference syntax — throw X::Obsolete
        if next == '{' {
            if !current.is_empty() {
                parts.push(Expr::Literal(Value::str(std::mem::take(current))));
            }
            parts.push(Expr::Call {
                name: Symbol::intern("die"),
                args: vec![Expr::Literal(Value::str(
                    "X::Obsolete: Unsupported use of ${expr}. In Raku please use: $(expr)."
                        .to_string(),
                ))],
            });
            let after_brace = &rest[2..];
            let end = after_brace.find('}').unwrap_or(after_brace.len());
            return Some(if end < after_brace.len() {
                &after_brace[end + 1..]
            } else {
                after_brace
            });
        }
        if next.is_alphabetic()
            || next == '_'
            || next == '*'
            || next == '?'
            || next == '!'
            || next == '^'
        {
            if !current.is_empty() {
                parts.push(Expr::Literal(Value::str(std::mem::take(current))));
            }
            let var_rest = &rest[1..];
            let (var_rest, var_name) = parse_var_name_from_str(var_rest);
            let (expr, var_rest) = parse_postcircumfix_index(var_rest, Expr::Var(var_name));
            // Handle postcircumfix call interpolation: "$var()" / "$var(args)"
            let (expr, var_rest) = try_parse_interp_call(var_rest, expr);
            // Handle method call interpolation: "$var.method()" or "$var.method"
            let (expr, var_rest) = try_parse_interp_method_call(var_rest, expr);
            parts.push(expr);
            return Some(var_rest);
        }
        if next == '.' {
            let after_dot = &rest[2..];
            if !after_dot.is_empty() {
                let first = after_dot.as_bytes()[0];
                if first.is_ascii_alphabetic() || first == b'_' {
                    if !current.is_empty() {
                        parts.push(Expr::Literal(Value::str(std::mem::take(current))));
                    }
                    let end = after_dot
                        .find(|c: char| !c.is_alphanumeric() && c != '_' && c != '-')
                        .unwrap_or(after_dot.len());
                    let var_name = format!(".{}", &after_dot[..end]);
                    let (expr, remainder) =
                        parse_postcircumfix_index(&after_dot[end..], Expr::Var(var_name));
                    let (expr, remainder) = try_parse_interp_method_call(remainder, expr);
                    parts.push(expr);
                    return Some(remainder);
                }
            }
        }
        // Double-sigil interpolation: $$name, $@name, $%name, $&name
        if let Some(result) =
            try_double_sigil_interp(rest, parts, current, parse_postcircumfix_index)
        {
            return Some(result);
        }
    }
    if rest.starts_with('@') && rest.len() > 1 {
        let next = rest.as_bytes()[1] as char;
        // Double-sigil interpolation: @$name, @@name, @%name, @&name
        if matches!(next, '$' | '@' | '%' | '&')
            && let Some(result) =
                try_double_sigil_interp(rest, parts, current, parse_postcircumfix_index)
        {
            return Some(result);
        }
        // @{...} is Perl 5 array dereference syntax — throw X::Obsolete
        if next == '{' {
            if !current.is_empty() {
                parts.push(Expr::Literal(Value::str(std::mem::take(current))));
            }
            parts.push(Expr::Call {
                name: Symbol::intern("die"),
                args: vec![Expr::Literal(Value::str(
                    "X::Obsolete: Unsupported use of @{expr}. In Raku please use: @(expr)."
                        .to_string(),
                ))],
            });
            let after_brace = &rest[2..];
            let end = after_brace.find('}').unwrap_or(after_brace.len());
            return Some(if end < after_brace.len() {
                &after_brace[end + 1..]
            } else {
                after_brace
            });
        }
        if next.is_alphabetic() || next == '_' {
            let var_rest = &rest[1..];
            let end = var_rest
                .find(|c: char| !c.is_alphanumeric() && c != '_' && c != '-')
                .unwrap_or(var_rest.len());
            let name = &var_rest[..end];
            let expr = Expr::ArrayVar(name.to_string());
            let after_name = &var_rest[end..];
            let mut remainder = after_name;
            // Zen-slice interpolation: "@arr[]" and "@arr.[]" should interpolate the array value
            let mut consumed = false;
            if let Some(r) = remainder.strip_prefix(".[]") {
                remainder = r;
                consumed = true;
            } else if let Some(r) = remainder.strip_prefix("[]") {
                remainder = r;
                consumed = true;
            }
            let (expr, remainder) = parse_postcircumfix_index(remainder, expr);
            // Try method call interpolation: "@arr.method()" or "@arr.join(' ')"
            let (expr, remainder) = try_parse_interp_method_call(remainder, expr);
            if !consumed && remainder.len() == after_name.len() {
                // @array without postcircumfix ([], [n], <key>) or .method() is literal
                return None;
            }
            if !current.is_empty() {
                parts.push(Expr::Literal(Value::str(std::mem::take(current))));
            }
            parts.push(expr);
            return Some(remainder);
        }
    }
    if rest.starts_with('%') && rest.len() > 1 {
        let next = rest.as_bytes()[1] as char;
        // Double-sigil interpolation: %$name, %@name, %%name, %&name
        if matches!(next, '$' | '@' | '%' | '&')
            && let Some(result) =
                try_double_sigil_interp(rest, parts, current, parse_postcircumfix_index)
        {
            return Some(result);
        }
        if next.is_alphabetic() || next == '_' {
            let var_rest = &rest[1..];
            let end = var_rest
                .find(|c: char| !c.is_alphanumeric() && c != '_' && c != '-')
                .unwrap_or(var_rest.len());
            let name = &var_rest[..end];
            let expr = Expr::HashVar(name.to_string());
            let tail = &var_rest[end..];
            // Zen-slice: %hash{} should stringify the whole hash
            if let Some(r) = tail.strip_prefix("{}") {
                if !current.is_empty() {
                    parts.push(Expr::Literal(Value::str(std::mem::take(current))));
                }
                parts.push(expr);
                return Some(r);
            }
            // Zen-slice: %hash<> should stringify the whole hash
            if let Some(after_zen) = tail.strip_prefix("<>") {
                if !current.is_empty() {
                    parts.push(Expr::Literal(Value::str(std::mem::take(current))));
                }
                parts.push(expr);
                return Some(after_zen);
            }
            // Zen-slice: %hash[] should stringify the whole hash
            if let Some(r) = tail.strip_prefix("[]") {
                if !current.is_empty() {
                    parts.push(Expr::Literal(Value::str(std::mem::take(current))));
                }
                parts.push(expr);
                return Some(r);
            }
            let (expr, remainder) = parse_postcircumfix_index(tail, expr);
            // In qq-like strings, `%hash` without a postcircumfix remains literal.
            // Only interpolate hash variables for explicit access forms like
            // `%hash<key>` / `%hash{...}`.
            if remainder.len() == tail.len() {
                return None;
            }
            if !current.is_empty() {
                parts.push(Expr::Literal(Value::str(std::mem::take(current))));
            }
            parts.push(expr);
            return Some(remainder);
        }
    }
    // &func() interpolation — only with parentheses
    if rest.starts_with('&') && rest.len() > 1 {
        let next = rest.as_bytes()[1] as char;
        // Double-sigil interpolation: &$name, &@name, &%name
        if matches!(next, '$' | '@' | '%')
            && let Some(result) =
                try_double_sigil_interp(rest, parts, current, parse_postcircumfix_index)
        {
            return Some(result);
        }
        if next.is_alphabetic() || next == '_' {
            let var_rest = &rest[1..];
            let end = var_rest
                .find(|c: char| !c.is_alphanumeric() && c != '_' && c != '-')
                .unwrap_or(var_rest.len());
            let name = &var_rest[..end];
            let after_name = &var_rest[end..];
            // Must be followed by (...) to interpolate
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
                    let remainder = &after_name[pe + 1..];
                    // Parse arguments
                    let args = if args_str.trim().is_empty() {
                        vec![]
                    } else {
                        let mut args = vec![];
                        for arg in args_str.split(',') {
                            let arg = arg.trim();
                            if let Ok((_, expr)) = crate::parser::expr::expression(arg) {
                                args.push(expr);
                            }
                        }
                        args
                    };
                    if !current.is_empty() {
                        parts.push(Expr::Literal(Value::str(std::mem::take(current))));
                    }
                    parts.push(Expr::Call {
                        name: Symbol::intern(name),
                        args,
                    });
                    return Some(remainder);
                }
            }
        }
    }
    None
}
