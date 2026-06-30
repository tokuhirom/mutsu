use crate::ast::Expr;
use crate::parser::helpers::ws;
use crate::parser::parse_result::{PResult, parse_char, take_while1};
use crate::symbol::Symbol;

pub(crate) fn auto_invoke_bareword_method_target(expr: Expr) -> Expr {
    let Expr::BareWord(name) = expr else {
        return expr;
    };
    // When a name is declared as a type (class/role/grammar/enum) in scope, a
    // bareword-dot like `foo.new` is a method call on the type object, NOT a
    // call to a same-named sub. The type shadows the sub for this syntax — e.g.
    // a file-scope `sub foo` plus an inner `my class foo` (S06-advanced/wrap.t).
    if crate::parser::stmt::simple::is_user_declared_type(&name) {
        return Expr::BareWord(name);
    }
    if crate::parser::stmt::simple::is_user_declared_sub(&name)
        || crate::parser::stmt::simple::is_imported_function(&name)
    {
        return Expr::Call {
            name: Symbol::intern(&name),
            args: Vec::new(),
        };
    }
    Expr::BareWord(name)
}

pub(crate) fn append_call_arg(expr: &mut Expr, arg: Expr) -> bool {
    match expr {
        Expr::Call { args, .. } => {
            args.push(arg);
            true
        }
        Expr::MethodCall { args, .. } => {
            args.push(arg);
            true
        }
        Expr::CallOn { args, .. } => {
            args.push(arg);
            true
        }
        Expr::HyperMethodCall { args, .. } => {
            args.push(arg);
            true
        }
        Expr::HyperMethodCallDynamic { args, .. } => {
            args.push(arg);
            true
        }
        _ => false,
    }
}

/// Result of parsing bracket indices — either a single-dimension index or
/// a multi-dimensional (semicolon-separated) index.
pub(crate) enum ParsedBracketIndex {
    /// Normal single-dimension index (possibly comma-separated list).
    Single(Expr),
    /// Multi-dimensional index: dimensions separated by semicolons.
    /// Each dimension can itself be a comma-separated list.
    MultiDim(Vec<Expr>),
}

pub(crate) fn parse_bracket_indices(input: &str) -> PResult<'_, Expr> {
    let (rest, parsed) = parse_bracket_indices_inner(input)?;
    match parsed {
        ParsedBracketIndex::Single(expr) => Ok((rest, expr)),
        // For callers that don't handle MultiDim, flatten to ArrayLiteral.
        // The postfix parser will use parse_bracket_indices_inner directly.
        ParsedBracketIndex::MultiDim(dims) => Ok((rest, Expr::ArrayLiteral(dims))),
    }
}

pub(crate) fn parse_bracket_indices_inner(input: &str) -> PResult<'_, ParsedBracketIndex> {
    // Allow phaser-only blocks (e.g. `%h{ CATCH { } }`) inside subscripts.
    // In Raku this evaluates the block which returns Nil and then indexes
    // the hash with Nil; we represent it as a Whatever placeholder to keep
    // parsing alive (the evaluation will produce a Nil result at runtime).
    {
        let probe = input.trim_start();
        if probe.starts_with("CATCH") || probe.starts_with("CONTROL") {
            let kw_len = if probe.starts_with("CATCH") { 5 } else { 7 };
            let after_kw = &probe[kw_len..];
            if after_kw.is_empty()
                || after_kw.starts_with(' ')
                || after_kw.starts_with('\t')
                || after_kw.starts_with('{')
            {
                // Skip whitespace, then parse the phaser body block and discard.
                let (r, _) = ws(after_kw)?;
                if r.starts_with('{') {
                    // Skip a balanced brace block.
                    let bytes = r.as_bytes();
                    let mut depth = 0i32;
                    let mut i = 0;
                    while i < bytes.len() {
                        match bytes[i] {
                            b'{' => depth += 1,
                            b'}' => {
                                depth -= 1;
                                if depth == 0 {
                                    i += 1;
                                    break;
                                }
                            }
                            _ => {}
                        }
                        i += 1;
                    }
                    let after_block = &r[i..];
                    let (after_block, _) = ws(after_block)?;
                    return Ok((after_block, ParsedBracketIndex::Single(Expr::Whatever)));
                }
            }
        }
    }
    let (r, first) = crate::parser::expr::expression(input)?;
    let mut current_dim = vec![first];
    let mut dimensions: Vec<Expr> = Vec::new();
    let mut has_semicolons = false;
    let mut has_trailing_comma = false;
    let mut r = r;
    loop {
        let (r2, _) = ws(r)?;
        if r2.starts_with(',') {
            let (r3, _) = parse_char(r2, ',')?;
            let (r3, _) = ws(r3)?;
            // Handle trailing comma before ']' or ';'
            if r3.starts_with(']') || r3.starts_with(';') {
                has_trailing_comma = true;
                r = r3;
                continue;
            }
            let (r3, next) = crate::parser::expr::expression(r3)?;
            // `@a[0, 2 ... *]` — the sequence operator's seed is the WHOLE
            // preceding comma list, not just its immediate left operand
            // (raku parses this as `(0, 2) ... *`). When a comma item turns out
            // to be a sequence, fold the items gathered so far into its seed.
            if let Expr::Binary {
                left,
                op:
                    op @ (crate::token_kind::TokenKind::DotDotDot
                    | crate::token_kind::TokenKind::DotDotDotCaret),
                right,
            } = next
            {
                let mut seed = std::mem::take(&mut current_dim);
                seed.push(*left);
                current_dim = vec![Expr::Binary {
                    left: Box::new(Expr::ArrayLiteral(seed)),
                    op,
                    right,
                }];
            } else {
                current_dim.push(next);
            }
            r = r3;
            continue;
        }
        if r2.starts_with(';') && !r2.starts_with(";;") {
            has_semicolons = true;
            // Finish current dimension
            let dim_expr = if current_dim.len() == 1 {
                current_dim.remove(0)
            } else {
                Expr::ArrayLiteral(std::mem::take(&mut current_dim))
            };
            dimensions.push(dim_expr);
            current_dim = Vec::new();
            let (r3, _) = parse_char(r2, ';')?;
            let (r3, _) = ws(r3)?;
            let (r3, next) = crate::parser::expr::expression(r3)?;
            current_dim.push(next);
            r = r3;
            continue;
        }
        if has_semicolons {
            // Finish last dimension
            let dim_expr = if current_dim.len() == 1 {
                current_dim.remove(0)
            } else {
                Expr::ArrayLiteral(current_dim)
            };
            dimensions.push(dim_expr);
            return Ok((r2, ParsedBracketIndex::MultiDim(dimensions)));
        }
        return Ok((
            r2,
            ParsedBracketIndex::Single(if current_dim.len() == 1 && !has_trailing_comma {
                current_dim.remove(0)
            } else {
                Expr::ArrayLiteral(current_dim)
            }),
        ));
    }
}

/// Result of parsing a quoted method name.
pub(crate) enum QuotedMethodName {
    /// Static method name (single-quoted or double-quoted without interpolation)
    Static(String),
    /// Dynamic method name (double-quoted with interpolation)
    Dynamic(Expr),
}

pub(crate) fn parse_quoted_method_name(input: &str) -> Option<(&str, QuotedMethodName)> {
    if input.starts_with('\'') {
        // Single-quoted: no interpolation
        let start = 1;
        let mut end = None;
        for (i, c) in input[start..].char_indices() {
            if c == '\'' {
                end = Some(i);
                break;
            }
        }
        let end = end?;
        let content = &input[start..start + end];
        let rest = &input[start + end + 1..];
        return Some((rest, QuotedMethodName::Static(content.to_string())));
    }
    if input.starts_with('"') {
        // Double-quoted: may have interpolation. A `{ … }` interpolation block can
        // itself contain `"`-delimited string literals (`."{$c++; "new"}"`), so the
        // closing quote of the method name is only the `"` seen at brace-depth 0
        // and outside any inner string literal.
        let start = 1;
        let mut escaped = false;
        let mut brace_depth = 0i32;
        let mut in_inner_str = false;
        let mut end = None;
        for (i, c) in input[start..].char_indices() {
            if escaped {
                escaped = false;
                continue;
            }
            if c == '\\' {
                escaped = true;
                continue;
            }
            if in_inner_str {
                // Inside a `"…"` literal nested in a `{ … }` block: only its own
                // closing quote matters; braces here are string content.
                if c == '"' {
                    in_inner_str = false;
                }
                continue;
            }
            match c {
                '{' => brace_depth += 1,
                '}' => brace_depth = (brace_depth - 1).max(0),
                '"' if brace_depth == 0 => {
                    end = Some(i);
                    break;
                }
                '"' => in_inner_str = true,
                _ => {}
            }
        }
        let end = end?;
        let content = &input[start..start + end];
        let rest = &input[start + end + 1..];
        // Check if content has interpolation
        if content.contains('$') || content.contains('@') || content.contains('{') {
            use crate::parser::primary::string::interpolate_string_content_with_modes;
            let name_expr = interpolate_string_content_with_modes(content, true, true);
            return Some((rest, QuotedMethodName::Dynamic(name_expr)));
        }
        return Some((rest, QuotedMethodName::Static(content.to_string())));
    }
    None
}

pub(crate) fn parse_private_method_name(input: &str) -> Option<(&str, String)> {
    let mut rest = input;
    let mut name = String::new();
    let mut first = true;
    loop {
        let (r, part) =
            take_while1(rest, |c: char| c.is_alphanumeric() || c == '_' || c == '-').ok()?;
        if !first {
            name.push_str("::");
        }
        name.push_str(part);
        first = false;
        rest = r;
        if let Some(r2) = rest.strip_prefix("::") {
            rest = r2;
            continue;
        }
        break;
    }
    Some((rest, name))
}

/// Parse the operator name from a prefix-as-postfix construct like `<->`, `«~»`, `<<~>>`, `["~"]`.
/// Input starts after the `:` in `.:<->`.
pub(crate) fn parse_prefix_as_postfix(input: &str) -> Option<(&str, String)> {
    // <<op>> (must be checked before <op>)
    if let Some(rest) = input.strip_prefix("<<") {
        // Handle both <<op>> and <<'op'>>
        let end = rest.find(">>")?;
        let mut op = &rest[..end];
        // Strip quotes if present: <<'op'>>
        if let Some(inner) = op.strip_prefix('\'').and_then(|s| s.strip_suffix('\'')) {
            op = inner;
        }
        if op.is_empty() {
            return None;
        }
        return Some((&rest[end + 2..], op.to_string()));
    }
    // <op>
    if let Some(rest) = input.strip_prefix('<') {
        let end = rest.find('>')?;
        let op = &rest[..end];
        if op.is_empty() {
            return None;
        }
        return Some((&rest[end + 1..], op.to_string()));
    }
    // «op» (U+00AB / U+00BB)
    if let Some(rest) = input.strip_prefix('\u{00AB}') {
        let end = rest.find('\u{00BB}')?;
        let op = &rest[..end];
        if op.is_empty() {
            return None;
        }
        return Some((&rest[end + '\u{00BB}'.len_utf8()..], op.to_string()));
    }
    // ["op"]
    if let Some(rest) = input.strip_prefix('[')
        && let Some(rest) = rest.strip_prefix('"')
    {
        let end = rest.find('"')?;
        let op = &rest[..end];
        let rest = &rest[end + 1..];
        let rest = rest.strip_prefix(']')?;
        if op.is_empty() {
            return None;
        }
        return Some((rest, op.to_string()));
    }
    None
}

pub(crate) fn is_postfix_operator_char(c: char) -> bool {
    if c.is_whitespace() || c.is_alphanumeric() || c == '_' {
        return false;
    }
    // Exclude characters that are infix operators or special symbols
    // U+2026 HORIZONTAL ELLIPSIS (…) is the sequence infix operator
    // U+221E INFINITY (∞) is a numeric literal, not an operator char
    if c == '\u{2026}' || c == '\u{221E}' {
        return false;
    }
    !matches!(
        c,
        '.' | ','
            | ';'
            | ':'
            | '('
            | ')'
            | '['
            | ']'
            | '{'
            | '}'
            | '"'
            | '\''
            | '\\'
            | '$'
            | '@'
            | '%'
            | '&'
            | '#'
    )
}

pub(crate) fn is_postfix_operator_boundary(rest: &str) -> bool {
    rest.is_empty()
        || rest.starts_with(|c: char| {
            c.is_whitespace() || matches!(c, '.' | ')' | '}' | ']' | ',' | ';' | ':')
        })
}

pub(crate) fn has_ternary_else_after(input: &str) -> bool {
    let mut rest = input;
    while let Ok((next, _)) = ws(rest) {
        if next.len() == rest.len() {
            break;
        }
        rest = next;
    }
    rest.starts_with("!!")
}

pub(crate) fn parse_custom_postfix_operator(input: &str) -> Option<(String, usize)> {
    // Don't consume characters that are closing delimiters of circumfix operators
    if crate::parser::stmt::simple::is_circumfix_close_delimiter(input) {
        return None;
    }

    if input.starts_with('!') {
        return Some(("!".to_string(), '!'.len_utf8()));
    }

    let mut chars = input.chars();
    let first = chars.next()?;
    if first.is_ascii() || !is_postfix_operator_char(first) {
        return None;
    }

    let mut len = first.len_utf8();
    for c in chars {
        if c.is_ascii() || !is_postfix_operator_char(c) {
            break;
        }
        len += c.len_utf8();
    }
    Some((input[..len].to_string(), len))
}
