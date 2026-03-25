use super::super::super::expr::expression;
use super::super::super::parse_result::{PError, PResult, parse_char};
use super::ws;
use crate::ast::Expr;
use crate::symbol::Symbol;
use crate::token_kind::TokenKind;
use crate::value::Value;

pub(super) fn is_supported_variable_trait(trait_name: &str) -> bool {
    if matches!(trait_name, "default" | "export" | "dynamic") {
        return true;
    }
    // Native typed buffer traits (e.g. `my @a is buf8`, `my @a is blob16`)
    if matches!(
        trait_name,
        "buf"
            | "blob"
            | "buf8"
            | "buf16"
            | "buf32"
            | "buf64"
            | "blob8"
            | "blob16"
            | "blob32"
            | "blob64"
            | "utf8"
    ) {
        return true;
    }
    // Type-ish variable traits are accepted in roast (e.g. `is List`, `is Map`).
    trait_name
        .chars()
        .next()
        .is_some_and(|c| c.is_ascii_uppercase())
}

pub(super) fn parse_export_trait_tags(input: &str) -> PResult<'_, Vec<String>> {
    let (mut rest, _) = ws(input)?;
    let mut tags = Vec::new();
    if !rest.starts_with('(') {
        return Ok((rest, tags));
    }
    let after_open = &rest[1..];
    let mut depth = 1usize;
    let mut end: Option<usize> = None;
    for (i, ch) in after_open.char_indices() {
        match ch {
            '(' => depth += 1,
            ')' => {
                depth -= 1;
                if depth == 0 {
                    end = Some(i);
                    break;
                }
            }
            _ => {}
        }
    }
    let end = end.ok_or_else(|| PError::expected("closing ')' in export trait"))?;
    let inner = &after_open[..end];
    rest = &after_open[end + 1..];
    let mut i = 0usize;
    while i < inner.len() {
        let c = inner[i..].chars().next().unwrap_or('\0');
        let c_len = c.len_utf8();
        if c.is_whitespace() || c == ',' {
            i += c_len;
            continue;
        }
        if c == ':' {
            i += c_len;
            if let Some(next) = inner[i..].chars().next()
                && next == '!'
            {
                i += next.len_utf8();
            }
            let start = i;
            while i < inner.len() {
                let ch = inner[i..].chars().next().unwrap_or('\0');
                if ch.is_alphanumeric() || ch == '_' || ch == '-' {
                    i += ch.len_utf8();
                } else {
                    break;
                }
            }
            if i > start {
                let tag = inner[start..i].to_string();
                if !tags.iter().any(|t| t == &tag) {
                    tags.push(tag);
                }
            }
            continue;
        }
        i += c_len;
    }
    let (rest, _) = ws(rest)?;
    Ok((rest, tags))
}

pub(super) fn parse_sigilless_decl_name(input: &str) -> PResult<'_, String> {
    super::super::parse_sub_name_pub(input)
}

pub(in crate::parser::stmt) fn parse_array_shape_suffix(input: &str) -> PResult<'_, Vec<Expr>> {
    let (rest, _) = parse_char(input, '[')?;
    let (rest, _) = ws(rest)?;
    let mut dims = Vec::new();
    let mut rest = rest;

    while !rest.starts_with(']') {
        let (r, dim) = expression(rest)?;
        dims.push(dim);
        let (r, _) = ws(r)?;
        if r.starts_with(',') || (r.starts_with(';') && !r.starts_with(";;")) {
            let sep = if r.starts_with(',') { ',' } else { ';' };
            let (r, _) = parse_char(r, sep)?;
            let (r, _) = ws(r)?;
            rest = r;
            continue;
        }
        rest = r;
    }

    let (rest, _) = parse_char(rest, ']')?;
    Ok((rest, dims))
}

pub(super) fn shaped_array_new_expr(dims: Vec<Expr>) -> Expr {
    let shape_value = if dims.len() == 1 {
        dims.into_iter()
            .next()
            .unwrap_or(Expr::Literal(Value::Int(0)))
    } else {
        Expr::ArrayLiteral(dims)
    };

    Expr::MethodCall {
        target: Box::new(Expr::BareWord("Array".to_string())),
        name: Symbol::intern("new"),
        args: vec![Expr::Binary {
            left: Box::new(Expr::Literal(Value::str_from("shape"))),
            op: TokenKind::FatArrow,
            right: Box::new(shape_value),
        }],
        modifier: None,
        quoted: false,
    }
}

pub(super) fn shaped_array_new_with_data_expr(dims: Vec<Expr>, data: Expr) -> Expr {
    let shape_value = if dims.len() == 1 {
        dims.into_iter()
            .next()
            .unwrap_or(Expr::Literal(Value::Int(0)))
    } else {
        Expr::ArrayLiteral(dims)
    };

    Expr::MethodCall {
        target: Box::new(Expr::BareWord("Array".to_string())),
        name: Symbol::intern("new"),
        args: vec![
            Expr::Binary {
                left: Box::new(Expr::Literal(Value::str_from("shape"))),
                op: TokenKind::FatArrow,
                right: Box::new(shape_value),
            },
            Expr::Binary {
                left: Box::new(Expr::Literal(Value::str_from("data"))),
                op: TokenKind::FatArrow,
                right: Box::new(data),
            },
        ],
        modifier: None,
        quoted: false,
    }
}

pub(super) fn register_term_symbol_from_decl_name(name: &str) {
    if let Some(callable_name) = name.strip_prefix('&') {
        super::super::simple::register_user_callable_term_symbol(callable_name);
        // Also register operator categories (infix, prefix, postfix, circumfix,
        // postcircumfix) as user subs so the parser recognizes them as operators.
        // This handles `constant &infix:<op> = ...` style declarations.
        if callable_name.starts_with("infix:")
            || callable_name.starts_with("prefix:")
            || callable_name.starts_with("postfix:")
            || callable_name.starts_with("circumfix:")
            || callable_name.starts_with("postcircumfix:")
        {
            super::super::simple::register_user_sub(callable_name);
        }
    } else {
        // Don't register keywords as term symbols — they must be handled
        // by the keyword-specific paths in identifier_or_call, not as
        // bare words via declared_term_symbol. E.g. `my $sub` should not
        // cause `sub` to be treated as a bare word on the RHS.
        if !is_parser_keyword(name) {
            super::super::simple::register_user_term_symbol(name);
        }
    }
}

pub(super) fn normalize_language_version(version_token: &str) -> String {
    if version_token.starts_with("v6.c") {
        "6.c".to_string()
    } else if version_token.starts_with("v6.d") {
        "6.d".to_string()
    } else {
        "6.e".to_string()
    }
}

pub(super) fn is_parser_keyword(name: &str) -> bool {
    matches!(
        name,
        "if" | "unless"
            | "for"
            | "while"
            | "until"
            | "given"
            | "when"
            | "loop"
            | "repeat"
            | "try"
            | "do"
            | "gather"
            | "sub"
            | "method"
            | "my"
            | "our"
            | "has"
            | "class"
            | "role"
            | "module"
            | "use"
            | "need"
            | "import"
            | "require"
            | "return"
            | "last"
            | "next"
            | "redo"
            | "die"
            | "say"
            | "print"
            | "put"
            | "note"
            | "with"
            | "without"
            | "supply"
            | "react"
            | "whenever"
            | "start"
            | "quietly"
            | "sink"
            | "let"
    )
}
