/// Array (@), hash (%), and code (&) sigil variable parsing.
///
/// Covers `@array`, `%hash`, `&code`, sigil-context coercions, symbolic
/// derefs, and operator-code-ref syntax (`&infix:<+>`, `&[op]`).
use std::sync::atomic::{AtomicU64, Ordering};

use crate::ast::Expr;
use crate::parser::helpers::{is_raku_identifier_start, normalize_raku_identifier, ws};
use crate::parser::parse_result::{PError, PResult, parse_char};

use super::adverb::parse_var_name_adverb_suffixes;
use super::ident::{
    is_pseudo_package, parse_ident_with_hyphens, parse_qualified_ident_with_hyphens,
};
use super::perl5::detect_perl5_sigil_var;
use super::scalar::scalar_var;

static ANON_ARRAY_COUNTER: AtomicU64 = AtomicU64::new(0);

/// Parse an @array variable reference.
pub(crate) fn array_var(input: &str) -> PResult<'_, Expr> {
    let (input, _) = parse_char(input, '@')?;
    // Detect Perl 5 special array variables (@-, @+) and throw X::Syntax::Perl5Var.
    if let Some(err) = detect_perl5_sigil_var('@', input) {
        return Err(PError::fatal(err));
    }
    // Symbolic variable dereference: @::("name")
    if let Some(after_colons) = input.strip_prefix("::(") {
        let (after_expr, expr) = crate::parser::expr::expression(after_colons)?;
        let (after_expr, _) = ws(after_expr)?;
        let (rest, _) = parse_char(after_expr, ')')?;
        return Ok((
            rest,
            Expr::SymbolicDeref {
                sigil: "@".to_string(),
                expr: Box::new(expr),
            },
        ));
    }
    // Handle `.` twigil: @.attr → self.attr (in list context)
    if input.starts_with('.')
        && input.len() > 1
        && (input.as_bytes()[1].is_ascii_alphabetic() || input.as_bytes()[1] == b'_')
    {
        let after_dot = &input[1..];
        let (rest, name) = parse_qualified_ident_with_hyphens(after_dot)?;
        return Ok((rest, Expr::ArrayVar(format!(".{}", name))));
    }
    // Handle twigils
    let (rest, twigil) = if input.starts_with('*')
        || input.starts_with('?')
        || input.starts_with('!')
        || input.starts_with('^')
        || (input.starts_with('~') && input.len() > 1 && input.as_bytes()[1].is_ascii_alphabetic())
    {
        (&input[1..], &input[..1])
    } else {
        (input, "")
    };
    // Contextualized scalar specials (e.g., @$/, @$_): parse `$...` then lift
    // to an array variable targeting the same underlying name.
    if twigil.is_empty() && rest.starts_with('$') {
        // Handle @$<name> — list coercion of a named capture variable
        if let Ok((r2, expr @ Expr::CaptureVar(_))) = scalar_var(rest) {
            return Ok((
                r2,
                Expr::MethodCall {
                    target: Box::new(expr),
                    name: crate::symbol::Symbol::intern("list"),
                    args: vec![],
                    modifier: None,
                    quoted: false,
                },
            ));
        }
        if let Ok((r2, Expr::Var(name))) = scalar_var(rest) {
            return Ok((r2, Expr::ArrayVar(name)));
        }
    }
    // @{expr} is Perl 5 array dereference syntax — throw X::Obsolete
    if twigil.is_empty()
        && rest.starts_with('{')
        && let Ok((_r2, inner)) = crate::parser::primary::misc::block_or_hash_expr(rest)
        && !matches!(inner, crate::ast::Expr::Hash(_))
    {
        return Err(PError::fatal(
            "X::Obsolete: Unsupported use of @{expr}. In Raku please use: @(expr).".to_string(),
        ));
    }
    // Handle @<name> — list coercion of a named capture variable from $/
    // (e.g., after a regex match, @<fie> gives the positional elements of $<fie>)
    if twigil.is_empty()
        && rest.starts_with('<')
        && let Some(after_lt) = rest.strip_prefix('<')
        && let Some(end) = after_lt.find('>')
    {
        let name = &after_lt[..end];
        if !name.is_empty() {
            let after_gt = &after_lt[end + 1..];
            return Ok((
                after_gt,
                Expr::MethodCall {
                    target: Box::new(Expr::CaptureVar(name.to_string())),
                    name: crate::symbol::Symbol::intern("list"),
                    args: vec![],
                    modifier: None,
                    quoted: false,
                },
            ));
        }
    }
    // Bare @ (anonymous array variable) — each occurrence gets a unique name
    let next_is_ident =
        !rest.is_empty() && rest.chars().next().is_some_and(is_raku_identifier_start);
    if !next_is_ident && twigil.is_empty() {
        let id = ANON_ARRAY_COUNTER.fetch_add(1, Ordering::Relaxed);
        return Ok((rest, Expr::ArrayVar(format!("__ANON_ARRAY_{id}__"))));
    }
    let (rest, name) = parse_qualified_ident_with_hyphens(rest)?;
    let (rest, name) = parse_var_name_adverb_suffixes(rest, name);
    let full_name = if twigil.is_empty() {
        name
    } else {
        format!("{}{}", twigil, name)
    };
    Ok((rest, Expr::ArrayVar(full_name)))
}

/// Parse a %hash variable reference.
pub(crate) fn hash_var(input: &str) -> PResult<'_, Expr> {
    let (input, _) = parse_char(input, '%')?;
    // Detect Perl 5 special hash variables (%-, %+, %!) and throw X::Syntax::Perl5Var.
    if let Some(err) = detect_perl5_sigil_var('%', input) {
        return Err(PError::fatal(err));
    }
    // Symbolic variable dereference: %::("name")
    if let Some(after_colons) = input.strip_prefix("::(") {
        let (after_expr, expr) = crate::parser::expr::expression(after_colons)?;
        let (after_expr, _) = ws(after_expr)?;
        let (rest, _) = parse_char(after_expr, ')')?;
        return Ok((
            rest,
            Expr::SymbolicDeref {
                sigil: "%".to_string(),
                expr: Box::new(expr),
            },
        ));
    }
    // Handle `.` twigil: %.attr → self.attr (in hash context)
    if input.starts_with('.')
        && input.len() > 1
        && (input.as_bytes()[1].is_ascii_alphabetic() || input.as_bytes()[1] == b'_')
    {
        let after_dot = &input[1..];
        let (rest, name) = parse_qualified_ident_with_hyphens(after_dot)?;
        return Ok((rest, Expr::HashVar(format!(".{}", name))));
    }
    // Handle twigils
    let (rest, twigil) = if input.starts_with('*')
        || input.starts_with('?')
        || input.starts_with('!')
        || input.starts_with('^')
        || (input.starts_with('~') && input.len() > 1 && input.as_bytes()[1].is_ascii_alphabetic())
    {
        (&input[1..], &input[..1])
    } else {
        (input, "")
    };
    // Contextualized scalar specials (e.g., %$h, %$/, %$_): parse `$...` then
    // coerce to hash context via a `.hash` method call.
    if twigil.is_empty()
        && rest.starts_with('$')
        && let Ok((r2, expr)) = scalar_var(rest)
    {
        return Ok((
            r2,
            Expr::MethodCall {
                target: Box::new(expr),
                name: crate::symbol::Symbol::intern("hash"),
                args: vec![],
                modifier: None,
                quoted: false,
            },
        ));
    }
    // %{...} — hash coercion of a block expression
    // In Raku, %{expr} treats the block as a hash initializer.
    if twigil.is_empty()
        && rest.starts_with('{')
        && let Ok((r2, inner)) = crate::parser::primary::misc::block_or_hash_expr(rest)
    {
        // Wrap in hash() call — this will fail at runtime if odd number of elements
        return Ok((
            r2,
            Expr::Call {
                name: crate::symbol::Symbol::intern("hash"),
                args: vec![inner],
            },
        ));
    }
    // Handle %<name> — hash coercion of a named capture variable from $/
    // (e.g., after a regex match, %<foe> gives the hash view of $<foe>)
    if twigil.is_empty()
        && rest.starts_with('<')
        && let Some(after_lt) = rest.strip_prefix('<')
        && let Some(end) = after_lt.find('>')
    {
        let name = &after_lt[..end];
        if !name.is_empty() {
            let after_gt = &after_lt[end + 1..];
            return Ok((
                after_gt,
                Expr::MethodCall {
                    target: Box::new(Expr::CaptureVar(name.to_string())),
                    name: crate::symbol::Symbol::intern("hash"),
                    args: vec![],
                    modifier: None,
                    quoted: false,
                },
            ));
        }
    }
    // Bare % (anonymous hash variable)
    let next_is_ident =
        !rest.is_empty() && rest.chars().next().is_some_and(is_raku_identifier_start);
    if !next_is_ident && twigil.is_empty() {
        return Ok((rest, Expr::HashVar("__ANON_HASH__".to_string())));
    }
    // Special: %*ENV
    let (rest, name) = parse_qualified_ident_with_hyphens(rest)?;
    let (rest, name) = parse_var_name_adverb_suffixes(rest, name);
    let full_name = if twigil.is_empty() {
        name
    } else {
        format!("{}{}", twigil, name)
    };
    Ok((rest, Expr::HashVar(full_name)))
}

/// Parse a &code variable reference.
/// Handles `&foo`, twigil forms `&?BLOCK`, operator references like `&infix:<+>`,
/// package-qualified code refs like `&SETTING::OUTER::name`, and indirect package
/// lookups like `&::($expr)::name` or `&CALLER::($expr)::name`.
pub(crate) fn code_var(input: &str) -> PResult<'_, Expr> {
    let (input, _) = parse_char(input, '&')?;
    // Dereference callable stored in a variable/expression:
    // &$x, &@x, &%x, &($expr)
    if input.starts_with('$') {
        return scalar_var(input);
    }
    if input.starts_with('@') {
        return array_var(input);
    }
    if input.starts_with('%') {
        return hash_var(input);
    }
    if let Some(after_paren) = input.strip_prefix('(') {
        let (rest, expr) = crate::parser::expr::expression(after_paren)?;
        let (rest, _) = ws(rest)?;
        let (rest, _) = parse_char(rest, ')')?;
        return Ok((rest, expr));
    }
    // Callable block literal dereference: &{ ... }
    if input.starts_with('{') {
        let (rest, body) = crate::parser::primary::misc::parse_block_body(input)?;
        return Ok((
            rest,
            Expr::AnonSub {
                body,
                is_rw: false,
                is_block: true,
            },
        ));
    }
    // Handle &[op] — short form for &infix:<op>
    if let Some(after_bracket) = input.strip_prefix('[')
        && let Some(end_pos) = after_bracket.find(']')
    {
        let op_name = &after_bracket[..end_pos];
        let rest = &after_bracket[end_pos + 1..];
        let full_name = format!("infix:<{}>", op_name);
        return Ok((rest, Expr::CodeVar(full_name)));
    }
    // Handle &CALLER::($expr)::name — CALLER prefix followed by indirect lookup
    if let Some(after_caller) = input.strip_prefix("CALLER::(") {
        let (after_expr, expr) = crate::parser::expr::expression(after_caller)?;
        let (after_expr, _) = parse_char(after_expr, ')')?;
        let (after_expr, _) = ws(after_expr)?;
        if let Some(after_sep) = after_expr.strip_prefix("::") {
            let (rest, name) = parse_ident_with_hyphens(after_sep)?;
            return Ok((
                rest,
                Expr::IndirectCodeLookup {
                    package: Box::new(expr),
                    name: normalize_raku_identifier(name),
                },
            ));
        }
        return Err(PError::expected(":: after indirect package lookup"));
    }
    // Handle &::($expr)::name — indirect package lookup
    // Handle &::($expr) — symbolic code dereference (looks up &name at runtime)
    if let Some(after_colons) = input.strip_prefix("::(") {
        let (after_expr, expr) = crate::parser::expr::expression(after_colons)?;
        let (after_expr, _) = parse_char(after_expr, ')')?;
        let (after_expr, _) = ws(after_expr)?;
        if let Some(after_sep) = after_expr.strip_prefix("::") {
            let (rest, name) = parse_ident_with_hyphens(after_sep)?;
            return Ok((
                rest,
                Expr::IndirectCodeLookup {
                    package: Box::new(expr),
                    name: normalize_raku_identifier(name),
                },
            ));
        }
        // No ::name suffix — this is &::("name"), a symbolic code variable deref
        return Ok((
            after_expr,
            Expr::SymbolicDeref {
                sigil: "&".to_string(),
                expr: Box::new(expr),
            },
        ));
    }
    // Handle package-qualified code refs: &SETTING::OUTER::...::name
    // or &CALLER::SETTING::OUTER::...::name
    if let Ok((_, first_ident)) = parse_ident_with_hyphens(input)
        && is_pseudo_package(first_ident)
    {
        let after_first = &input[first_ident.len()..];
        if after_first.starts_with("::") {
            // Consume the full Package::Package::...::name chain
            let mut pos = 0;
            let mut qualified = String::new();
            loop {
                if let Ok((_, ident)) = parse_ident_with_hyphens(&input[pos..]) {
                    let after_ident = &input[pos + ident.len()..];
                    if after_ident.starts_with("::") && is_pseudo_package(ident) {
                        // This is a pseudo-package, consume it and the ::
                        qualified.push_str(ident);
                        qualified.push_str("::");
                        pos += ident.len() + 2;
                        continue;
                    }
                    // This is the final name
                    qualified.push_str(&normalize_raku_identifier(ident));
                    pos += ident.len();
                    return Ok((&input[pos..], Expr::CodeVar(qualified)));
                }
                break;
            }
        }
    }
    // Handle package-qualified code refs: &ClassName::method, &Foo::Bar::method, etc.
    if let Ok((_, first_ident)) = parse_ident_with_hyphens(input) {
        let after_first = &input[first_ident.len()..];
        if after_first.starts_with("::") {
            let mut pos = 0;
            let mut parts: Vec<String> = Vec::new();
            // Consume all Ident:: segments
            loop {
                if let Ok((_, ident)) = parse_ident_with_hyphens(&input[pos..]) {
                    let after_ident = &input[pos + ident.len()..];
                    if after_ident.starts_with("::") {
                        parts.push(normalize_raku_identifier(ident));
                        pos += ident.len() + 2; // skip ident and ::
                        continue;
                    }
                    // Final identifier (no :: after it)
                    parts.push(normalize_raku_identifier(ident));
                    pos += ident.len();
                    if parts.len() >= 2 {
                        let qualified = parts.join("::");
                        return Ok((&input[pos..], Expr::CodeVar(qualified)));
                    }
                }
                break;
            }
        }
    }
    // Handle twigils on code vars: &?BLOCK, &!DISPATCHER, &^x, &*FOO
    let (rest, twigil) = if let Some(stripped) = input.strip_prefix('?') {
        (stripped, "?")
    } else if let Some(stripped) = input.strip_prefix('!') {
        (stripped, "!")
    } else if let Some(stripped) = input.strip_prefix('^') {
        (stripped, "^")
    } else if let Some(stripped) = input.strip_prefix('*') {
        (stripped, "*")
    } else {
        (input, "")
    };
    if twigil.is_empty()
        && let Ok((op_rest, op_name)) = crate::parser::stmt::parse_sub_name_pub(rest)
        && matches!(
            op_name.as_str(),
            n if n.starts_with("infix:<")
                || n.starts_with("prefix:<")
                || n.starts_with("postfix:<")
                || n.starts_with("term:<")
                || n.starts_with("circumfix:<")
                || n.starts_with("postcircumfix:<")
        )
    {
        return Ok((op_rest, Expr::CodeVar(op_name)));
    }

    let (rest, name) = parse_ident_with_hyphens(rest)?;
    // Check for operator reference:
    // &infix:<OP>, &infix:<<OP>>, &infix:«OP», &[op], ...
    if twigil.is_empty()
        && matches!(
            name,
            "infix" | "prefix" | "postfix" | "term" | "circumfix" | "postcircumfix"
        )
        && let Some((r, full_name)) = parse_operator_code_ref_suffix(name, rest)
    {
        return Ok((r, Expr::CodeVar(full_name)));
    }
    let full_name = if twigil.is_empty() {
        normalize_raku_identifier(name)
    } else {
        format!("{}{}", twigil, normalize_raku_identifier(name))
    };
    Ok((rest, Expr::CodeVar(full_name)))
}

fn parse_operator_code_ref_suffix<'a>(category: &str, rest: &'a str) -> Option<(&'a str, String)> {
    if let Some(after_open) = rest.strip_prefix(":<<")
        && let Some(end_pos) = after_open.find(">>")
    {
        let symbol = after_open[..end_pos].trim();
        let after_close = &after_open[end_pos + 2..];
        return Some((after_close, format!("{category}:<{symbol}>")));
    }

    if let Some(after_open) = rest.strip_prefix(":<") {
        // Nested <...> is valid in categorical operator names.
        let mut depth = 1u32;
        let mut chars = after_open.char_indices();
        while let Some((i, c)) = chars.next() {
            match c {
                '>' => {
                    depth -= 1;
                    if depth == 0 {
                        let symbol = &after_open[..i];
                        let after_close = &after_open[i + 1..];
                        return Some((after_close, format!("{category}:<{symbol}>")));
                    }
                }
                '<' => depth += 1,
                '\\' => {
                    chars.next();
                }
                _ => {}
            }
        }
    }

    if let Some(after_open) = rest.strip_prefix(":\u{ab}")
        && let Some(end_pos) = after_open.find('\u{bb}')
    {
        let symbol = after_open[..end_pos].trim();
        let after_close = &after_open[end_pos + '\u{bb}'.len_utf8()..];
        return Some((after_close, format!("{category}:<{symbol}>")));
    }

    if let Some(after_open) = rest.strip_prefix(":['")
        && let Some(end_pos) = after_open.find("']")
    {
        let symbol = after_open[..end_pos]
            .replace("\\'", "'")
            .replace("\\\\", "\\");
        let after_close = &after_open[end_pos + 2..];
        return Some((after_close, format!("{category}:<{symbol}>")));
    }

    if let Some(after_open) = rest.strip_prefix(":[\"")
        && let Some(end_pos) = after_open.find("\"]")
    {
        let symbol = &after_open[..end_pos];
        let after_close = &after_open[end_pos + 2..];
        return Some((after_close, format!("{category}:<{symbol}>")));
    }

    None
}
