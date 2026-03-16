use super::super::helpers::{skip_balanced_parens, ws, ws1};
use super::super::parse_result::{PError, PResult, opt_char, parse_char, take_while1};
use super::super::primary::regex::scan_to_delim;

use crate::ast::{Expr, ParamDef, Stmt};
use crate::symbol::Symbol;
use crate::token_kind::TokenKind;
use crate::value::Value;

use super::{block, ident, keyword, qualified_ident};

use super::sub::{parse_indirect_decl_name, parse_sub_name};
use super::{parse_param_list, parse_sub_traits};

fn parse_declarator_traits(input: &str) -> PResult<'_, Vec<(String, Value)>> {
    let mut traits = Vec::new();
    let (mut rest, _) = ws(input)?;
    loop {
        if !rest.starts_with(':') || rest.starts_with("::") {
            break;
        }
        rest = &rest[1..];
        let (r, name) = take_while1(rest, |c: char| c.is_alphanumeric() || c == '_' || c == '-')?;
        rest = r;
        let mut value = Value::Bool(true);
        if let Some(inner) = rest.strip_prefix('<') {
            if let Some(end) = inner.find('>') {
                value = Value::str(inner[..end].to_string());
                rest = &inner[end + 1..];
            } else {
                return Err(PError::expected("closing '>' in trait value"));
            }
        } else if rest.starts_with('(') {
            let after = skip_balanced_parens(rest);
            let body = &rest[1..rest.len() - after.len() - 1];
            value = Value::str(body.trim().to_string());
            rest = after;
        }
        traits.push((name.to_string(), value));
        let (r, _) = ws(rest)?;
        rest = r;
    }
    Ok((rest, traits))
}

fn parse_optional_bracket_suffix(input: &str) -> PResult<'_, String> {
    if !input.starts_with('[') {
        return Ok((input, String::new()));
    }
    let mut depth = 0u32;
    let mut end = None;
    for (i, ch) in input.char_indices() {
        if ch == '[' {
            depth += 1;
        } else if ch == ']' {
            depth = depth.saturating_sub(1);
            if depth == 0 {
                end = Some(i + 1);
                break;
            }
        }
    }
    let end = end.ok_or_else(|| PError::expected("closing ']'"))?;
    Ok((&input[end..], input[..end].to_string()))
}

fn meta_setter_stmt(type_name: &str, key: &str, value: Value) -> Stmt {
    Stmt::Expr(Expr::Call {
        name: Symbol::intern("__MUTSU_SET_META__"),
        args: vec![
            Expr::Literal(Value::str(type_name.to_string())),
            Expr::Literal(Value::str(key.to_string())),
            Expr::Literal(value),
        ],
    })
}

fn expr_uses_attr_twigil(expr: &Expr) -> bool {
    match expr {
        Expr::Var(name) | Expr::ArrayVar(name) | Expr::HashVar(name) => {
            name.starts_with('.') || name.starts_with('!')
        }
        Expr::MethodCall { target, args, .. } | Expr::HyperMethodCall { target, args, .. } => {
            expr_uses_attr_twigil(target) || args.iter().any(expr_uses_attr_twigil)
        }
        Expr::HyperMethodCallDynamic {
            target,
            name_expr,
            args,
            ..
        } => {
            expr_uses_attr_twigil(target)
                || expr_uses_attr_twigil(name_expr)
                || args.iter().any(expr_uses_attr_twigil)
        }
        Expr::Call { args, .. }
        | Expr::ArrayLiteral(args)
        | Expr::BracketArray(args, _)
        | Expr::CaptureLiteral(args)
        | Expr::StringInterpolation(args) => args.iter().any(expr_uses_attr_twigil),
        Expr::Unary { expr, .. } | Expr::PostfixOp { expr, .. } | Expr::Reduction { expr, .. } => {
            expr_uses_attr_twigil(expr)
        }
        Expr::Binary { left, right, .. }
        | Expr::MetaOp { left, right, .. }
        | Expr::HyperOp { left, right, .. } => {
            expr_uses_attr_twigil(left) || expr_uses_attr_twigil(right)
        }
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
        } => {
            expr_uses_attr_twigil(cond)
                || expr_uses_attr_twigil(then_expr)
                || expr_uses_attr_twigil(else_expr)
        }
        Expr::Index { target, index } => {
            expr_uses_attr_twigil(target) || expr_uses_attr_twigil(index)
        }
        Expr::IndexAssign {
            target,
            index,
            value,
        } => {
            expr_uses_attr_twigil(target)
                || expr_uses_attr_twigil(index)
                || expr_uses_attr_twigil(value)
        }
        Expr::AssignExpr { expr, .. } => expr_uses_attr_twigil(expr),
        Expr::DoBlock { body, .. }
        | Expr::Block(body)
        | Expr::Gather(body)
        | Expr::AnonSub { body, .. }
        | Expr::AnonSubParams { body, .. }
        | Expr::Lambda { body, .. } => body.iter().any(stmt_uses_attr_twigil),
        Expr::Try { body, catch } => {
            body.iter().any(stmt_uses_attr_twigil)
                || catch
                    .as_ref()
                    .is_some_and(|body| body.iter().any(stmt_uses_attr_twigil))
        }
        Expr::DoStmt(stmt) => stmt_uses_attr_twigil(stmt),
        Expr::CallOn { target, args } => {
            expr_uses_attr_twigil(target) || args.iter().any(expr_uses_attr_twigil)
        }
        Expr::InfixFunc { left, right, .. } => {
            expr_uses_attr_twigil(left) || right.iter().any(expr_uses_attr_twigil)
        }
        Expr::Exists { target, arg, .. } => {
            expr_uses_attr_twigil(target)
                || arg
                    .as_ref()
                    .is_some_and(|arg_expr| expr_uses_attr_twigil(arg_expr))
        }
        Expr::ZenSlice(inner) => expr_uses_attr_twigil(inner),
        _ => false,
    }
}

fn stmt_uses_attr_twigil(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Expr(expr)
        | Stmt::Return(expr)
        | Stmt::Take(expr)
        | Stmt::Die(expr)
        | Stmt::Fail(expr) => expr_uses_attr_twigil(expr),
        Stmt::VarDecl {
            expr,
            where_constraint,
            ..
        } => {
            expr_uses_attr_twigil(expr)
                || where_constraint
                    .as_ref()
                    .is_some_and(|wc| expr_uses_attr_twigil(wc))
        }
        Stmt::Assign { expr, .. } => expr_uses_attr_twigil(expr),
        Stmt::Block(body)
        | Stmt::SyntheticBlock(body)
        | Stmt::Package { body, .. }
        | Stmt::Catch(body)
        | Stmt::Control(body) => body.iter().any(stmt_uses_attr_twigil),
        Stmt::SubDecl { body, .. }
        | Stmt::MethodDecl { body, .. }
        | Stmt::TokenDecl { body, .. }
        | Stmt::RuleDecl { body, .. } => body.iter().any(stmt_uses_attr_twigil),
        Stmt::Label { stmt, .. } => stmt_uses_attr_twigil(stmt),
        _ => false,
    }
}

fn no_self_error() -> PError {
    let msg = "X::Syntax::NoSelf".to_string();
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    let ex = Value::make_instance(Symbol::intern("X::Syntax::NoSelf"), attrs);
    PError::fatal_with_exception(msg, Box::new(ex))
}

fn expr_is_bare_ident(expr: &Expr, ident: &str) -> bool {
    matches!(expr, Expr::Var(name) | Expr::BareWord(name) if name == ident)
}

fn stmt_is_also_is_rw(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Expr(Expr::InfixFunc {
            name, left, right, ..
        }) => {
            name == "is"
                && expr_is_bare_ident(left, "also")
                && right.len() == 1
                && expr_is_bare_ident(&right[0], "rw")
        }
        Stmt::Expr(Expr::Binary { left, op, right }) => {
            matches!(op, TokenKind::Ident(name) if name == "is")
                && expr_is_bare_ident(left, "also")
                && expr_is_bare_ident(right, "rw")
        }
        _ => false,
    }
}

/// Extract the parent class name from `also is <ClassName>` statements
/// (where ClassName is not `rw`).
fn stmt_also_is_parent(stmt: &Stmt) -> Option<String> {
    match stmt {
        Stmt::Expr(Expr::InfixFunc {
            name, left, right, ..
        }) if name == "is" && expr_is_bare_ident(left, "also") && right.len() == 1 => {
            if let Expr::BareWord(parent) = &right[0]
                && parent != "rw"
            {
                return Some(parent.clone());
            }
            None
        }
        Stmt::Expr(Expr::Binary { left, op, right }) => {
            if matches!(op, TokenKind::Ident(name) if name == "is")
                && expr_is_bare_ident(left, "also")
                && let Expr::BareWord(parent) = right.as_ref()
                && parent != "rw"
            {
                return Some(parent.clone());
            }
            None
        }
        _ => None,
    }
}

fn reject_no_self_in_subs(body: &[Stmt]) -> Result<(), PError> {
    for stmt in body {
        if let Stmt::SubDecl { body: sub_body, .. } = stmt
            && sub_body.iter().any(stmt_uses_attr_twigil)
        {
            return Err(no_self_error());
        }
    }
    Ok(())
}

fn consume_raw_braced_body(input: &str) -> PResult<'_, Vec<Stmt>> {
    if !input.starts_with('{') {
        return Err(PError::expected("raw braced body"));
    }
    let mut depth = 0u32;
    let mut i = 0usize;
    while i < input.len() {
        let ch = input[i..]
            .chars()
            .next()
            .ok_or_else(|| PError::expected("closing '}'"))?;
        let len = ch.len_utf8();
        match ch {
            '{' => depth += 1,
            '}' => {
                depth = depth.saturating_sub(1);
                if depth == 0 {
                    let rest = &input[i + len..];
                    return Ok((rest, Vec::new()));
                }
            }
            '\\' => {
                i += len;
                if i < input.len() {
                    let next_len = input[i..].chars().next().map(|c| c.len_utf8()).unwrap_or(0);
                    i += next_len;
                    continue;
                }
            }
            '\'' | '"' => {
                let quote = ch;
                i += len;
                while i < input.len() {
                    let c = input[i..]
                        .chars()
                        .next()
                        .ok_or_else(|| PError::expected("string close"))?;
                    let c_len = c.len_utf8();
                    if c == '\\' {
                        i += c_len;
                        if i < input.len() {
                            let n_len =
                                input[i..].chars().next().map(|n| n.len_utf8()).unwrap_or(0);
                            i += n_len;
                            continue;
                        }
                    }
                    i += c_len;
                    if c == quote {
                        break;
                    }
                }
                continue;
            }
            _ => {}
        }
        i += len;
    }
    Err(PError::expected("closing '}'"))
}

fn parse_token_like_name(input: &str) -> PResult<'_, String> {
    let (mut rest, mut name) = ident(input)?;
    loop {
        if !rest.starts_with(':') {
            break;
        }
        let r = &rest[1..];
        let (r, part) = take_while1(r, |c: char| c.is_alphanumeric() || c == '_' || c == '-')?;
        name.push(':');
        name.push_str(part);
        let mut r2 = r;
        if r2.starts_with('<')
            && let Some(end) = r2.find('>')
        {
            name.push_str(&r2[..=end]);
            r2 = &r2[end + 1..];
        } else if r2.starts_with('\u{ab}') {
            // «» (French quotes) — keep as «» internally to avoid
            // ambiguity when the value contains '>'
            let after_open = &r2['\u{ab}'.len_utf8()..];
            if let Some(end) = after_open.find('\u{bb}') {
                name.push('\u{ab}');
                name.push_str(&after_open[..end]);
                name.push('\u{bb}');
                r2 = &after_open[end + '\u{bb}'.len_utf8()..];
            }
        }
        rest = r2;
    }
    Ok((rest, name))
}

fn parse_raw_braced_regex_body(input: &str) -> PResult<'_, String> {
    let after_open = input
        .strip_prefix('{')
        .ok_or_else(|| PError::expected("regex body"))?;
    if let Some((body, rest)) = scan_to_delim(after_open, '{', '}', true) {
        return Ok((rest, body.trim().to_string()));
    }
    Err(PError::expected("regex closing delimiter"))
}

fn inject_implicit_rule_ws(pattern: &str) -> String {
    fn should_insert(prev: char, next: char) -> bool {
        !matches!(
            (prev, next),
            ('|', _)
                | (_, '|')
                | ('(', _)
                | (_, ')')
                | ('[', _)
                | (_, ']')
                | ('{', _)
                | (_, '}')
                | ('^', _)
                | (_, '$')
                | ('<', _)
                | (_, '>')
        )
    }

    let chars: Vec<char> = pattern.chars().collect();
    let mut out = String::new();
    let mut i = 0usize;
    let mut in_single = false;
    let mut in_double = false;
    let mut escaped = false;
    let mut brace_depth = 0usize;
    while i < chars.len() {
        let c = chars[i];
        if escaped {
            out.push(c);
            escaped = false;
            i += 1;
            continue;
        }
        if c == '\\' {
            out.push(c);
            escaped = true;
            i += 1;
            continue;
        }
        if c == '\'' && !in_double {
            in_single = !in_single;
            out.push(c);
            i += 1;
            continue;
        }
        if c == '"' && !in_single {
            in_double = !in_double;
            out.push(c);
            i += 1;
            continue;
        }
        // Track brace depth to skip ws injection inside code blocks { ... }
        if !in_single && !in_double {
            if c == '{' {
                brace_depth += 1;
                out.push(c);
                i += 1;
                continue;
            }
            if c == '}' && brace_depth > 0 {
                brace_depth -= 1;
                out.push(c);
                i += 1;
                continue;
            }
        }
        // Inside a code block — pass through without ws injection
        if brace_depth > 0 {
            out.push(c);
            i += 1;
            continue;
        }
        if !in_single && !in_double && c.is_whitespace() {
            let mut j = i;
            while j < chars.len() && chars[j].is_whitespace() {
                j += 1;
            }
            let prev = out.chars().rev().find(|ch| !ch.is_whitespace());
            let next = chars[j..].iter().copied().find(|ch| !ch.is_whitespace());
            if let (Some(p), Some(n)) = (prev, next) {
                if p == '^' {
                    if !out.ends_with(' ') && !out.is_empty() {
                        out.push(' ');
                    }
                    out.push_str("<.ws>?");
                    out.push(' ');
                } else if should_insert(p, n) {
                    if !out.ends_with(' ') && !out.is_empty() {
                        out.push(' ');
                    }
                    out.push_str("<.ws>");
                    out.push(' ');
                } else if !out.ends_with(' ') && !out.is_empty() {
                    out.push(' ');
                }
            }
            i = j;
            continue;
        }
        out.push(c);
        i += 1;
    }
    out.trim().to_string()
}

fn normalize_token_pattern(pattern: &str) -> String {
    let trimmed = pattern.trim();
    if trimmed.len() >= 2 && trimmed.starts_with('/') && trimmed.ends_with('/') {
        trimmed[1..trimmed.len() - 1].to_string()
    } else {
        trimmed.to_string()
    }
}

/// Parse `class` declaration.
pub(crate) fn class_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("class", input).ok_or_else(|| PError::expected("class declaration"))?;
    let (rest, _) = ws1(rest)?;
    class_decl_body(rest)
}

/// Parse `augment class ClassName { ... }` declaration (monkey-patching).
pub(crate) fn augment_class_decl(input: &str) -> PResult<'_, Stmt> {
    let rest =
        keyword("augment", input).ok_or_else(|| PError::expected("augment class declaration"))?;
    let (rest, _) = ws1(rest)?;
    let rest = keyword("class", rest).ok_or_else(|| PError::expected("'class' after 'augment'"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, name) = qualified_ident(rest)?;
    // Check for type adverbs (:D, :U, :auth, :ver, :api) which are not allowed on augment
    if rest.starts_with(':') && !rest.starts_with("::") {
        let adverb_rest = &rest[1..];
        // Check for known type adverbs
        let is_adverb = adverb_rest.starts_with('D')
            || adverb_rest.starts_with('U')
            || adverb_rest.starts_with("auth")
            || adverb_rest.starts_with("ver")
            || adverb_rest.starts_with("api");
        if is_adverb {
            let msg = "Cannot put adverbs on a typename when augmenting".to_string();
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("message".to_string(), Value::str(msg.clone()));
            let ex = Value::make_instance(Symbol::intern("X::Syntax::Augment::Adverb"), attrs);
            return Err(PError::fatal_with_exception(msg, Box::new(ex)));
        }
    }
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    Ok((
        rest,
        Stmt::AugmentClass {
            name: Symbol::intern(&name),
            body,
        },
    ))
}

/// Parse `anon class Name { ... }` declaration.
/// The class is created but not installed in the lexical scope.
/// Emits a ClassDecl (registered under the real name) followed by a call to
/// `__MUTSU_UNREGISTER_CLASS__` to remove the name from the scope.
pub(crate) fn anon_class_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("anon", input).ok_or_else(|| PError::expected("anon class declaration"))?;
    let (rest, _) = ws1(rest)?;
    let rest = keyword("class", rest).ok_or_else(|| PError::expected("class after anon"))?;
    let (rest, _) = ws1(rest)?;
    // Parse the class name
    let (rest, user_name) = qualified_ident(rest)?;
    let (rest, _) = ws(rest)?;
    // Parse parent classes
    let mut parents = Vec::new();
    let mut anon_repr: Option<String> = None;
    let mut r = rest;
    while let Some(r2) = keyword("is", r) {
        let (r2, _) = ws1(r2)?;
        let (r2, parent) = qualified_ident(r2)?;
        if parent == "repr" {
            if let Some(inner) = r2.strip_prefix('(') {
                let end = inner.find(')').unwrap_or(inner.len());
                let repr_val = inner[..end].trim().trim_matches('\'').trim_matches('"');
                anon_repr = Some(repr_val.to_string());
            }
            let r2 = skip_balanced_parens(r2);
            let (r2, _) = ws(r2)?;
            r = r2;
            continue;
        }
        parents.push(parent);
        let (r2, _) = ws(r2)?;
        r = r2;
    }
    let (rest, body) = block(r)?;
    reject_no_self_in_subs(&body)?;
    let class_decl = Stmt::ClassDecl {
        name: Symbol::intern(&user_name),
        name_expr: None,
        parents,
        class_is_rw: false,
        is_hidden: false,
        hidden_parents: Vec::new(),
        does_parents: Vec::new(),
        repr: anon_repr,
        body,
    };
    // Emit the class registration followed by unregistering the name from the scope
    let unregister = Stmt::Expr(Expr::Call {
        name: Symbol::intern("__MUTSU_UNREGISTER_CLASS__"),
        args: vec![Expr::Literal(Value::str(user_name))],
    });
    Ok((rest, Stmt::Block(vec![class_decl, unregister])))
}

/// Parse the body of a class declaration (after `class` keyword and whitespace).
pub(super) fn class_decl_body(input: &str) -> PResult<'_, Stmt> {
    let (rest, name, name_expr) = if input.starts_with("::") {
        let (rest, (name, expr)) = parse_indirect_decl_name(input)?;
        (rest, name, Some(expr))
    } else {
        let (rest, name) = qualified_ident(input)?;
        (rest, name, None)
    };
    let (rest, traits) = parse_declarator_traits(rest)?;
    let (rest, _) = ws(rest)?;

    // Parent clauses in any order: `is Parent`, `does Role[...]`, `hides Parent`.
    let mut is_hidden = false;
    let mut class_is_rw = false;
    let mut hidden_parents = Vec::new();
    let mut parents = Vec::new();
    let mut does_parents = Vec::new();
    let mut is_repr: Option<String> = None;
    let mut r = rest;
    loop {
        if let Some(r2) = keyword("is", r) {
            let (r2, _) = ws1(r2)?;
            // Handle `is ::Foo` (indirect name lookup) — treat the
            // `::Ident` as a parent name so that validation later
            // produces X::Inheritance::UnknownParent.
            let (r2, parent) = if let Some(stripped) = r2.strip_prefix("::") {
                let (r3, ident_part) = qualified_ident(stripped)?;
                (r3, format!("::{}", ident_part))
            } else {
                qualified_ident(r2)?
            };
            if parent == "hidden" {
                is_hidden = true;
            } else if parent == "rw" {
                class_is_rw = true;
            } else if parent == "repr" {
                // Extract repr value from `is repr('CUnion')` etc.
                if let Some(inner) = r2.strip_prefix('(') {
                    // Find the content between parens, stripping quotes
                    let end = inner.find(')').unwrap_or(inner.len());
                    let repr_val = inner[..end].trim().trim_matches('\'').trim_matches('"');
                    is_repr = Some(repr_val.to_string());
                }
                let r2 = skip_balanced_parens(r2);
                let (r2, _) = ws(r2)?;
                r = r2;
                continue;
            } else if parent.starts_with(|c: char| c.is_ascii_uppercase())
                || parent.starts_with("::")
            {
                let (r2, bracket_suffix) = parse_optional_bracket_suffix(r2)?;
                parents.push(format!("{}{}", parent, bracket_suffix));
                r = r2;
                let (r2, _) = ws(r)?;
                r = r2;
                continue;
            }
            let (r2, _) = ws(r2)?;
            r = r2;
            continue;
        }
        if let Some(r2) = keyword("does", r) {
            let (r2, _) = ws1(r2)?;
            let (r2, role_name) = qualified_ident(r2)?;
            let (r2, _) = ws(r2)?;
            let (r2, bracket_suffix) = parse_optional_bracket_suffix(r2)?;
            let full_name = format!("{}{}", role_name, bracket_suffix);
            parents.push(full_name.clone());
            does_parents.push(full_name);
            let (r2, _) = ws(r2)?;
            r = r2;
            continue;
        }
        if let Some(r2) = keyword("hides", r) {
            let (r2, _) = ws1(r2)?;
            let (r2, parent) = qualified_ident(r2)?;
            let (r2, _) = ws(r2)?;
            let (r2, bracket_suffix) = parse_optional_bracket_suffix(r2)?;
            let hidden_parent = format!("{}{}", parent, bracket_suffix);
            parents.push(hidden_parent.clone());
            hidden_parents.push(hidden_parent);
            let (r2, _) = ws(r2)?;
            r = r2;
            continue;
        }
        break;
    }

    let (rest, mut body) = block(r)?;
    reject_no_self_in_subs(&body)?;
    body.retain(|stmt| {
        if stmt_is_also_is_rw(stmt) {
            class_is_rw = true;
            false
        } else if let Some(parent_name) = stmt_also_is_parent(stmt) {
            parents.push(parent_name);
            false
        } else {
            true
        }
    });
    // Extract repr from `is repr(...)` or from declarator traits
    let repr = is_repr.or_else(|| {
        traits.iter().find_map(|(k, v)| {
            if k == "repr" {
                if let Value::Str(s) = v {
                    Some(s.to_string())
                } else {
                    None
                }
            } else {
                None
            }
        })
    });
    let class_stmt = Stmt::ClassDecl {
        name: Symbol::intern(&name),
        name_expr,
        parents,
        class_is_rw,
        is_hidden,
        hidden_parents,
        does_parents,
        repr,
        body,
    };
    let mut stmts = Vec::new();
    for (trait_name, trait_value) in traits {
        if trait_name == "ver" || trait_name == "auth" {
            stmts.push(meta_setter_stmt(&name, &trait_name, trait_value));
        }
    }
    if stmts.is_empty() {
        return Ok((rest, class_stmt));
    }
    stmts.push(class_stmt);
    Ok((rest, Stmt::Block(stmts)))
}

/// Parse `also is <trait>;` statement.
///
/// This is primarily used inside class bodies (e.g. `also is rw;`) and is
/// represented as a small infix expression statement for later lowering.
pub(super) fn also_trait_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("also", input).ok_or_else(|| PError::expected("also trait statement"))?;
    let (rest, _) = ws1(rest)?;
    // Handle `also does RoleName;`
    if let Some(r) = keyword("does", rest) {
        let (r, _) = ws1(r)?;
        let (r, name) = parse_token_like_name(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = opt_char(r, ';');
        return Ok((
            r,
            Stmt::DoesDecl {
                name: Symbol::intern(&name),
            },
        ));
    }
    let rest = keyword("is", rest).ok_or_else(|| PError::expected("is or does after also"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, trait_name) = ident(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, _) = opt_char(rest, ';');
    Ok((
        rest,
        Stmt::Expr(Expr::InfixFunc {
            name: "is".to_string(),
            left: Box::new(Expr::BareWord("also".to_string())),
            right: vec![Expr::BareWord(trait_name)],
            modifier: None,
        }),
    ))
}

/// Parse optional role type parameters like `[::T]`, `[Str $x]`, or
/// `[Int $x where { ... }]`.
/// Returns both full parameter defs and plain names used for substitution.
fn parse_optional_role_type_params(input: &str) -> PResult<'_, (Vec<String>, Vec<ParamDef>)> {
    let (r, _) = ws(input)?;
    if !r.starts_with('[') {
        return Ok((r, (Vec::new(), Vec::new())));
    }
    let mut depth = 0u32;
    let mut end = 0usize;
    for (i, ch) in r.char_indices() {
        if ch == '[' {
            depth += 1;
        } else if ch == ']' {
            depth = depth.saturating_sub(1);
            if depth == 0 {
                end = i;
                break;
            }
        }
    }
    if end == 0 {
        return Err(PError::expected("']'"));
    }
    let content = &r[1..end];
    for part in content.split(',') {
        let trimmed = part.trim();
        if let Some(stripped) = trimmed.strip_prefix("::")
            && let Ok((rest_after_ident, _)) = ident(stripped)
            && rest_after_ident.starts_with('?')
        {
            return Err(PError::fatal("X::Syntax::Malformed".to_string()));
        }
    }
    if let Ok((after_params, param_defs)) = parse_param_list(&r[1..])
        && let Ok((rest, _)) = parse_char(after_params, ']')
    {
        let params = param_defs
            .iter()
            .map(|pd| {
                if let Some(captured) = pd
                    .type_constraint
                    .as_deref()
                    .and_then(|t| t.strip_prefix("::"))
                {
                    captured.to_string()
                } else {
                    pd.name.trim_start_matches(['$', '@', '%', '&']).to_string()
                }
            })
            .collect::<Vec<_>>();
        let (rest, _) = ws(rest)?;
        return Ok((rest, (params, param_defs)));
    }

    // Fallback: keep permissive parsing for edge signatures that parse_param_list
    // does not yet support in role parameter lists.
    let mut params = Vec::new();
    for part in content.split(',') {
        let trimmed = part.trim();
        if let Some(stripped) = trimmed.strip_prefix("::") {
            let name_part = stripped.trim();
            if let Ok((_, name)) = ident(name_part) {
                params.push(name);
            }
            continue;
        }
        if let Some(stripped) = trimmed
            .strip_prefix('$')
            .or_else(|| trimmed.strip_prefix('@'))
            .or_else(|| trimmed.strip_prefix('%'))
            .or_else(|| trimmed.strip_prefix('&'))
        {
            if let Ok((_, name)) = ident(stripped.trim()) {
                params.push(name);
            }
            continue;
        }
        if let Some(pos) = trimmed.find("::") {
            let after = &trimmed[pos + 2..];
            if let Ok((_, name)) = ident(after.trim()) {
                params.push(name);
                continue;
            }
        }
        if let Some(pos) = trimmed.find('$') {
            let after = &trimmed[pos + 1..];
            if let Ok((_, name)) = ident(after.trim()) {
                params.push(name);
            }
        }
    }
    let (rest, _) = ws(&r[end + 1..])?;
    Ok((rest, (params, Vec::new())))
}

/// Skip optional role args like `[Str:D(Numeric)]` in a `does` clause.
fn skip_optional_role_args(input: &str) -> PResult<'_, ()> {
    let (mut r, _) = ws(input)?;
    if !r.starts_with('[') {
        return Ok((r, ()));
    }
    let mut depth = 0u32;
    while let Some(ch) = r.chars().next() {
        let len = ch.len_utf8();
        if ch == '[' {
            depth += 1;
        } else if ch == ']' {
            depth = depth.saturating_sub(1);
            if depth == 0 {
                r = &r[len..];
                break;
            }
        }
        r = &r[len..];
    }
    let (r, _) = ws(r)?;
    Ok((r, ()))
}

/// Parse `role` declaration.
pub(super) fn role_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("role", input).ok_or_else(|| PError::expected("role declaration"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, name) = qualified_ident(rest)?;
    let (mut rest, (type_params, type_param_defs)) = parse_optional_role_type_params(rest)?;
    let mut parent_roles: Vec<String> = Vec::new();
    let mut is_hidden_role = false;

    // Optional parent/trait clauses in any order.
    loop {
        if let Some(r) = keyword("does", rest) {
            let (r, _) = ws1(r)?;
            let (r, role_name) = qualified_ident(r)?;
            if role_name == name {
                return Err(PError::fatal(format!(
                    "X::InvalidType: role '{}' cannot compose itself",
                    name
                )));
            }
            let (r, _) = ws(r)?;
            let (r, bracket_suffix) = parse_optional_bracket_suffix(r)?;
            let (r, _) = ws(r)?;
            parent_roles.push(format!("{}{}", role_name, bracket_suffix));
            rest = r;
            continue;
        }
        if let Some(r) = keyword("is", rest) {
            let (r, _) = ws1(r)?;
            let (r, trait_name) = ident(r)?;
            let r = skip_balanced_parens(r);
            let (r, _) = ws(r)?;
            if trait_name == "hidden" {
                is_hidden_role = true;
            } else if !matches!(
                trait_name.as_str(),
                "rw" | "ok"
                    | "export"
                    | "required"
                    | "readonly"
                    | "repr"
                    | "default"
                    | "raw"
                    | "built"
                    | "copy"
                    | "DEPRECATED"
                    | "nodal"
                    | "pure"
            ) {
                // Known lowercase trait keywords are skipped;
                // everything else (including lowercase class/role names like irA)
                // is treated as a parent.
                parent_roles.push(trait_name);
            }
            rest = r;
            continue;
        }
        if let Some(r) = keyword("hides", rest) {
            let (r, _) = ws1(r)?;
            let (r, hidden_name) = qualified_ident(r)?;
            let (r, _) = ws(r)?;
            // Track as a parent relationship
            parent_roles.push(hidden_name.clone());
            // Also mark the hidden relationship with a special marker
            parent_roles.push(format!("__mutsu_role_hides__{}", hidden_name));
            rest = r;
            continue;
        }
        break;
    }

    let (rest, mut body) = match block(rest) {
        Ok(ok) => ok,
        Err(_) => consume_raw_braced_body(rest)?,
    };
    if is_hidden_role {
        body.insert(
            0,
            Stmt::DoesDecl {
                name: Symbol::intern("__mutsu_role_hidden__"),
            },
        );
    }
    for role_name in parent_roles.into_iter().rev() {
        body.insert(
            0,
            Stmt::DoesDecl {
                name: Symbol::intern(&role_name),
            },
        );
    }
    Ok((
        rest,
        Stmt::RoleDecl {
            name: Symbol::intern(&name),
            type_params,
            type_param_defs,
            body,
        },
    ))
}

/// Parse `does` declaration.
pub(super) fn does_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("does", input).ok_or_else(|| PError::expected("does declaration"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, name) = parse_token_like_name(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, _) = opt_char(rest, ';');
    Ok((
        rest,
        Stmt::DoesDecl {
            name: Symbol::intern(&name),
        },
    ))
}

/// Parse `trusts` declaration.
pub(super) fn trusts_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("trusts", input).ok_or_else(|| PError::expected("trusts declaration"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, name) = qualified_ident(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, _) = opt_char(rest, ';');
    Ok((
        rest,
        Stmt::TrustsDecl {
            name: Symbol::intern(&name),
        },
    ))
}

/// Parse a `token`, `regex`, or `rule` declaration.
pub(super) fn token_decl(input: &str) -> PResult<'_, Stmt> {
    let is_rule = keyword("rule", input).is_some();
    let is_regex = keyword("regex", input).is_some();
    let is_ratchet = !is_regex; // token and rule are ratcheting
    let rest = keyword("token", input)
        .or_else(|| keyword("rule", input))
        .or_else(|| keyword("regex", input))
        .ok_or_else(|| PError::expected("token/regex/rule declaration"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, name) = parse_token_like_name(rest)?;
    let (rest, _) = ws(rest)?;

    // Optional params
    let (rest, (params, param_defs)) = if rest.starts_with('(') {
        let (r, _) = parse_char(rest, '(')?;
        let (r, _) = ws(r)?;
        let (r, pd) = parse_param_list(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        let names: Vec<String> = pd.iter().map(|p| p.name.clone()).collect();
        (r, (names, pd))
    } else {
        (rest, (Vec::new(), Vec::new()))
    };

    let (rest, _) = ws(rest)?;
    let (rest, mut pattern) = parse_raw_braced_regex_body(rest)?;
    pattern = normalize_token_pattern(&pattern);
    if is_rule {
        pattern = inject_implicit_rule_ws(&pattern);
        if name.contains(":sym<") || name.contains(":sym\u{ab}") {
            if !pattern.ends_with(' ') {
                pattern.push(' ');
            }
            pattern.push_str("<.ws>?");
        }
    }
    if is_ratchet {
        pattern = format!(":ratchet {pattern}");
    }
    let body = vec![Stmt::Expr(Expr::Literal(Value::regex(pattern)))];

    if is_rule {
        Ok((
            rest,
            Stmt::RuleDecl {
                name: Symbol::intern(&name),
                params,
                param_defs,
                body,
                multi: false,
            },
        ))
    } else {
        Ok((
            rest,
            Stmt::TokenDecl {
                name: Symbol::intern(&name),
                params,
                param_defs,
                body,
                multi: false,
            },
        ))
    }
}

/// Parse `grammar` declaration.
pub(super) fn grammar_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("grammar", input).ok_or_else(|| PError::expected("grammar declaration"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, name) = qualified_ident(rest)?;
    let (rest, _) = ws(rest)?;
    let mut r = rest;
    let mut parents = Vec::new();
    while let Some(r2) = keyword("is", r) {
        let (r2, _) = ws1(r2)?;
        let (r2, parent_name) = qualified_ident(r2)?;
        parents.push(parent_name);
        let (r2, _) = ws(r2)?;
        r = r2;
    }
    let mut does_parents = Vec::new();
    while let Some(r2) = keyword("does", r) {
        let (r2, _) = ws1(r2)?;
        let (r2, role_name) = qualified_ident(r2)?;
        does_parents.push(role_name);
        let (r2, _) = ws(r2)?;
        let (r2, _) = skip_optional_role_args(r2)?;
        r = r2;
    }
    // Default parent is Grammar if no `is` clause (unless the grammar itself is named Grammar)
    if parents.is_empty() && name != "Grammar" {
        parents.push("Grammar".to_string());
    }
    let (rest, body) = block(r)?;
    Ok((
        rest,
        Stmt::ClassDecl {
            name: Symbol::intern(&name),
            name_expr: None,
            parents,
            class_is_rw: false,
            is_hidden: false,
            hidden_parents: vec![],
            does_parents,
            repr: None,
            body,
        },
    ))
}

/// Parse `module Name { ... }` declaration (non-unit form).
pub(super) fn module_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("module", input).ok_or_else(|| PError::expected("module declaration"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, name) = qualified_ident(rest)?;
    let (rest, traits) = parse_declarator_traits(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    let mut stmts = Vec::new();
    for (trait_name, trait_value) in traits {
        if trait_name == "ver" || trait_name == "auth" {
            stmts.push(meta_setter_stmt(&name, &trait_name, trait_value));
        }
    }
    let package_stmt = Stmt::Package {
        name: Symbol::intern(&name),
        body,
        is_unit: false,
    };
    if stmts.is_empty() {
        return Ok((rest, package_stmt));
    }
    stmts.push(package_stmt);
    Ok((rest, Stmt::SyntheticBlock(stmts)))
}

/// Parse `unit module` or `unit class` statement.
pub(super) fn unit_module_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("unit", input).ok_or_else(|| PError::expected("unit statement"))?;
    let (rest, _) = ws1(rest)?;
    // unit class Name;
    if let Some(r) = keyword("class", rest) {
        let (r, _) = ws1(r)?;
        let (r, name) = qualified_ident(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = opt_char(r, ';');
        return Ok((
            r,
            Stmt::ClassDecl {
                name: Symbol::intern(&name),
                name_expr: None,
                parents: Vec::new(),
                class_is_rw: false,
                is_hidden: false,
                hidden_parents: Vec::new(),
                does_parents: Vec::new(),
                repr: None,
                body: Vec::new(),
            },
        ));
    }
    let rest = keyword("module", rest).ok_or_else(|| PError::expected("'module' after 'unit'"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, name) = qualified_ident(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, _) = opt_char(rest, ';');
    Ok((
        rest,
        Stmt::Package {
            name: Symbol::intern(&name),
            body: Vec::new(),
            is_unit: true,
        },
    ))
}

/// Parse `package` declaration.
pub(super) fn package_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("package", input).ok_or_else(|| PError::expected("package declaration"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, name) = qualified_ident(rest)?;
    let (rest, _traits) = parse_declarator_traits(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    Ok((
        rest,
        Stmt::Package {
            name: Symbol::intern(&name),
            body,
            is_unit: false,
        },
    ))
}

/// Parse `proto` declaration.
pub(super) fn proto_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("proto", input).ok_or_else(|| PError::expected("proto declaration"))?;
    let (rest, _) = ws1(rest)?;
    // proto token | proto rule | proto sub | proto method
    let _is_token = keyword("token", rest).is_some() || keyword("rule", rest).is_some();
    let rest = if let Some(r) = keyword("token", rest)
        .or_else(|| keyword("rule", rest))
        .or_else(|| keyword("sub", rest))
        .or_else(|| keyword("method", rest))
    {
        let (r, _) = ws1(r)?;
        r
    } else {
        rest
    };
    let (rest, name) = parse_sub_name(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, param_defs) = if rest.starts_with('(') {
        let (r, _) = parse_char(rest, '(')?;
        let (r, _) = ws(r)?;
        let (r, pd) = parse_param_list(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        (r, pd)
    } else {
        (rest, Vec::new())
    };
    let params: Vec<String> = param_defs.iter().map(|p| p.name.clone()).collect();
    let (rest, _) = ws(rest)?;
    // Parse traits (is export, etc.)
    let (rest, traits) = parse_sub_traits(rest)?;
    let (rest, _) = ws(rest)?;
    // May have body or just semicolon
    let mut body = Vec::new();
    if rest.starts_with('{') {
        let (rest, parsed_body) = match block(rest) {
            Ok(ok) => ok,
            Err(_) => consume_raw_braced_body(rest)?,
        };
        body = parsed_body;
        return Ok((
            rest,
            Stmt::ProtoDecl {
                name: Symbol::intern(&name),
                params,
                param_defs,
                body,
                is_export: traits.is_export,
                custom_traits: traits.custom_traits.clone(),
            },
        ));
    }
    let (rest, _) = opt_char(rest, ';');
    Ok((
        rest,
        Stmt::ProtoDecl {
            name: Symbol::intern(&name),
            params,
            param_defs,
            body,
            is_export: traits.is_export,
            custom_traits: traits.custom_traits,
        },
    ))
}
