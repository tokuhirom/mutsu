use super::super::expr::expression;
use super::super::helpers::{skip_balanced_parens, ws, ws1};
use super::super::parse_result::{
    PError, PResult, merge_expected_messages, opt_char, parse_char, take_while1,
};

use crate::ast::{AssignOp, Expr, Stmt};
use crate::token_kind::TokenKind;
use crate::value::Value;

use super::sub::parse_type_constraint_expr;
use super::{
    class::{module_decl, package_decl, proto_decl, role_decl},
    ident, keyword, parse_assign_expr_or_comma, parse_statement_modifier, qualified_ident,
    var_name,
};

use super::super::parse_result::take_while_opt;
use super::{class_decl_body, method_decl_body, parse_comma_or_expr, sub_decl_body};

fn strip_type_smiley_suffix(type_name: &str) -> &str {
    type_name
        .strip_suffix(":U")
        .or_else(|| type_name.strip_suffix(":D"))
        .or_else(|| type_name.strip_suffix(":_"))
        .unwrap_or(type_name)
}

fn typed_default_expr(type_name: &str) -> Expr {
    if strip_type_smiley_suffix(type_name) == "Mu" {
        Expr::BareWord("Mu".to_string())
    } else {
        Expr::Literal(Value::Nil)
    }
}

fn default_decl_expr(
    is_array: bool,
    is_hash: bool,
    shape_dims: Option<&[Expr]>,
    type_constraint: Option<&str>,
) -> Expr {
    if is_array {
        if let Some(dims) = shape_dims {
            shaped_array_new_expr(dims.to_vec())
        } else {
            Expr::Literal(Value::real_array(Vec::new()))
        }
    } else if is_hash {
        Expr::Hash(Vec::new())
    } else if let Some(tc) = type_constraint {
        typed_default_expr(tc)
    } else {
        Expr::Literal(Value::Nil)
    }
}

fn is_decl_trailing_or_chain_op(op: &TokenKind) -> bool {
    matches!(op, TokenKind::OrWord | TokenKind::OrElse)
}

fn rewrite_decl_assignment_or_chain(expr: Expr, mut decl_stmt: Stmt) -> Option<Stmt> {
    let mut init = expr;
    let mut tails: Vec<(TokenKind, Expr)> = Vec::new();
    while let Expr::Binary { left, op, right } = init {
        if !is_decl_trailing_or_chain_op(&op) {
            init = Expr::Binary { left, op, right };
            break;
        }
        tails.push((op, *right));
        init = *left;
    }
    if tails.is_empty() {
        return None;
    }

    if let Stmt::VarDecl { expr, .. } = &mut decl_stmt {
        *expr = init;
    } else {
        return None;
    }

    let mut chain = Expr::DoStmt(Box::new(decl_stmt));
    for (op, right) in tails.into_iter().rev() {
        chain = Expr::Binary {
            left: Box::new(chain),
            op,
            right: Box::new(right),
        };
    }
    Some(Stmt::Expr(chain))
}

fn is_supported_variable_trait(trait_name: &str) -> bool {
    if matches!(trait_name, "default" | "export" | "dynamic") {
        return true;
    }
    // Type-ish variable traits are accepted in roast (e.g. `is List`, `is Map`).
    trait_name
        .chars()
        .next()
        .is_some_and(|c| c.is_ascii_uppercase())
}

fn parse_export_trait_tags(input: &str) -> PResult<'_, Vec<String>> {
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

fn parse_sigilless_decl_name(input: &str) -> PResult<'_, String> {
    super::parse_sub_name_pub(input)
}

pub(super) fn parse_array_shape_suffix(input: &str) -> PResult<'_, Vec<Expr>> {
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

fn shaped_array_new_expr(dims: Vec<Expr>) -> Expr {
    let shape_value = if dims.len() == 1 {
        dims.into_iter()
            .next()
            .unwrap_or(Expr::Literal(Value::Int(0)))
    } else {
        Expr::ArrayLiteral(dims)
    };

    Expr::MethodCall {
        target: Box::new(Expr::BareWord("Array".to_string())),
        name: "new".to_string(),
        args: vec![Expr::Binary {
            left: Box::new(Expr::Literal(Value::Str("shape".to_string()))),
            op: TokenKind::FatArrow,
            right: Box::new(shape_value),
        }],
        modifier: None,
        quoted: false,
    }
}

/// Generate `Array.new(:shape(N), :data(expr))` for shaped array declarations
/// with an assignment expression (e.g. `my @b[3] = <a b c>`).
fn shaped_array_new_with_data_expr(dims: Vec<Expr>, data: Expr) -> Expr {
    let shape_value = if dims.len() == 1 {
        dims.into_iter()
            .next()
            .unwrap_or(Expr::Literal(Value::Int(0)))
    } else {
        Expr::ArrayLiteral(dims)
    };

    Expr::MethodCall {
        target: Box::new(Expr::BareWord("Array".to_string())),
        name: "new".to_string(),
        args: vec![
            Expr::Binary {
                left: Box::new(Expr::Literal(Value::Str("shape".to_string()))),
                op: TokenKind::FatArrow,
                right: Box::new(shape_value),
            },
            Expr::Binary {
                left: Box::new(Expr::Literal(Value::Str("data".to_string()))),
                op: TokenKind::FatArrow,
                right: Box::new(data),
            },
        ],
        modifier: None,
        quoted: false,
    }
}

fn register_term_symbol_from_decl_name(name: &str) {
    if let Some(callable_name) = name.strip_prefix('&') {
        super::simple::register_user_callable_term_symbol(callable_name);
    } else {
        super::simple::register_user_term_symbol(name);
    }
}

/// Parse a `use` statement.
pub(super) fn use_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("use", input).ok_or_else(|| PError::expected("use statement"))?;
    let (rest, _) = ws1(rest)?;

    // Handle `use v6`, `use v6.d`, etc.
    if rest.starts_with('v') {
        let (r, _) = take_while1(rest, |c: char| {
            c.is_alphanumeric() || c == '.' || c == '*' || c == '+'
        })?;
        let (r, _) = ws(r)?;
        let _ = opt_char(r, ';');
        let (r, _) = opt_char(r, ';');
        return Ok((
            r,
            Stmt::Use {
                module: "v6".to_string(),
                arg: None,
            },
        ));
    }

    let (rest, module) = qualified_ident(rest)?;
    let (rest, _) = ws(rest)?;

    // `use newline :lf|:cr|:crlf` uses a colonpair argument and must be preserved.
    if module == "newline" && rest.starts_with(':') && !rest.starts_with("::") {
        let (rest, arg) = super::super::primary::colonpair_expr(rest)?;
        let (rest, _) = ws(rest)?;
        let (rest, _) = opt_char(rest, ';');
        return Ok((
            rest,
            Stmt::Use {
                module,
                arg: Some(arg),
            },
        ));
    }

    // Skip adverbs/colonpairs on use (e.g. `use Foo :ALL`, `use Foo :tag1 :tag2`)
    let mut rest = rest;
    while rest.starts_with(':') && !rest.starts_with("::") {
        let r = &rest[1..];
        // :!name
        let r = r.strip_prefix('!').unwrap_or(r);
        if let Ok((r, _name)) = ident(r) {
            // :name(expr)
            let r = skip_balanced_parens(r);
            let (r, _) = ws(r)?;
            rest = r;
        } else {
            break;
        }
    }

    // Optional argument
    let (rest, arg) = if rest.starts_with(';') || rest.is_empty() || rest.starts_with('}') {
        (rest, None)
    } else if rest.starts_with('<') {
        // Angle-bracket import list
        let (r, expr) = super::super::primary::primary(rest)?;
        (r, Some(expr))
    } else {
        let (r, expr) = expression(rest)?;
        (r, Some(expr))
    };
    let (rest, _) = ws(rest)?;
    let (rest, _) = opt_char(rest, ';');
    // Handle `use lib "path"` or `use lib $*PROGRAM.parent(N).add("path")` at parse time
    if module == "lib"
        && let Some(ref expr) = arg
    {
        super::simple::try_add_parse_time_lib_path(expr);
    }
    // Register exported function names so they are recognized as calls without parens.
    super::simple::register_module_exports(&module);
    Ok((rest, Stmt::Use { module, arg }))
}

/// Parse `import Module [:TAG ...];`
pub(super) fn import_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("import", input).ok_or_else(|| PError::expected("import statement"))?;
    let (rest, _) = ws1(rest)?;
    let (mut rest, module) = qualified_ident(rest)?;
    let mut tags = Vec::new();
    loop {
        let (r, _) = ws(rest)?;
        let mut r = r;
        if let Some(after_comma) = r.strip_prefix(',') {
            let (r2, _) = ws(after_comma)?;
            r = r2;
        }
        if !r.starts_with(':') || r.starts_with("::") {
            rest = r;
            break;
        }
        let r = &r[1..];
        let (r, name) = ident(r)?;
        if !tags.iter().any(|t| t == &name) {
            tags.push(name);
        }
        rest = r;
    }
    let (rest, _) = ws(rest)?;
    let (rest, _) = opt_char(rest, ';');
    Ok((rest, Stmt::Import { module, tags }))
}

/// Parse `need Module;` — load module without importing its exports.
pub(super) fn need_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("need", input).ok_or_else(|| PError::expected("need statement"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, module) = qualified_ident(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, _) = opt_char(rest, ';');
    Ok((rest, Stmt::Need { module }))
}

/// Parse a `no` statement.
pub(super) fn no_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("no", input).ok_or_else(|| PError::expected("no statement"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, module) = qualified_ident(rest)?;
    let (rest, _) = ws(rest)?;

    let (rest, arg) = if rest.starts_with(';') || rest.is_empty() || rest.starts_with('}') {
        (rest, None)
    } else if rest.starts_with('<') {
        let (r, expr) = super::super::primary::primary(rest)?;
        (r, Some(expr))
    } else {
        let (r, expr) = expression(rest)?;
        (r, Some(expr))
    };

    let (rest, _) = ws(rest)?;
    let (rest, _) = opt_char(rest, ';');
    let _ = arg;
    Ok((rest, Stmt::No { module }))
}

/// Parse `my`, `our`, or `state` variable declaration.
pub(super) fn my_decl(input: &str) -> PResult<'_, Stmt> {
    my_decl_inner(input, true)
}

/// Parse a `my`/`our`/`state` declaration in expression context (no statement modifier).
/// This avoids consuming semicolons via `parse_statement_modifier` when called from
/// within an expression (e.g., colon-arg: `@a.push: my \p = expr;`).
pub(super) fn my_decl_expr(input: &str) -> PResult<'_, Stmt> {
    my_decl_inner(input, false)
}

fn my_decl_inner(input: &str, apply_modifier: bool) -> PResult<'_, Stmt> {
    let is_state = keyword("state", input).is_some();
    let is_our = keyword("our", input).is_some();
    let rest = keyword("my", input)
        .or_else(|| keyword("our", input))
        .or_else(|| keyword("state", input))
        .ok_or_else(|| PError::expected("my/our/state declaration"))?;
    let (rest, _) = ws1(rest)?;

    // my enum Foo <...>
    if let Some(r) = keyword("enum", rest) {
        let (r, _) = ws1(r)?;
        return parse_enum_decl_body(r);
    }

    // my/our proto ...
    if keyword("proto", rest).is_some() {
        return proto_decl(rest);
    }

    // Typed routine declarations, e.g. `my Bool sub f(...) { ... }`.
    if let Some((after_type, routine_type)) = parse_type_constraint_expr(rest)
        && routine_type
            .chars()
            .next()
            .is_some_and(|c| c.is_ascii_uppercase())
    {
        let (after_type, _) = ws(after_type)?;
        if let Some(r) = keyword("multi", after_type) {
            let (r, _) = ws1(r)?;
            let r = keyword("sub", r)
                .map(|r2| ws(r2).map(|(r3, _)| r3).unwrap_or(r2))
                .unwrap_or(r);
            let (r, mut stmt) = sub_decl_body(r, true, false, false)?;
            if let Stmt::SubDecl { return_type, .. } = &mut stmt
                && return_type.is_none()
            {
                *return_type = Some(routine_type.clone());
            }
            return Ok((r, stmt));
        }
        if let Some(r) = keyword("sub", after_type) {
            let (r, _) = ws1(r)?;
            let (r, mut stmt) = sub_decl_body(r, false, false, false)?;
            if let Stmt::SubDecl { return_type, .. } = &mut stmt
                && return_type.is_none()
            {
                *return_type = Some(routine_type.clone());
            }
            return Ok((r, stmt));
        }
    }

    // my multi [sub] name(...) { ... }
    if let Some(r) = keyword("multi", rest) {
        let (r, _) = ws1(r)?;
        // "sub" is optional after "multi"
        let r = keyword("sub", r)
            .map(|r2| ws(r2).map(|(r3, _)| r3).unwrap_or(r2))
            .unwrap_or(r);
        return sub_decl_body(r, true, false, false);
    }

    // my sub name(...) { ... }
    if let Some(r) = keyword("sub", rest) {
        let (r, _) = ws1(r)?;
        return sub_decl_body(r, false, false, false);
    }

    // my/our method name(...) { ... }
    if let Some(r) = keyword("method", rest) {
        let (r, _) = ws1(r)?;
        return method_decl_body(r, false, is_our);
    }

    // my/our submethod name(...) { ... }
    if let Some(r) = keyword("submethod", rest) {
        let (r, _) = ws1(r)?;
        return method_decl_body(r, false, is_our);
    }

    // my class Name is Parent { ... }
    if let Some(r) = keyword("class", rest) {
        let (r, _) = ws1(r)?;
        return class_decl_body(r);
    }
    // my grammar Name { ... }
    if keyword("grammar", rest).is_some() {
        return super::class::grammar_decl(rest);
    }
    // my role Name[...] { ... }
    if keyword("role", rest).is_some() {
        return role_decl(rest);
    }
    // my module Name { ... }
    if keyword("module", rest).is_some() {
        return module_decl(rest);
    }
    // my package Name { ... }
    if keyword("package", rest).is_some() {
        return package_decl(rest);
    }
    // my subset Name of BaseType where ...
    if let Some(r) = keyword("subset", rest) {
        let (r, _) = ws1(r)?;
        return subset_decl(r);
    }
    // my constant $x = ...
    if keyword("constant", rest).is_some() {
        let (r, stmt) = constant_decl(rest)?;
        if apply_modifier {
            return parse_statement_modifier(r, stmt);
        }
        return Ok((r, stmt));
    }
    // my regex/token/rule Name { ... }
    // Reuse token/regex/rule declaration parsing so `<Name>` works in regexes.
    if keyword("regex", rest).is_some()
        || keyword("token", rest).is_some()
        || keyword("rule", rest).is_some()
    {
        let (rest, stmt) = super::class::token_decl(rest)?;
        if apply_modifier {
            return parse_statement_modifier(rest, stmt);
        }
        return Ok((rest, stmt));
    }

    // Sigilless variable: my \name = expr / my \name := expr / my \name ::= expr
    if let Some(r) = rest.strip_prefix('\\') {
        let (r, name) = parse_sigilless_decl_name(r)?;
        register_term_symbol_from_decl_name(&name);
        let (r, _) = ws(r)?;
        if let Some(r) = r.strip_prefix("::=").or_else(|| r.strip_prefix(":=")) {
            let (r, _) = ws(r)?;
            let (r, expr) = parse_assign_expr_or_comma(r)?;
            let stmt = Stmt::VarDecl {
                name,
                expr,
                type_constraint: None,
                is_state,
                is_our,
                is_dynamic: false,
                is_export: false,
                export_tags: Vec::new(),
                custom_traits: Vec::new(),
            };
            if apply_modifier {
                return parse_statement_modifier(r, stmt);
            }
            return Ok((r, stmt));
        }
        if r.starts_with('=') && !r.starts_with("==") && !r.starts_with("=>") {
            let r = &r[1..];
            let (r, _) = ws(r)?;
            let (r, expr) = parse_assign_expr_or_comma(r)?;
            let stmt = Stmt::VarDecl {
                name,
                expr,
                type_constraint: None,
                is_state,
                is_our,
                is_dynamic: false,
                is_export: false,
                export_tags: Vec::new(),
                custom_traits: Vec::new(),
            };
            if apply_modifier {
                return parse_statement_modifier(r, stmt);
            }
            return Ok((r, stmt));
        }
        return Ok((
            r,
            Stmt::VarDecl {
                name,
                expr: Expr::Literal(Value::Nil),
                type_constraint: None,
                is_state,
                is_our,
                is_dynamic: false,
                is_export: false,
                export_tags: Vec::new(),
                custom_traits: Vec::new(),
            },
        ));
    }

    // Optional type constraint: my Int $x or my Str(Match) $x (coercion type)
    let (rest, mut type_constraint) = {
        // Try to parse a type name followed by a sigil or \
        let saved = rest;
        if let Some((r, tc)) = parse_type_constraint_expr(rest) {
            let (r2, _) = ws(r)?;
            if r2.starts_with('$')
                || r2.starts_with('@')
                || r2.starts_with('%')
                || r2.starts_with('&')
                || r2.starts_with('\\')
            {
                (r2, Some(tc))
            } else {
                (saved, None)
            }
        } else {
            (saved, None)
        }
    };

    // Sigilless variable after type: my Int \name = expr / my Int \name := expr / ::=
    if let Some(r) = rest.strip_prefix('\\') {
        let (r, name) = parse_sigilless_decl_name(r)?;
        register_term_symbol_from_decl_name(&name);
        let (r, _) = ws(r)?;
        if let Some(r) = r.strip_prefix("::=").or_else(|| r.strip_prefix(":=")) {
            let (r, _) = ws(r)?;
            let (r, expr) = parse_assign_expr_or_comma(r)?;
            let stmt = Stmt::VarDecl {
                name,
                expr,
                type_constraint,
                is_state,
                is_our,
                is_dynamic: false,
                is_export: false,
                export_tags: Vec::new(),
                custom_traits: Vec::new(),
            };
            if apply_modifier {
                return parse_statement_modifier(r, stmt);
            }
            return Ok((r, stmt));
        }
        if r.starts_with('=') && !r.starts_with("==") && !r.starts_with("=>") {
            let r = &r[1..];
            let (r, _) = ws(r)?;
            let (r, expr) = parse_assign_expr_or_comma(r)?;
            let stmt = Stmt::VarDecl {
                name,
                expr,
                type_constraint,
                is_state,
                is_our,
                is_dynamic: false,
                is_export: false,
                export_tags: Vec::new(),
                custom_traits: Vec::new(),
            };
            if apply_modifier {
                return parse_statement_modifier(r, stmt);
            }
            return Ok((r, stmt));
        }
        return Ok((
            r,
            Stmt::VarDecl {
                name,
                expr: type_constraint
                    .as_deref()
                    .map_or(Expr::Literal(Value::Nil), typed_default_expr),
                type_constraint,
                is_state,
                is_our,
                is_dynamic: false,
                is_export: false,
                export_tags: Vec::new(),
                custom_traits: Vec::new(),
            },
        ));
    }

    // Destructuring: my ($a, $b) = expr
    if rest.starts_with('(') {
        return parse_destructuring_decl(rest);
    }

    // Parse variable
    let sigil = rest.as_bytes().first().copied().unwrap_or(0);
    let is_array = sigil == b'@';
    let is_hash = sigil == b'%';
    let is_code = sigil == b'&';

    let prefix = if is_array {
        "@"
    } else if is_hash {
        "%"
    } else if is_code {
        "&"
    } else {
        ""
    };

    let (rest, name) = if sigil == b'$' || is_array || is_hash || is_code {
        let (r, n) = var_name(rest)?;
        (r, format!("{}{}", prefix, n))
    } else {
        // Sigilless variable
        let (r, n) = ident(rest)?;
        (r, n)
    };
    register_term_symbol_from_decl_name(&name);

    let (rest, _) = ws(rest)?;

    // Parse hash key-type parameterization: %h{Str}, %h{Int}, ...
    let (rest, hash_key_constraint) = if is_hash
        && rest.starts_with('{')
        && !rest.starts_with("{{")
        && let Some(end) = rest.find('}')
    {
        let key_type = rest[1..end].trim().to_string();
        let after = &rest[end + 1..];
        let (after, _) = ws(after)?;
        (after, Some(key_type))
    } else {
        (rest, None)
    };
    let mut hash_key_constraint = hash_key_constraint;

    // Optional shaped-array declaration suffix: my @arr[2;2];
    let (rest, shape_dims) = if is_array && rest.starts_with('[') {
        let (rest, dims) = parse_array_shape_suffix(rest)?;
        let (rest, _) = ws(rest)?;
        (rest, Some(dims))
    } else {
        (rest, None)
    };

    // Parse variable traits. Builtins are handled directly; unknown/custom
    // traits are recorded for trait_mod:<is> dispatch at runtime.
    let mut has_dynamic_trait = false;
    let mut has_export_trait = false;
    let mut export_tags: Vec<String> = Vec::new();
    let mut custom_traits: Vec<(String, Option<Expr>)> = Vec::new();
    let mut rest = {
        let mut r = rest;
        while let Some(after_is) = keyword("is", r) {
            let (r2, _) = ws1(after_is)?;
            // Parse trait name
            let (r2, trait_name) = ident(r2)?;
            if trait_name == "readonly" {
                return Err(PError::fatal(
                    "X::Comp::Trait::Unknown: Unknown variable trait 'is readonly'".to_string(),
                ));
            }
            let is_builtin = is_supported_variable_trait(&trait_name);
            if trait_name == "dynamic" {
                has_dynamic_trait = true;
            }
            if trait_name == "export" {
                if !is_our {
                    return Err(PError::fatal(
                        "X::Comp::Trait::Scope: Trait 'is export' not allowed on my-scoped variable"
                            .to_string(),
                    ));
                }
                has_export_trait = true;
                let (r3, tags) = parse_export_trait_tags(r2)?;
                if tags.is_empty() {
                    if !export_tags.iter().any(|t| t == "DEFAULT") {
                        export_tags.push("DEFAULT".to_string());
                    }
                } else {
                    for tag in tags {
                        if !export_tags.iter().any(|t| t == &tag) {
                            export_tags.push(tag);
                        }
                    }
                }
                r = r3;
                continue;
            }
            let (r2, _) = ws(r2)?;
            // Parse optional trait argument: (expr)
            if let Some(r3) = r2.strip_prefix('(') {
                let (r3, _) = ws(r3)?;
                let (r3, trait_arg) = expression(r3)?;
                let (r3, _) = ws(r3)?;
                let (r3, _) = parse_char(r3, ')')?;
                let (r3, _) = ws(r3)?;
                if !is_builtin {
                    custom_traits.push((trait_name.clone(), Some(trait_arg)));
                }
                r = r3;
            } else {
                if !is_builtin {
                    custom_traits.push((trait_name.clone(), None));
                }
                r = r2;
            }
        }
        r
    };

    // Postfix container typing: my @a of Int; my %h of Int;
    if let Some(after_of) = keyword("of", rest) {
        let (r, _) = ws1(after_of)?;
        let (r, tc) = parse_type_constraint_expr(r).ok_or_else(|| PError::expected("type"))?;
        let (r, _) = ws(r)?;
        type_constraint = Some(tc);
        rest = r;
    }
    if is_hash {
        type_constraint = match (type_constraint, hash_key_constraint.take()) {
            (Some(value_tc), Some(key_tc)) => Some(format!("{}{{{}}}", value_tc, key_tc)),
            (None, Some(key_tc)) => Some(format!("Any{{{}}}", key_tc)),
            (value_tc, None) => value_tc,
        };
    }

    // Feed initialization: my @a <== expr / my @a <<== expr
    if let Some(rest) = rest
        .strip_prefix("<==")
        .or_else(|| rest.strip_prefix("<<=="))
    {
        let (rest, _) = ws(rest)?;
        let (rest, mut expr) = parse_assign_expr_or_comma(rest)?;
        if is_array {
            expr = Expr::Call {
                name: "__mutsu_feed_array_assign".to_string(),
                args: vec![expr],
            };
        }
        let expr = if let Some(dims) = shape_dims {
            shaped_array_new_with_data_expr(dims, expr)
        } else {
            expr
        };
        let stmt = Stmt::VarDecl {
            name,
            expr,
            type_constraint,
            is_state,
            is_our,
            is_dynamic: has_dynamic_trait,
            is_export: has_export_trait,
            export_tags: export_tags.clone(),
            custom_traits: custom_traits.clone(),
        };
        if apply_modifier {
            return parse_statement_modifier(rest, stmt);
        }
        return Ok((rest, stmt));
    }

    // Assignment
    if let Some((stripped, op)) = super::assign::parse_compound_assign_op(rest) {
        let (rest, _) = ws(stripped)?;
        let (rest, rhs) = parse_assign_expr_or_comma(rest).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected right-hand expression after compound assignment",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(rest.len())),
            exception: None,
        })?;
        let decl_stmt = Stmt::VarDecl {
            name: name.clone(),
            expr: default_decl_expr(
                is_array,
                is_hash,
                shape_dims.as_deref(),
                type_constraint.as_deref(),
            ),
            type_constraint: type_constraint.clone(),
            is_state,
            is_our,
            is_dynamic: has_dynamic_trait,
            is_export: has_export_trait,
            export_tags: export_tags.clone(),
            custom_traits: custom_traits.clone(),
        };
        let assign_stmt = Stmt::Assign {
            name: name.clone(),
            expr: super::assign::compound_assigned_value_expr(Expr::Var(name.clone()), op, rhs),
            op: AssignOp::Assign,
        };
        let stmt = Stmt::SyntheticBlock(vec![decl_stmt, assign_stmt]);
        if apply_modifier {
            return parse_statement_modifier(rest, stmt);
        }
        return Ok((rest, stmt));
    }
    if let Some((stripped, op_name)) = super::assign::parse_custom_compound_assign_op(rest) {
        let (rest, _) = ws(stripped)?;
        let (rest, rhs) = parse_assign_expr_or_comma(rest).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected right-hand expression after compound assignment",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(rest.len())),
            exception: None,
        })?;
        let decl_stmt = Stmt::VarDecl {
            name: name.clone(),
            expr: default_decl_expr(
                is_array,
                is_hash,
                shape_dims.as_deref(),
                type_constraint.as_deref(),
            ),
            type_constraint: type_constraint.clone(),
            is_state,
            is_our,
            is_dynamic: has_dynamic_trait,
            is_export: has_export_trait,
            export_tags: export_tags.clone(),
            custom_traits: custom_traits.clone(),
        };
        let assign_stmt = Stmt::Assign {
            name: name.clone(),
            expr: Expr::InfixFunc {
                name: op_name,
                left: Box::new(Expr::Var(name.clone())),
                right: vec![rhs],
                modifier: None,
            },
            op: AssignOp::Assign,
        };
        let stmt = Stmt::SyntheticBlock(vec![decl_stmt, assign_stmt]);
        if apply_modifier {
            return parse_statement_modifier(rest, stmt);
        }
        return Ok((rest, stmt));
    }

    // Assignment
    if rest.starts_with('=') && !rest.starts_with("==") && !rest.starts_with("=>") {
        let rest = &rest[1..];
        let (rest, _) = ws(rest)?;
        // class-scope routine aliasing form:
        //   our &name = method name(...) { ... }
        // This should register as a real method declaration (with is_our),
        // not as a plain code-variable assignment.
        if is_our && is_code {
            if let Some(r) = keyword("method", rest) {
                let (r, _) = ws1(r)?;
                let (r, stmt) = method_decl_body(r, false, true)?;
                if apply_modifier {
                    return parse_statement_modifier(r, stmt);
                }
                return Ok((r, stmt));
            }
            if let Some(r) = keyword("multi", rest) {
                let (r, _) = ws1(r)?;
                if let Some(r) = keyword("method", r) {
                    let (r, _) = ws1(r)?;
                    let (r, stmt) = method_decl_body(r, true, true)?;
                    if apply_modifier {
                        return parse_statement_modifier(r, stmt);
                    }
                    return Ok((r, stmt));
                }
            }
        }
        let (rest, expr) = parse_assign_expr_or_comma(rest)?;
        // For shaped array declarations with assignment (e.g. my @b[3] = <a b c>),
        // create a shaped array and populate it with the assigned data.
        let expr = if let Some(dims) = shape_dims {
            shaped_array_new_with_data_expr(dims, expr)
        } else {
            expr
        };
        let base_stmt = Stmt::VarDecl {
            name: name.clone(),
            expr: expr.clone(),
            type_constraint: type_constraint.clone(),
            is_state,
            is_our,
            is_dynamic: has_dynamic_trait,
            is_export: has_export_trait,
            export_tags: export_tags.clone(),
            custom_traits: custom_traits.clone(),
        };
        if let Some(stmt) = rewrite_decl_assignment_or_chain(expr.clone(), base_stmt) {
            if apply_modifier {
                return parse_statement_modifier(rest, stmt);
            }
            return Ok((rest, stmt));
        }
        let stmt = Stmt::VarDecl {
            name,
            expr,
            type_constraint,
            is_state,
            is_our,
            is_dynamic: has_dynamic_trait,
            is_export: has_export_trait,
            export_tags: export_tags.clone(),
            custom_traits: custom_traits.clone(),
        };
        if apply_modifier {
            return parse_statement_modifier(rest, stmt);
        }
        return Ok((rest, stmt));
    }
    // Method-call-assign .= in declaration: my Type $var .= method(args)
    if let Some(stripped) = rest.strip_prefix(".=") {
        let (rest, _) = ws(stripped)?;
        // Parse method name
        let (rest, method_name) =
            take_while1(rest, |c: char| c.is_alphanumeric() || c == '_' || c == '-')?;
        let method_name = method_name.to_string();
        // Parse optional args
        let (rest, args) = if rest.starts_with('(') {
            let (r, _) = parse_char(rest, '(')?;
            let (r, _) = ws(r)?;
            let (r, args) = super::super::primary::parse_call_arg_list(r)?;
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, ')')?;
            (r, args)
        } else {
            (rest, Vec::new())
        };
        // Build: Type.method(args) — use the type constraint as the target
        let target_name = type_constraint.clone().unwrap_or_else(|| name.clone());
        let expr = Expr::MethodCall {
            target: Box::new(Expr::BareWord(target_name)),
            name: method_name,
            args,
            modifier: None,
            quoted: false,
        };
        let stmt = Stmt::VarDecl {
            name,
            expr,
            type_constraint,
            is_state,
            is_our,
            is_dynamic: has_dynamic_trait,
            is_export: has_export_trait,
            export_tags: export_tags.clone(),
            custom_traits: custom_traits.clone(),
        };
        if apply_modifier {
            return parse_statement_modifier(rest, stmt);
        }
        return Ok((rest, stmt));
    }
    // Binding := or ::=
    if let Some(stripped) = rest.strip_prefix("::=").or_else(|| rest.strip_prefix(":=")) {
        let (rest, _) = ws(stripped)?;
        let (mut rest, mut expr) = parse_assign_expr_or_comma(rest)?;
        let (tail, _) = ws(rest)?;
        if let Expr::BareWord(name) = &expr
            && !tail.is_empty()
            && !tail.starts_with(';')
            && !tail.starts_with('}')
            && let Ok((r_after, arg_expr)) = parse_assign_expr_or_comma(tail)
        {
            expr = Expr::Call {
                name: name.clone(),
                args: vec![arg_expr],
            };
            rest = r_after;
        }
        let stmt = Stmt::VarDecl {
            name,
            expr,
            type_constraint,
            is_state,
            is_our,
            is_dynamic: has_dynamic_trait,
            is_export: has_export_trait,
            export_tags: export_tags.clone(),
            custom_traits: custom_traits.clone(),
        };
        if apply_modifier {
            return parse_statement_modifier(rest, stmt);
        }
        return Ok((rest, stmt));
    }

    let (rest, _) = opt_char(rest, ';');
    let expr = default_decl_expr(
        is_array,
        is_hash,
        shape_dims.as_deref(),
        type_constraint.as_deref(),
    );
    Ok((
        rest,
        Stmt::VarDecl {
            name,
            expr,
            type_constraint,
            is_state,
            is_our,
            is_dynamic: has_dynamic_trait,
            is_export: has_export_trait,
            export_tags,
            custom_traits,
        },
    ))
}

/// Parse destructuring: ($a, $b, $c) = expr
/// Metadata for each variable in a destructuring declaration.
struct DestructureVar {
    /// Full variable name including sigil prefix for @/% (e.g. "@y", "x", "%h")
    name: String,
    /// Whether this is a slurpy parameter (*@rest)
    is_slurpy: bool,
    /// Whether this is an optional parameter ($x?)
    #[allow(dead_code)]
    is_optional: bool,
    /// Whether this is a named parameter (:@even)
    is_named: bool,
}

pub(super) fn parse_destructuring_decl(input: &str) -> PResult<'_, Stmt> {
    let (rest, _) = parse_char(input, '(')?;
    let (rest, _) = ws(rest)?;
    let mut vars: Vec<DestructureVar> = Vec::new();
    let mut r = rest;
    loop {
        if r.starts_with(')') {
            break;
        }

        let mut is_slurpy = false;
        let mut is_named = false;

        // Check for slurpy prefix '*'
        if let Some(after) = r.strip_prefix('*') {
            is_slurpy = true;
            r = after;
        }

        // Check for named prefix ':'
        if let Some(after) = r.strip_prefix(':') {
            is_named = true;
            r = after;
        }

        let sigil = r.as_bytes().first().copied().unwrap_or(0);
        if sigil == b'$' || sigil == b'@' || sigil == b'%' || sigil == b'&' {
            let prefix = match sigil {
                b'@' => "@",
                b'%' => "%",
                b'&' => "&",
                _ => "",
            };
            let (r2, n) = var_name(r)?;
            let full_name = format!("{}{}", prefix, n);
            let (r2, _) = ws(r2)?;

            // Check for optional suffix '?'
            let (r2, is_optional) = if let Some(after) = r2.strip_prefix('?') {
                (after, true)
            } else {
                (r2, false)
            };
            let (r2, _) = ws(r2)?;

            vars.push(DestructureVar {
                name: full_name,
                is_slurpy,
                is_optional,
                is_named,
            });

            if r2.starts_with(',') {
                let (r2, _) = parse_char(r2, ',')?;
                let (r2, _) = ws(r2)?;
                r = r2;
            } else {
                r = r2;
            }
        } else {
            return Err(PError::expected("variable sigil ($, @, %, &)"));
        }
    }
    let (rest, _) = parse_char(r, ')')?;
    let (rest, _) = ws(rest)?;
    let is_binding = rest.starts_with(":=") || rest.starts_with("::=");
    if rest.starts_with('=') || rest.starts_with("::=") || rest.starts_with(":=") {
        let rest = if let Some(stripped) = rest.strip_prefix("::=") {
            stripped
        } else if let Some(stripped) = rest.strip_prefix(":=") {
            stripped
        } else {
            &rest[1..]
        };
        let (rest, _) = ws(rest)?;
        let (rest, raw_rhs) = parse_comma_or_expr(rest)?;
        let (rest, _) = ws(rest)?;
        let (rest, _) = opt_char(rest, ';');

        let has_named = vars.iter().any(|v| v.is_named);

        // For := binding with positional params, wrap the RHS in .list
        // to handle Capture unpacking. Don't do this for named destructuring.
        let rhs = if is_binding && !has_named {
            Expr::MethodCall {
                target: Box::new(raw_rhs),
                name: "list".to_string(),
                args: vec![],
                modifier: None,
                quoted: false,
            }
        } else {
            raw_rhs
        };

        if has_named {
            // Named destructuring: bind from a hash
            // Desugar: my %__destructure_tmp__ = <rhs>; my @even = %tmp<even>; ...
            let tmp_name = "%__destructure_tmp__".to_string();
            let hash_bare = "__destructure_tmp__".to_string();
            let mut stmts = vec![Stmt::VarDecl {
                name: tmp_name,
                expr: rhs,
                type_constraint: None,
                is_state: false,
                is_our: false,
                is_dynamic: false,
                is_export: false,
                export_tags: Vec::new(),
                custom_traits: Vec::new(),
            }];
            for dvar in &vars {
                // Extract bare name (without sigil prefix)
                let bare_name = if dvar.name.starts_with('@')
                    || dvar.name.starts_with('%')
                    || dvar.name.starts_with('&')
                {
                    &dvar.name[1..]
                } else {
                    &dvar.name
                };
                stmts.push(Stmt::VarDecl {
                    name: dvar.name.clone(),
                    expr: Expr::Index {
                        target: Box::new(Expr::HashVar(hash_bare.clone())),
                        index: Box::new(Expr::Literal(Value::Str(bare_name.to_string()))),
                    },
                    type_constraint: None,
                    is_state: false,
                    is_our: false,
                    is_dynamic: false,
                    is_export: false,
                    export_tags: Vec::new(),
                    custom_traits: Vec::new(),
                });
            }
            return Ok((rest, Stmt::SyntheticBlock(stmts)));
        }

        // Positional destructuring
        let tmp_name = "@__destructure_tmp__".to_string();
        let array_bare = "__destructure_tmp__".to_string();
        let mut stmts = vec![Stmt::VarDecl {
            name: tmp_name,
            expr: rhs,
            type_constraint: None,
            is_state: false,
            is_our: false,
            is_dynamic: false,
            is_export: false,
            export_tags: Vec::new(),
            custom_traits: Vec::new(),
        }];
        let has_explicit_slurpy = vars.iter().any(|v| v.is_slurpy);
        for (i, dvar) in vars.iter().enumerate() {
            // An @-sigiled variable that is the last non-slurpy variable
            // should consume all remaining elements (implicit slurpy behavior).
            let is_implicit_slurpy = !has_explicit_slurpy
                && dvar.name.starts_with('@')
                && !vars[i + 1..].iter().any(|v| !v.is_slurpy);

            let expr = if dvar.is_slurpy || is_implicit_slurpy {
                // Slurpy: collect remaining elements from index i onward
                // Desugar: @rest = @tmp[i..*]
                Expr::Index {
                    target: Box::new(Expr::ArrayVar(array_bare.clone())),
                    index: Box::new(Expr::Binary {
                        left: Box::new(Expr::Literal(Value::Int(i as i64))),
                        op: TokenKind::DotDot,
                        right: Box::new(Expr::Whatever),
                    }),
                }
            } else {
                Expr::Index {
                    target: Box::new(Expr::ArrayVar(array_bare.clone())),
                    index: Box::new(Expr::Literal(Value::Int(i as i64))),
                }
            };
            stmts.push(Stmt::VarDecl {
                name: dvar.name.clone(),
                expr,
                type_constraint: None,
                is_state: false,
                is_our: false,
                is_dynamic: false,
                is_export: false,
                export_tags: Vec::new(),
                custom_traits: Vec::new(),
            });
            // For := binding, mark scalar variables as readonly
            if is_binding && dvar.name.starts_with(|c: char| c != '@' && c != '%') {
                stmts.push(Stmt::MarkReadonly(dvar.name.clone()));
            }
        }
        return Ok((rest, Stmt::SyntheticBlock(stmts)));
    }
    // No assignment
    let (rest, _) = ws(rest)?;
    let (rest, _) = opt_char(rest, ';');
    let mut stmts = Vec::new();
    for dvar in &vars {
        stmts.push(Stmt::VarDecl {
            name: dvar.name.clone(),
            expr: Expr::Literal(Value::Nil),
            type_constraint: None,
            is_state: false,
            is_our: false,
            is_dynamic: false,
            is_export: false,
            export_tags: Vec::new(),
            custom_traits: Vec::new(),
        });
    }
    Ok((rest, Stmt::SyntheticBlock(stmts)))
}

/// Parse `has` attribute declaration.
pub(super) fn has_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("has", input).ok_or_else(|| PError::expected("has declaration"))?;
    let (rest, _) = ws1(rest)?;

    // Optional type constraint (with optional smiley :D, :U, :_)
    let mut type_constraint: Option<String> = None;
    let rest = if let Ok((r, tc)) = ident(rest) {
        // Skip smiley after type name
        let r = if r.starts_with(":D") || r.starts_with(":U") || r.starts_with(":_") {
            &r[2..]
        } else {
            r
        };
        let (r2, _) = ws(r)?;
        if r2.starts_with('$') || r2.starts_with('@') || r2.starts_with('%') {
            type_constraint = Some(tc.to_string());
            r2
        } else {
            rest
        }
    } else {
        rest
    };

    let sigil = rest.as_bytes().first().copied().unwrap_or(0);
    let (rest, _) = if sigil == b'$' || sigil == b'@' || sigil == b'%' {
        (&rest[1..], ())
    } else {
        return Err(PError::expected("sigil ($, @, %)"));
    };

    // Check for public accessor marker '.'
    let (rest, is_public) = if let Some(stripped) = rest.strip_prefix('.') {
        (stripped, true)
    } else if let Some(stripped) = rest.strip_prefix('!') {
        (stripped, false)
    } else {
        (rest, false)
    };

    let (rest, name) = take_while1(rest, |c: char| c.is_alphanumeric() || c == '_' || c == '-')?;
    let name = name.to_string();
    let (mut rest, _) = ws(rest)?;

    // `is` traits (may have multiple: `is rw is required`)
    // Traits come before default value: `has $.x is rw = 42`
    let mut is_rw = false;
    while let Some(r) = keyword("is", rest) {
        let (r, _) = ws1(r)?;
        let (r, trait_name) = ident(r)?;
        if trait_name == "rw" {
            is_rw = true;
        }
        let (r, _) = ws(r)?;
        rest = r;
    }

    // `handles` trait, e.g. `has $.x handles <a b>`
    let mut handles = Vec::new();
    while let Some(r) = keyword("handles", rest) {
        let (r, _) = ws1(r)?;
        if let Some(r) = r.strip_prefix('<') {
            let mut cursor = r;
            loop {
                let (r_ws, _) = ws(cursor)?;
                cursor = r_ws;
                if let Some(r_end) = cursor.strip_prefix('>') {
                    rest = r_end;
                    break;
                }
                let (r_name, method_name) = take_while1(cursor, |c: char| {
                    c.is_alphanumeric() || c == '_' || c == '-'
                })?;
                handles.push(method_name.to_string());
                cursor = r_name;
            }
        } else if let Some(after_star) = r.strip_prefix('*') {
            // handles * — wildcard delegation
            handles.push("*".to_string());
            rest = after_star;
        } else {
            let (r_name, method_name) = ident(r)?;
            handles.push(method_name.to_string());
            rest = r_name;
        }
        let (r, _) = ws(rest)?;
        rest = r;
    }

    // Default value
    let (rest, mut default) = if rest.starts_with('=') && !rest.starts_with("==") {
        let rest = &rest[1..];
        let (rest, _) = ws(rest)?;
        let (rest, expr) = expression(rest)?;
        (rest, Some(expr))
    } else if let Some(tc) = &type_constraint {
        // Typed attribute with no explicit default → use type object as default
        (rest, Some(Expr::BareWord(tc.clone())))
    } else {
        (rest, None)
    };
    if sigil == b'@'
        && let Some(expr) = default.take()
    {
        default = Some(match expr {
            Expr::ArrayLiteral(_)
            | Expr::BracketArray(_)
            | Expr::ArrayVar(_)
            | Expr::Var(_)
            | Expr::Index { .. } => expr,
            other => Expr::ArrayLiteral(vec![other]),
        });
    }
    let (rest, _) = ws(rest)?;

    let (rest, _) = opt_char(rest, ';');
    Ok((
        rest,
        Stmt::HasDecl {
            name,
            is_public,
            default,
            handles,
            is_rw,
            type_constraint,
        },
    ))
}

/// Parse `enum` declaration.
/// Parse `anon enum` declaration.
pub(crate) fn anon_enum_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("anon", input).ok_or_else(|| PError::expected("anon enum declaration"))?;
    let (rest, _) = ws1(rest)?;
    let rest = keyword("enum", rest).ok_or_else(|| PError::expected("enum after anon"))?;
    let (rest, _) = ws1(rest)?;
    parse_anon_enum_body(rest)
}

pub(crate) fn enum_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("enum", input).ok_or_else(|| PError::expected("enum declaration"))?;
    let (rest, _) = ws1(rest)?;
    // Anonymous enum: `enum < foo bar >` or `enum :: < foo bar >`
    if rest.starts_with('<') || rest.starts_with('(') {
        return parse_anon_enum_body(rest);
    }
    if let Some(r) = rest.strip_prefix("::") {
        let (r, _) = ws(r)?;
        if r.starts_with('<') || r.starts_with('(') {
            return parse_anon_enum_body(r);
        }
    }
    parse_enum_decl_body(rest)
}

/// Parse anonymous enum body (after `enum` keyword with no name).
fn parse_anon_enum_body(input: &str) -> PResult<'_, Stmt> {
    let (rest, variants) = if input.starts_with('<') {
        let (r, _) = parse_char(input, '<')?;
        let mut variants = Vec::new();
        let mut r = r;
        loop {
            let (r2, _) = take_while_opt(r, |c: char| c == ' ' || c == '\t');
            if let Some(r2) = r2.strip_prefix('>') {
                r = r2;
                break;
            }
            let (r2, word) =
                take_while1(r2, |c: char| c != '>' && c != ' ' && c != '\t' && c != '\n')?;
            variants.push((word.to_string(), None));
            r = r2;
        }
        (r, variants)
    } else if input.starts_with('(') {
        let (r, _) = parse_char(input, '(')?;
        let (r, _) = ws(r)?;
        let mut variants = Vec::new();
        let mut r = r;
        loop {
            if let Some(r2) = r.strip_prefix(')') {
                r = r2;
                break;
            }
            let (r2, variant) = parse_enum_variant_entry(r)?;
            variants.push(variant);
            let (r2, _) = ws(r2)?;
            if let Some(stripped) = r2.strip_prefix(',') {
                let (r2, _) = ws(stripped)?;
                r = r2;
            } else {
                r = r2;
            }
        }
        (r, variants)
    } else {
        return Err(PError::expected("anonymous enum variants"));
    };
    let (rest, _) = ws(rest)?;
    let (rest, _) = opt_char(rest, ';');
    Ok((
        rest,
        Stmt::EnumDecl {
            name: String::new(),
            variants,
        },
    ))
}

fn parse_enum_variant_entry(input: &str) -> PResult<'_, (String, Option<Expr>)> {
    let (rest, expr) = expression(input)?;
    match expr {
        Expr::BareWord(name) | Expr::Literal(Value::Str(name)) => Ok((rest, (name, None))),
        Expr::Binary {
            left,
            op: crate::token_kind::TokenKind::FatArrow,
            right,
        } => match *left {
            Expr::Literal(Value::Str(name)) => {
                let value_expr = match *right {
                    Expr::Literal(Value::Bool(true)) => None,
                    other => Some(other),
                };
                Ok((rest, (name, value_expr)))
            }
            _ => Err(PError::expected("enum variant name")),
        },
        _ => Err(PError::expected("enum variant")),
    }
}

pub(super) fn parse_enum_decl_body(input: &str) -> PResult<'_, Stmt> {
    let (rest, name) = ident(input)?;
    let (rest, _) = ws(rest)?;

    // Enum variants in <> or ()
    let (rest, variants) = if rest.starts_with('<') {
        let (r, _) = parse_char(rest, '<')?;
        let mut variants = Vec::new();
        let mut r = r;
        loop {
            let (r2, _) = take_while_opt(r, |c: char| c == ' ' || c == '\t');
            if let Some(r2) = r2.strip_prefix('>') {
                r = r2;
                break;
            }
            let (r2, word) =
                take_while1(r2, |c: char| c != '>' && c != ' ' && c != '\t' && c != '\n')?;
            variants.push((word.to_string(), None));
            r = r2;
        }
        (r, variants)
    } else if rest.starts_with('(') {
        let (r, _) = parse_char(rest, '(')?;
        let (r, _) = ws(r)?;
        let mut variants = Vec::new();
        let mut r = r;
        loop {
            if let Some(r) = r.strip_prefix(')') {
                return Ok((r, Stmt::EnumDecl { name, variants }));
            }
            let (r2, variant) = parse_enum_variant_entry(r)?;
            variants.push(variant);
            let (r2, _) = ws(r2)?;
            if let Some(stripped) = r2.strip_prefix(',') {
                let (r2, _) = ws(stripped)?;
                r = r2;
            } else {
                r = r2;
            }
        }
    } else {
        (rest, Vec::new())
    };

    let (rest, _) = ws(rest)?;
    let (rest, _) = opt_char(rest, ';');
    Ok((rest, Stmt::EnumDecl { name, variants }))
}

/// Parse `constant` declaration.
pub(super) fn constant_decl(input: &str) -> PResult<'_, Stmt> {
    let rest =
        keyword("constant", input).ok_or_else(|| PError::expected("constant declaration"))?;
    let (rest, _) = ws1(rest)?;
    // The name can be $var, @var, %var, &var, or bare identifier.
    // Keep the sigil in the stored name so lookup semantics match normal
    // declarations (`my @x` stores `@x`, not bare `x`).
    let sigil = rest.as_bytes().first().copied().unwrap_or(0);
    let (rest, name) = if let Some(r) = rest.strip_prefix('\\') {
        let (r, n) = parse_sigilless_decl_name(r)?;
        register_term_symbol_from_decl_name(&n);
        (r, n)
    } else if matches!(sigil, b'$' | b'@' | b'%' | b'&') {
        let prefix = match sigil {
            b'@' => "@",
            b'%' => "%",
            b'&' => "&",
            _ => "",
        };
        let (r, n) = var_name(rest)?;
        let name = format!("{prefix}{n}");
        register_term_symbol_from_decl_name(&name);
        (r, name)
    } else {
        let (r, n) = ident(rest)?;
        register_term_symbol_from_decl_name(&n);
        (r, n)
    };
    let (mut rest, _) = ws(rest)?;
    let mut is_export = false;
    let mut export_tags: Vec<String> = Vec::new();
    while let Some(after_is) = keyword("is", rest) {
        let (r2, _) = ws1(after_is)?;
        let (r2, trait_name) = ident(r2)?;
        if trait_name != "export" {
            return Err(PError::fatal(format!(
                "X::Comp::Trait::Unknown: Unknown variable trait 'is {}'",
                trait_name
            )));
        }
        is_export = true;
        let (r3, tags) = parse_export_trait_tags(r2)?;
        if tags.is_empty() {
            if !export_tags.iter().any(|t| t == "DEFAULT") {
                export_tags.push("DEFAULT".to_string());
            }
        } else {
            for tag in tags {
                if !export_tags.iter().any(|t| t == &tag) {
                    export_tags.push(tag);
                }
            }
        }
        rest = r3;
    }
    if rest.starts_with('=') || rest.starts_with("::=") || rest.starts_with(":=") {
        let rest = if let Some(stripped) = rest.strip_prefix("::=") {
            stripped
        } else if let Some(stripped) = rest.strip_prefix(":=") {
            stripped
        } else {
            &rest[1..]
        };
        let (rest, _) = ws(rest)?;
        let (rest, expr) = parse_comma_or_expr(rest)?;
        // Track compile-time string constants for operator name resolution
        if let crate::ast::Expr::Literal(crate::value::Value::Str(ref s)) = expr {
            super::simple::register_compile_time_constant(&name, s.clone());
        }
        let (rest, _) = ws(rest)?;
        let (rest, _) = opt_char(rest, ';');
        return Ok((
            rest,
            Stmt::VarDecl {
                name,
                expr,
                type_constraint: None,
                is_state: false,
                is_our: true,
                is_dynamic: false,
                is_export,
                export_tags: export_tags.clone(),
                custom_traits: Vec::new(),
            },
        ));
    }
    let (rest, _) = opt_char(rest, ';');
    Ok((
        rest,
        Stmt::VarDecl {
            name,
            expr: Expr::Literal(Value::Nil),
            type_constraint: None,
            is_state: false,
            is_our: true,
            is_dynamic: false,
            is_export,
            export_tags,
            custom_traits: Vec::new(),
        },
    ))
}

/// Parse `subset` declaration.
pub(super) fn subset_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("subset", input).ok_or_else(|| PError::expected("subset declaration"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, name) = ident(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, base) = if let Some(r) = keyword("of", rest) {
        let (r, _) = ws1(r)?;
        let (r, base) = parse_type_constraint_expr(r).ok_or_else(|| PError::expected("type"))?;
        let (r, _) = ws(r)?;
        (r, base)
    } else {
        (rest, "Any".to_string())
    };
    let (rest, predicate) = if let Some(r) = keyword("where", rest) {
        let (r, _) = ws1(r)?;
        let (r, pred) = expression(r)?;
        (r, Some(pred))
    } else {
        (rest, None)
    };
    let (rest, _) = ws(rest)?;
    let (rest, _) = opt_char(rest, ';');
    Ok((
        rest,
        Stmt::SubsetDecl {
            name,
            base,
            predicate,
        },
    ))
}
