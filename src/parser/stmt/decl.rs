use super::super::expr::expression;
use super::super::helpers::{skip_balanced_parens, ws, ws1};
use super::super::parse_result::{
    PError, PResult, merge_expected_messages, opt_char, parse_char, take_while1,
};

/// Parse a single argument in colon method-call syntax (.method: arg1, arg2).
/// Tries colonpair first (:name, :$var, :!flag, :0port), then expression.
fn parse_colon_method_arg(input: &str) -> PResult<'_, Expr> {
    if input.starts_with(':')
        && !input.starts_with("::")
        && let Ok(result) = crate::parser::primary::misc::colonpair_expr(input)
    {
        return Ok(result);
    }
    expression(input)
}

use crate::ast::{AssignOp, Expr, HandleSpec, PhaserKind, Stmt};
use crate::symbol::Symbol;
use crate::token_kind::TokenKind;
use crate::value::Value;

use super::sub::parse_type_constraint_expr;
use super::sub_param::parse_of_type_constraint_chain;
use super::{
    class::{module_decl, package_decl, proto_decl, role_decl},
    ident, keyword, parse_assign_expr_or_comma, parse_statement_modifier, qualified_ident,
    var_name,
};

use super::super::parse_result::take_while_opt;
use super::{
    class_decl_body, method_decl_body, method_decl_body_my, parse_comma_or_expr, sub_decl_body,
};

fn parse_decl_type_constraint(input: &str) -> Option<(&str, String)> {
    let (mut rest, mut type_name) = parse_type_constraint_expr(input)?;
    loop {
        let (after_ws, _) = ws(rest).ok()?;
        let Some(after_of) = keyword("of", after_ws) else {
            break;
        };
        let (after_of, _) = ws1(after_of).ok()?;
        let (after_inner, inner_type) = parse_decl_type_constraint(after_of)?;
        type_name = format!("{}[{}]", type_name, inner_type);
        rest = after_inner;
    }
    Some((rest, type_name))
}

/// Wrap a statement with a LEAVE phaser for `will leave { ... }`.
/// The phaser body sets `$_` to the variable and executes the block.
fn wrap_with_will_leave(stmt: Stmt, var_name: &str, leave_body: Option<Vec<Stmt>>) -> Stmt {
    match leave_body {
        None => stmt,
        Some(body) => {
            // Build: LEAVE { my $_ = $var; <body> }
            let mut phaser_body = vec![Stmt::VarDecl {
                name: "$_".to_string(),
                expr: Expr::Var(var_name.to_string()),
                type_constraint: None,
                is_state: false,
                is_our: false,
                is_dynamic: false,
                is_export: false,
                export_tags: Vec::new(),
                custom_traits: Vec::new(),
                where_constraint: None,
            }];
            phaser_body.extend(body);
            let phaser = Stmt::Phaser {
                kind: PhaserKind::Leave,
                body: phaser_body,
            };
            Stmt::SyntheticBlock(vec![stmt, phaser])
        }
    }
}

fn strip_type_smiley_suffix(type_name: &str) -> &str {
    type_name
        .strip_suffix(":U")
        .or_else(|| type_name.strip_suffix(":D"))
        .or_else(|| type_name.strip_suffix(":_"))
        .unwrap_or(type_name)
}

fn typed_default_expr(type_name: &str) -> Expr {
    let base = strip_type_smiley_suffix(type_name);
    if base == "Mu" {
        Expr::BareWord("Mu".to_string())
    } else if base == "int"
        || base == "int8"
        || base == "int16"
        || base == "int32"
        || base == "int64"
        || base == "uint"
        || base == "uint8"
        || base == "uint16"
        || base == "uint32"
        || base == "uint64"
        || base == "byte"
    {
        Expr::Literal(Value::Int(0))
    } else if base == "num" || base == "num32" || base == "num64" {
        Expr::Literal(Value::Num(0.0))
    } else if base == "str" {
        Expr::Literal(Value::str_from(""))
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

fn scalar_binding_rhs_is_readonly(expr: &Expr) -> bool {
    matches!(expr, Expr::Literal(_))
}

/// Handle comma-separated `my` declarations: `my $x = 1, my $y = 2`
/// If the remaining input starts with `, my/our/state`, parse the subsequent
/// declarations and combine them into a SyntheticBlock.
fn parse_comma_chained_decls<'a>(input: &'a str, first: Stmt) -> PResult<'a, Stmt> {
    let (r, _) = ws(input)?;
    if !r.starts_with(',') {
        return Ok((input, first));
    }
    let after_comma = &r[1..];
    let (after_ws, _) = ws(after_comma)?;
    // Check if the next token is my/our/state (another declaration)
    if keyword("my", after_ws).is_none()
        && keyword("our", after_ws).is_none()
        && keyword("state", after_ws).is_none()
    {
        return Ok((input, first));
    }
    // Parse the next declaration
    let (rest, next) = my_decl_inner(after_ws, false)?;
    let mut stmts = match first {
        Stmt::SyntheticBlock(v) => v,
        other => vec![other],
    };
    match next {
        Stmt::SyntheticBlock(v) => stmts.extend(v),
        other => stmts.push(other),
    }
    Ok((rest, Stmt::SyntheticBlock(stmts)))
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

fn register_term_symbol_from_decl_name(name: &str) {
    if let Some(callable_name) = name.strip_prefix('&') {
        super::simple::register_user_callable_term_symbol(callable_name);
        // Also register operator categories (infix, prefix, postfix, circumfix,
        // postcircumfix) as user subs so the parser recognizes them as operators.
        // This handles `constant &infix:<op> = ...` style declarations.
        if callable_name.starts_with("infix:")
            || callable_name.starts_with("prefix:")
            || callable_name.starts_with("postfix:")
            || callable_name.starts_with("circumfix:")
            || callable_name.starts_with("postcircumfix:")
        {
            super::simple::register_user_sub(callable_name);
        }
    } else {
        // Don't register keywords as term symbols — they must be handled
        // by the keyword-specific paths in identifier_or_call, not as
        // bare words via declared_term_symbol. E.g. `my $sub` should not
        // cause `sub` to be treated as a bare word on the RHS.
        if !is_parser_keyword(name) {
            super::simple::register_user_term_symbol(name);
        }
    }
}

fn normalize_language_version(version_token: &str) -> String {
    if version_token.starts_with("v6.c") {
        "6.c".to_string()
    } else if version_token.starts_with("v6.d") {
        "6.d".to_string()
    } else {
        "6.e".to_string()
    }
}

fn is_parser_keyword(name: &str) -> bool {
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

/// Parse a `use` statement.
pub(super) fn use_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("use", input).ok_or_else(|| PError::expected("use statement"))?;
    let (rest, _) = ws1(rest)?;

    // Handle `use v6`, `use v6.d`, etc.
    // Only match when 'v' is followed by a digit (version number), not other identifiers.
    if rest.starts_with('v') && rest[1..].chars().next().is_some_and(|c| c.is_ascii_digit()) {
        let (r, version_token) = take_while1(rest, |c: char| {
            c.is_alphanumeric() || c == '.' || c == '*' || c == '+'
        })?;
        let version = normalize_language_version(version_token);
        super::simple::set_current_language_version(&version);
        let (r, _) = ws(r)?;
        let _ = opt_char(r, ';');
        let (r, _) = opt_char(r, ';');
        return Ok((
            r,
            Stmt::Use {
                module: "v6".to_string(),
                arg: Some(Expr::Literal(Value::str(version))),
            },
        ));
    }

    let (rest, module) = qualified_ident(rest)?;
    let (rest, _) = ws(rest)?;

    // Handle `use variables :D/:U/:_` pragma
    if module == "variables" {
        return parse_use_variables_pragma(rest);
    }

    // Handle `use attributes :D/:U/:_` pragma
    if module == "attributes" {
        return parse_use_attributes_pragma(rest);
    }

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

    // Skip adverbs/colonpairs on use (e.g. `use Foo :ALL`, `use Foo :tag1, :tag2`)
    let mut rest = rest;
    loop {
        if rest.starts_with(':') && !rest.starts_with("::") {
            let r = &rest[1..];
            // :!name
            let r = r.strip_prefix('!').unwrap_or(r);
            if let Ok((r, _name)) = ident(r) {
                // :name(expr)
                let r = skip_balanced_parens(r);
                let (r, _) = ws(r)?;
                // Skip optional comma between tags
                let r = r.strip_prefix(',').unwrap_or(r);
                let (r, _) = ws(r)?;
                rest = r;
            } else {
                break;
            }
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

/// Parse `use <pragma_name> :D/:U/:_` pragma.
/// Validates the argument and emits the Use statement.
/// Shared implementation for both `use variables` and `use attributes`.
fn parse_use_smiley_pragma<'a>(input: &'a str, pragma_name: &'a str) -> PResult<'a, Stmt> {
    let rest = input;

    // No argument → X::Pragma::MustOneOf
    if rest.starts_with(';') || rest.is_empty() || rest.starts_with('}') {
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("name".to_string(), Value::str(pragma_name.to_string()));
        attrs.insert(
            "message".to_string(),
            Value::str(format!(
                "Must use one of :D, :U, :_ with '{}' pragma",
                pragma_name
            )),
        );
        let ex = Value::make_instance(Symbol::intern("X::Pragma::MustOneOf"), attrs);
        return Err(PError::fatal_with_exception(
            format!("Must use one of :D, :U, :_ with '{}' pragma", pragma_name),
            Box::new(ex),
        ));
    }

    // Must start with ':'
    if !rest.starts_with(':') {
        // String argument like `use variables "bar"` → X::Pragma::UnknownArg
        let (r, arg_expr) = expression(rest)?;
        let arg_str = format!("{:?}", arg_expr);
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("name".to_string(), Value::str(pragma_name.to_string()));
        // Try to extract string value from the expression
        let arg_val = if let Expr::Literal(Value::Str(ref s)) = arg_expr {
            s.to_string()
        } else {
            arg_str
        };
        attrs.insert("arg".to_string(), Value::str(arg_val.clone()));
        attrs.insert(
            "message".to_string(),
            Value::str(format!(
                "Unknown argument '{}' to '{}' pragma",
                arg_val, pragma_name
            )),
        );
        let ex = Value::make_instance(Symbol::intern("X::Pragma::UnknownArg"), attrs);
        let _ = r;
        return Err(PError::fatal_with_exception(
            format!("Unknown argument '{}' to '{}' pragma", arg_val, pragma_name),
            Box::new(ex),
        ));
    }

    // Parse colonpair arguments
    let mut smileys: Vec<String> = Vec::new();
    let mut r = rest;
    loop {
        if !r.starts_with(':') || r.starts_with("::") {
            break;
        }
        let after_colon = &r[1..];
        if let Ok((r2, smiley_name)) = ident(after_colon) {
            smileys.push(smiley_name.to_string());
            let (r2, _) = ws(r2)?;
            // Skip optional comma
            let r2 = r2.strip_prefix(',').unwrap_or(r2);
            let (r2, _) = ws(r2)?;
            r = r2;
        } else {
            break;
        }
    }

    // Validate smileys
    if smileys.is_empty() {
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("name".to_string(), Value::str(pragma_name.to_string()));
        let ex = Value::make_instance(Symbol::intern("X::Pragma::MustOneOf"), attrs);
        return Err(PError::fatal_with_exception(
            format!("Must use one of :D, :U, :_ with '{}' pragma", pragma_name),
            Box::new(ex),
        ));
    }

    // Check for invalid smileys
    for s in &smileys {
        if s != "D" && s != "U" && s != "_" {
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("name".to_string(), Value::str(s.clone()));
            attrs.insert(
                "message".to_string(),
                Value::str(format!(
                    "Invalid type smiley ':{}' used, only ':D', ':U' and ':_' are allowed",
                    s
                )),
            );
            let ex = Value::make_instance(Symbol::intern("X::InvalidTypeSmiley"), attrs);
            return Err(PError::fatal_with_exception(
                format!(
                    "Invalid type smiley ':{}' used, only ':D', ':U' and ':_' are allowed",
                    s
                ),
                Box::new(ex),
            ));
        }
    }

    // Multiple smileys → X::Pragma::OnlyOne
    if smileys.len() > 1 {
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("name".to_string(), Value::str(pragma_name.to_string()));
        attrs.insert(
            "message".to_string(),
            Value::str(format!(
                "Can only use one of :D, :U, :_ with '{}' pragma",
                pragma_name
            )),
        );
        let ex = Value::make_instance(Symbol::intern("X::Pragma::OnlyOne"), attrs);
        return Err(PError::fatal_with_exception(
            format!(
                "Can only use one of :D, :U, :_ with '{}' pragma",
                pragma_name
            ),
            Box::new(ex),
        ));
    }

    let smiley = &smileys[0];
    let (r, _) = ws(r)?;
    let (r, _) = opt_char(r, ';');
    Ok((
        r,
        Stmt::Use {
            module: pragma_name.to_string(),
            arg: Some(Expr::Literal(Value::str(format!(":{}", smiley)))),
        },
    ))
}

/// Parse `use variables :D/:U/:_` pragma.
fn parse_use_variables_pragma(input: &str) -> PResult<'_, Stmt> {
    parse_use_smiley_pragma(input, "variables")
}

/// Parse `use attributes :D/:U/:_` pragma.
fn parse_use_attributes_pragma(input: &str) -> PResult<'_, Stmt> {
    let result = parse_use_smiley_pragma(input, "attributes")?;
    // Set the parse-time attributes pragma so has_decl can check it
    if let Stmt::Use {
        arg: Some(Expr::Literal(Value::Str(ref s))),
        ..
    } = result.1
    {
        super::simple::set_attributes_pragma(s);
    }
    Ok(result)
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

    // `no attributes` is not allowed — throw X::Pragma::CannotWhat
    if module == "attributes" {
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("what".to_string(), Value::str("no".to_string()));
        attrs.insert("name".to_string(), Value::str("attributes".to_string()));
        attrs.insert(
            "message".to_string(),
            Value::str("Cannot use 'no' with the 'attributes' pragma".to_string()),
        );
        let ex = Value::make_instance(Symbol::intern("X::Pragma::CannotWhat"), attrs);
        return Err(PError::fatal_with_exception(
            "Cannot use 'no' with the 'attributes' pragma".to_string(),
            Box::new(ex),
        ));
    }

    // `no variables` is not allowed — throw X::Pragma::CannotWhat
    if module == "variables" {
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("what".to_string(), Value::str("no".to_string()));
        attrs.insert("name".to_string(), Value::str("variables".to_string()));
        attrs.insert(
            "message".to_string(),
            Value::str("Cannot use 'no' with the 'variables' pragma".to_string()),
        );
        let ex = Value::make_instance(Symbol::intern("X::Pragma::CannotWhat"), attrs);
        return Err(PError::fatal_with_exception(
            "Cannot use 'no' with the 'variables' pragma".to_string(),
            Box::new(ex),
        ));
    }

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
    let (rest, stmt) = my_decl_inner(input, true)?;
    // If postfix ++ / -- follows, reject from statement-level parsing so the
    // expression parser can handle `state $i++` as PostfixOp(DoStmt(VarDecl)).
    if rest.starts_with("++") || rest.starts_with("--") {
        return Err(PError::expected(
            "statement (postfix operator on declarator requires expression context)",
        ));
    }
    Ok((rest, stmt))
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
    let (mut rest, _) = ws1(rest)?;

    // my enum Foo <...>
    // my Array enum Foo <...>  (typed enum — base type is accepted but currently ignored)
    if let Some(r) = keyword("enum", rest) {
        let (r, _) = ws1(r)?;
        return parse_enum_decl_body(r);
    }
    // Check for `my <Type> enum <Name> ...` (e.g., `my Array enum PageSizes «...»`)
    {
        let saved = rest;
        if let Ok((after_type, type_name)) = ident(rest)
            && let Ok((after_ws, _)) = ws1(after_type)
            && let Some(r) = keyword("enum", after_ws)
            && let Ok((r, _)) = ws1(r)
        {
            return parse_enum_decl_body_with_type(r, Some(type_name));
        }
        rest = saved;
    }

    // my/our proto ...
    if keyword("proto", rest).is_some() {
        // If proto is followed by a variable sigil (e.g., `my proto $!`),
        // treat it as a regular variable declaration, ignoring proto.
        let after_proto = keyword("proto", rest).unwrap();
        let (after_ws, _) = ws(after_proto)?;
        if after_ws.starts_with('$')
            || after_ws.starts_with('@')
            || after_ws.starts_with('%')
            || after_ws.starts_with('&')
        {
            rest = after_ws;
            // Fall through to normal variable parsing below
        } else {
            return proto_decl(rest);
        }
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
            if let Stmt::SubDecl {
                return_type,
                custom_traits,
                ..
            } = &mut stmt
            {
                if return_type.is_none() {
                    *return_type = Some(routine_type.clone());
                }
                if is_our {
                    custom_traits.push("__our_scoped".to_string());
                }
            }
            return Ok((r, stmt));
        }
        if let Some(r) = keyword("sub", after_type) {
            let (r, _) = ws1(r)?;
            let (r, mut stmt) = sub_decl_body(r, false, false, false)?;
            if let Stmt::SubDecl {
                return_type,
                custom_traits,
                ..
            } = &mut stmt
            {
                if return_type.is_none() {
                    *return_type = Some(routine_type.clone());
                }
                if is_our {
                    custom_traits.push("__our_scoped".to_string());
                }
            }
            return Ok((r, stmt));
        }
        // `my Str subset MyStr [where ...]` — shorthand subset with prefix base type
        if let Some(r) = keyword("subset", after_type) {
            let (r, _) = ws1(r)?;
            let (r, name) = qualified_ident(r)?;
            let (mut r, _) = ws(r)?;
            // Parse optional traits (e.g. `is export`)
            while let Some(r2) = keyword("is", r) {
                let (r2, _) = ws1(r2)?;
                let (r2, trait_name) = ident(r2)?;
                let (r2, _) = ws(r2)?;
                if trait_name == "export" {
                    r = skip_balanced_parens(r2);
                    let (r2, _) = ws(r)?;
                    r = r2;
                } else {
                    r = r2;
                }
            }
            let (r, predicate) = if let Some(r2) = keyword("where", r) {
                let (r2, _) = ws1(r2)?;
                let (r2, pred) = expression(r2)?;
                (r2, Some(pred))
            } else {
                (r, None)
            };
            let (r, _) = ws(r)?;
            let (r, _) = opt_char(r, ';');
            return Ok((
                r,
                Stmt::SubsetDecl {
                    name: Symbol::intern(&name),
                    base: routine_type,
                    predicate,
                    version: super::simple::current_language_version(),
                },
            ));
        }
        // Check for multiple prefix constraints on routine: `our Int Str sub foo()`
        if let Some((after_second_type, _second_tc)) = parse_type_constraint_expr(after_type) {
            let (after_second_type, _) = ws(after_second_type)?;
            if keyword("sub", after_second_type).is_some()
                || keyword("multi", after_second_type).is_some()
            {
                return Err(PError::raw(
                    "FATAL:X::Comp::NYI: Multiple prefix constraints not yet implemented. Sorry."
                        .to_string(),
                    Some(after_type.len()),
                ));
            }
        }
    }

    // my multi [sub] name(...) { ... }
    if let Some(r) = keyword("multi", rest) {
        let (r, _) = ws1(r)?;
        // "sub" is optional after "multi"
        let r = keyword("sub", r)
            .map(|r2| ws(r2).map(|(r3, _)| r3).unwrap_or(r2))
            .unwrap_or(r);
        let (r, mut stmt) = sub_decl_body(r, true, false, false)?;
        if is_our && let Stmt::SubDecl { custom_traits, .. } = &mut stmt {
            custom_traits.push("__our_scoped".to_string());
        }
        return Ok((r, stmt));
    }

    // my sub name(...) { ... }
    if let Some(r) = keyword("sub", rest) {
        let (r, _) = ws1(r)?;
        let (r, mut stmt) = sub_decl_body(r, false, false, false)?;
        if is_our && let Stmt::SubDecl { custom_traits, .. } = &mut stmt {
            custom_traits.push("__our_scoped".to_string());
        }
        return Ok((r, stmt));
    }

    // my/our method name(...) { ... }
    if let Some(r) = keyword("method", rest) {
        let (r, _) = ws1(r)?;
        if is_our {
            return method_decl_body(r, false, true);
        } else {
            return method_decl_body_my(r, false, false);
        }
    }

    // my/our submethod name(...) { ... }
    if let Some(r) = keyword("submethod", rest) {
        let (r, _) = ws1(r)?;
        if is_our {
            return method_decl_body(r, false, true);
        } else {
            return method_decl_body_my(r, false, false);
        }
    }

    // my class Name is Parent { ... }
    if let Some(r) = keyword("class", rest) {
        let (r, _) = ws1(r)?;
        return class_decl_body(r, !is_our);
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
    if keyword("subset", rest).is_some() {
        return subset_decl(rest);
    }
    // my constant $x = ...
    if keyword("constant", rest).is_some() {
        let (r, mut stmt) = constant_decl(rest)?;
        // Patch scope: `my constant` → lexical; `our constant` → package scoped.
        // `constant_decl()` defaults to `is_our: true` (implied `our`), but when
        // prefixed with `my`/`our` the outer scope declaration should take precedence.
        if let Stmt::VarDecl {
            is_our: ref mut our_flag,
            ..
        } = stmt
        {
            *our_flag = is_our;
        }
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
            let decl = Stmt::VarDecl {
                name: name.clone(),
                expr,
                type_constraint: None,
                is_state,
                is_our,
                is_dynamic: false,
                is_export: false,
                export_tags: Vec::new(),
                custom_traits: Vec::new(),
                where_constraint: None,
            };
            // Sigilless variables are always readonly (bound, not assigned).
            // Use MarkSigillessReadonly to set __mutsu_sigilless_readonly::NAME
            // in the env, rather than MarkReadonly which would conflict with
            // sigiled variables of the same name.
            let stmt = Stmt::SyntheticBlock(vec![decl, Stmt::MarkSigillessReadonly(name)]);
            if apply_modifier {
                return parse_statement_modifier(r, stmt);
            }
            return Ok((r, stmt));
        }
        if r.starts_with('=') && !r.starts_with("==") && !r.starts_with("=>") {
            let r = &r[1..];
            let (r, _) = ws(r)?;
            let (r, expr) = parse_assign_expr_or_comma(r)?;
            let decl = Stmt::VarDecl {
                name: name.clone(),
                expr,
                type_constraint: None,
                is_state,
                is_our,
                is_dynamic: false,
                is_export: false,
                export_tags: Vec::new(),
                custom_traits: Vec::new(),
                where_constraint: None,
            };
            // Sigilless variables are always readonly (bound, not assigned).
            let stmt = Stmt::SyntheticBlock(vec![decl, Stmt::MarkSigillessReadonly(name)]);
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
                where_constraint: None,
            },
        ));
    }

    // Handle type capture: my ::a $a — `::name` captures the type of the variable.
    // We treat it as a type constraint string starting with "::" to distinguish
    // it from normal type constraints at runtime.
    let (rest, type_constraint) = if rest.starts_with("::")
        && !rest.starts_with("::=")
        && rest[2..]
            .chars()
            .next()
            .is_some_and(|c| c.is_alphabetic() || c == '_')
    {
        let ident_start = &rest[2..];
        let end = ident_start
            .find(|c: char| !c.is_alphanumeric() && c != '_' && c != '-')
            .unwrap_or(ident_start.len());
        let capture_name = &ident_start[..end];
        let tc = format!("::{}", capture_name);
        let r = &ident_start[end..];
        let (r, _) = ws(r)?;
        (r, Some(tc))
    } else {
        (rest, None)
    };

    // Optional type constraint: my Int $x or my Str(Match) $x (coercion type)
    let (mut rest, mut type_constraint) = if type_constraint.is_some() {
        (rest, type_constraint)
    } else {
        // Try to parse a type name followed by a sigil or \ or `constant`
        let saved = rest;
        if let Some((r, tc)) = parse_decl_type_constraint(rest) {
            let (r2, _) = ws(r)?;
            if r2.starts_with('$')
                || r2.starts_with('@')
                || r2.starts_with('%')
                || r2.starts_with('&')
                || r2.starts_with('\\')
                || r2.starts_with('(')
                || keyword("constant", r2).is_some()
            {
                (r2, Some(tc))
            } else {
                // Check for multiple prefix type constraints (e.g. `my Int Str $x`)
                // where the second type name is followed by a sigil
                if let Some((r3, _second_tc)) = parse_decl_type_constraint(r2) {
                    let (r4, _) = ws(r3).unwrap_or((r3, ()));
                    if r4.starts_with('$')
                        || r4.starts_with('@')
                        || r4.starts_with('%')
                        || r4.starts_with('&')
                        || r4.starts_with('\\')
                    {
                        return Err(PError::raw(
                            "FATAL:X::Comp::NYI: Multiple prefix constraints not yet implemented. Sorry."
                                .to_string(),
                            Some(r2.len()),
                        ));
                    }
                }
                (saved, None)
            }
        } else {
            (saved, None)
        }
    };

    if let Some(tc) = type_constraint.take() {
        let (r, tc) =
            parse_of_type_constraint_chain(rest, tc).ok_or_else(|| PError::expected("type"))?;
        type_constraint = Some(tc);
        rest = r;
    }

    // Check for invalid type smileys (e.g. Int:foo)
    // Only check the last `:X` that is NOT part of `::` (namespace separator).
    if let Some(ref tc) = type_constraint
        && let Some(colon_pos) = tc.rfind(':')
        && (colon_pos == 0 || tc.as_bytes()[colon_pos - 1] != b':')
    {
        let smiley = &tc[colon_pos + 1..];
        if !smiley.is_empty()
            && smiley != "D"
            && smiley != "U"
            && smiley != "_"
            && smiley.chars().next().is_some_and(|c| c.is_alphabetic())
        {
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("name".to_string(), Value::str(smiley.to_string()));
            attrs.insert(
                "message".to_string(),
                Value::str(format!(
                    "Invalid type smiley ':{}' used, only ':D', ':U' and ':_' are allowed",
                    smiley
                )),
            );
            let ex = Value::make_instance(Symbol::intern("X::InvalidTypeSmiley"), attrs);
            return Err(PError::fatal_with_exception(
                format!(
                    "Invalid type smiley ':{}' used, only ':D', ':U' and ':_' are allowed",
                    smiley
                ),
                Box::new(ex),
            ));
        }
    }

    // my <Type> constant <name> = <expr>
    if keyword("constant", rest).is_some() {
        let (r, mut stmt) = constant_decl(rest)?;
        // Patch in the type constraint and scope
        if let Stmt::VarDecl {
            type_constraint: ref mut tc,
            is_our: ref mut our_flag,
            ref mut expr,
            ..
        } = stmt
        {
            // For `.= new` style declarations, the default receiver is Mu.
            // Replace it with the actual type constraint so `my Foo constant x .= new`
            // compiles to `Foo.new(...)` instead of `Mu.new(...)`.
            if let Some(ref type_name) = type_constraint
                && let Expr::MethodCall { target, .. } = expr
                && matches!(target.as_ref(), Expr::BareWord(w) if w == "Mu")
            {
                **target = Expr::BareWord(type_name.clone());
            }
            *tc = type_constraint;
            // `my <Type> constant` → lexically scoped; `our <Type> constant` → package scoped
            *our_flag = is_our;
        }
        if apply_modifier {
            return parse_statement_modifier(r, stmt);
        }
        return Ok((r, stmt));
    }

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
                where_constraint: None,
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
                where_constraint: None,
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
                where_constraint: None,
            },
        ));
    }

    // Destructuring: my ($a, $b) = expr  or  my Int:D ($x = 5)
    if rest.starts_with('(') {
        return parse_destructuring_decl(rest, is_state, type_constraint);
    }

    // Parse variable
    let sigil = rest.as_bytes().first().copied().unwrap_or(0);
    let is_array = sigil == b'@';
    let is_hash = sigil == b'%';
    let is_code = sigil == b'&';

    // Handle `our $.name` / `my $.name` — class-level (shared) attributes.
    // These use the dot twigil which means "public accessor".
    if (sigil == b'$' || is_array || is_hash)
        && rest.len() > 2
        && rest.as_bytes()[1] == b'.'
        && rest[2..]
            .chars()
            .next()
            .is_some_and(|c| c.is_alphabetic() || c == '_')
    {
        let after_dot = &rest[2..];
        let (after_name, attr_name) = take_while1(after_dot, |c: char| {
            c.is_alphanumeric() || c == '_' || c == '-'
        })?;
        let attr_name = attr_name.to_string();
        let (after_name, _) = ws(after_name)?;
        // Parse optional default value
        let (after_name, default) = if after_name.starts_with('=')
            && !after_name.starts_with("==")
            && !after_name.starts_with("=>")
        {
            let r = &after_name[1..];
            let (r, _) = ws(r)?;
            let (r, expr) = expression(r)?;
            (r, Some(expr))
        } else {
            (after_name, None)
        };
        let stmt = Stmt::HasDecl {
            name: Symbol::intern(&attr_name),
            is_public: true,
            default,
            handles: Vec::new(),
            is_rw: true,
            is_readonly: false,
            type_constraint: type_constraint.clone(),
            type_smiley: None,
            is_required: None,
            sigil: sigil as char,
            where_constraint: None,
            is_alias: false,
            is_our,
            is_my: !is_our && !is_state,
            is_default: None,
        };
        if apply_modifier {
            return parse_statement_modifier(after_name, stmt);
        }
        return Ok((after_name, stmt));
    }

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
        // Sigilless variable without \ — this is an error in Raku
        return Err(PError::fatal(
            "X::Syntax::Malformed: Malformed my variable (did you mean to declare a sigilless variable with \\?)".to_string(),
        ));
    };
    let term_decl_name = if sigil == b'$' {
        format!("${name}")
    } else {
        name.clone()
    };
    register_term_symbol_from_decl_name(&term_decl_name);

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
    let mut has_dynamic_trait = name.starts_with('*')
        || name.starts_with("@*")
        || name.starts_with("%*")
        || name.starts_with("&*");
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
            if is_hash && (trait_name == "Mix" || trait_name == "MixHash") {
                type_constraint = Some(trait_name.clone());
            }
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
            // "default" and buf-type traits are supported but need to be in
            // custom_traits for runtime processing via ApplyVarTrait.
            let is_buf_trait = matches!(
                trait_name.as_str(),
                "Buf"
                    | "Blob"
                    | "buf"
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
            );
            // `my @a is List` creates an immutable List container in Raku.
            // Keep this trait so runtime can enforce readonly assignment.
            let is_list_trait = is_array && trait_name == "List";
            // Always include uppercase-starting traits in custom_traits for runtime
            // processing. This covers both known types (Buf, List) and user-defined
            // classes (e.g. `my @a is MyArray` where MyArray is Array[Str]).
            let is_uppercase_start = trait_name
                .chars()
                .next()
                .is_some_and(|c| c.is_ascii_uppercase());
            let include_in_traits = !is_builtin
                || trait_name == "default"
                || is_buf_trait
                || is_list_trait
                || is_uppercase_start;
            // Parse optional trait argument: (expr)
            if let Some(r3) = r2.strip_prefix('(') {
                let (r3, _) = ws(r3)?;
                let (r3, trait_arg) = expression(r3)?;
                let (r3, _) = ws(r3)?;
                let (r3, _) = parse_char(r3, ')')?;
                let (r3, _) = ws(r3)?;
                if include_in_traits {
                    custom_traits.push((trait_name.clone(), Some(trait_arg)));
                }
                r = r3;
            } else {
                if include_in_traits {
                    custom_traits.push((trait_name.clone(), None));
                }
                r = r2;
            }
        }
        r
    };

    // Parse `will leave { ... }` trait — desugared into a LEAVE phaser.
    let will_leave_body: Option<Vec<Stmt>> = if let Some(after_will) = keyword("will", rest) {
        let (r, _) = ws1(after_will)?;
        let (r, phaser_name) = ident(r)?;
        if phaser_name != "leave" {
            return Err(PError::expected("'leave' after 'will'"));
        }
        let (r, _) = ws(r)?;
        let (r, body) = super::block(r)?;
        rest = r;
        let (rest2, _) = ws(rest)?;
        rest = rest2;
        Some(body)
    } else {
        None
    };

    // Postfix container typing: my @a of Int; my %h of Int;
    if let Some(after_of) = keyword("of", rest) {
        let (r, _) = ws1(after_of)?;
        let (r, tc) = parse_type_constraint_expr(r).ok_or_else(|| PError::expected("type"))?;
        let (r, _) = ws(r)?;
        type_constraint = Some(tc);
        rest = r;
    }
    // Parse `is` traits that come after `of` (e.g. `my $a of Int is default("foo")`)
    {
        let mut r = rest;
        while let Some(after_is) = keyword("is", r) {
            let (r2, _) = ws1(after_is)?;
            let (r2, trait_name) = ident(r2)?;
            let is_builtin = is_supported_variable_trait(&trait_name);
            let include_in_traits = !is_builtin || trait_name == "default";
            let (r2, _) = ws(r2)?;
            if let Some(r3) = r2.strip_prefix('(') {
                let (r3, _) = ws(r3)?;
                let (r3, trait_arg) = expression(r3)?;
                let (r3, _) = ws(r3)?;
                let (r3, _) = parse_char(r3, ')')?;
                let (r3, _) = ws(r3)?;
                if include_in_traits {
                    custom_traits.push((trait_name.clone(), Some(trait_arg)));
                }
                r = r3;
            } else {
                if include_in_traits {
                    custom_traits.push((trait_name.clone(), None));
                }
                r = r2;
            }
        }
        rest = r;
    }
    if is_hash {
        type_constraint = match (type_constraint, hash_key_constraint.take()) {
            (Some(value_tc), Some(key_tc)) => Some(format!("{}{{{}}}", value_tc, key_tc)),
            (None, Some(key_tc)) => Some(format!("Any{{{}}}", key_tc)),
            (value_tc, None) => value_tc,
        };
    }

    // Optional `where` constraint on variable: my $x where * > 0
    let where_constraint = if let Some(r) = keyword("where", rest) {
        let (r, _) = ws1(r)?;
        let (r, pred) = expression(r)?;
        let (r, _) = ws(r)?;
        rest = r;
        Some(Box::new(pred))
    } else {
        None
    };

    // Feed initialization: my @a <== expr / my @a <<== expr
    if let Some(rest) = rest
        .strip_prefix("<==")
        .or_else(|| rest.strip_prefix("<<=="))
    {
        let (rest, _) = ws(rest)?;
        let (rest, mut expr) = parse_assign_expr_or_comma(rest)?;
        if is_array {
            expr = Expr::Call {
                name: Symbol::intern("__mutsu_feed_array_assign"),
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
            where_constraint: where_constraint.clone(),
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
            where_constraint: where_constraint.clone(),
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
            where_constraint: where_constraint.clone(),
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
        // Scalar declarations stop at comma (my $x = 1, 2 → $x gets 1).
        // Array/hash declarations consume the full comma list (my @a = 1, 2 → @a gets [1, 2]).
        let (rest, expr) = if is_array || is_hash {
            parse_assign_expr_or_comma(rest)?
        } else {
            expression(rest)?
        };
        // For shaped array declarations with assignment (e.g. my @b[3] = <a b c>),
        // create a shaped array and populate it with the assigned data.
        let expr = if let Some(dims) = shape_dims {
            shaped_array_new_with_data_expr(dims, expr)
        } else {
            expr
        };
        let var_name_for_leave = name.clone();
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
            where_constraint: where_constraint.clone(),
        };
        if let Some(stmt) = rewrite_decl_assignment_or_chain(expr.clone(), base_stmt) {
            let stmt = wrap_with_will_leave(stmt, &var_name_for_leave, will_leave_body.clone());
            // Handle comma-separated declarations: my $x = 1, my $y = 2
            let (rest, stmt) = parse_comma_chained_decls(rest, stmt)?;
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
            where_constraint: where_constraint.clone(),
        };
        let stmt = wrap_with_will_leave(stmt, &var_name_for_leave, will_leave_body.clone());
        // Handle comma-separated declarations: my $x = 1, my $y = 2
        let (rest, stmt) = parse_comma_chained_decls(rest, stmt)?;
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
        // Parse optional args (parenthesized or colon-form)
        let (rest, args) = if rest.starts_with('(') {
            let (r, _) = parse_char(rest, '(')?;
            let (r, _) = ws(r)?;
            let (r, args) = super::super::primary::parse_call_arg_list(r)?;
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, ')')?;
            (r, args)
        } else if rest.starts_with(':') && !rest.starts_with("::") {
            // Colon-arg syntax: .=method: arg, arg2
            let r = &rest[1..];
            let (r, _) = ws(r)?;
            let (r, first_arg) = parse_colon_method_arg(r)?;
            let mut args = vec![first_arg];
            let mut r_inner = r;
            loop {
                let (r2, _) = ws(r_inner)?;
                // Adjacent colonpairs without comma
                if r2.starts_with(':')
                    && !r2.starts_with("::")
                    && let Ok((r3, arg)) = crate::parser::primary::misc::colonpair_expr(r2)
                {
                    args.push(arg);
                    r_inner = r3;
                    continue;
                }
                if !r2.starts_with(',') {
                    break;
                }
                let r2 = &r2[1..];
                let (r2, _) = ws(r2)?;
                // Handle trailing comma before ';' or '}'
                if r2.starts_with(';') || r2.starts_with('}') || r2.is_empty() {
                    r_inner = r2;
                    break;
                }
                let (r2, next) = parse_colon_method_arg(r2)?;
                args.push(next);
                r_inner = r2;
            }
            (r_inner, args)
        } else {
            (rest, Vec::new())
        };
        // Build: Type.method(args) — use the type constraint as the target
        let target_name = type_constraint.clone().unwrap_or_else(|| name.clone());
        let expr = Expr::MethodCall {
            target: Box::new(Expr::BareWord(target_name)),
            name: Symbol::intern(&method_name),
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
            where_constraint: where_constraint.clone(),
        };
        // Handle trailing comma list: `my Type $x .= method(), expr, ...`
        // In Raku, the comma creates a sink list — the declaration is evaluated,
        // then the remaining expressions are evaluated and discarded.
        let (rest, _) = ws(rest)?;
        if rest.starts_with(',') && !rest.starts_with(",,") {
            let (r, _) = parse_char(rest, ',')?;
            let (r, _) = ws(r)?;
            let mut sink_stmts = vec![stmt];
            if !r.starts_with(';') && !r.is_empty() && !r.starts_with('}') {
                let (mut r_inner, first_sink) = expression(r)?;
                sink_stmts.push(Stmt::Expr(first_sink));
                loop {
                    let (r2, _) = ws(r_inner)?;
                    if !r2.starts_with(',') || r2.starts_with(",,") {
                        r_inner = r2;
                        break;
                    }
                    let (r2, _) = parse_char(r2, ',')?;
                    let (r2, _) = ws(r2)?;
                    if r2.starts_with(';') || r2.is_empty() || r2.starts_with('}') {
                        r_inner = r2;
                        break;
                    }
                    let (r2, next_sink) = expression(r2)?;
                    sink_stmts.push(Stmt::Expr(next_sink));
                    r_inner = r2;
                }
                let combined = Stmt::SyntheticBlock(sink_stmts);
                if apply_modifier {
                    return parse_statement_modifier(r_inner, combined);
                }
                return Ok((r_inner, combined));
            }
            let combined = Stmt::SyntheticBlock(sink_stmts);
            if apply_modifier {
                return parse_statement_modifier(r, combined);
            }
            return Ok((r, combined));
        }
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
                name: Symbol::intern(name),
                args: vec![arg_expr],
            };
            rest = r_after;
        }
        let bound_name = name.clone();
        let mark_scalar_readonly =
            !is_array && !bound_name.starts_with('%') && scalar_binding_rhs_is_readonly(&expr);
        let bind_to_var = matches!(expr, Expr::Var(_));
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
            where_constraint: where_constraint.clone(),
        };
        let stmt = if is_array || bound_name.starts_with('%') {
            let mut stmts = Vec::new();
            if bound_name.starts_with('%') {
                stmts.push(Stmt::MarkReadonly(bound_name.clone()));
                // Add MarkBind so the compiler emits MarkBindContext,
                // preserving the container identity (e.g. Map stays Map).
                stmts.push(Stmt::MarkBind);
            }
            stmts.push(stmt);
            if is_array {
                stmts.push(Stmt::Expr(Expr::Call {
                    name: Symbol::intern("__mutsu_record_bound_array_len"),
                    args: vec![Expr::Literal(Value::str(bound_name.clone()))],
                }));
                stmts.push(Stmt::Expr(Expr::Call {
                    name: Symbol::intern("__mutsu_record_shaped_array_dims"),
                    args: vec![Expr::Literal(Value::str(bound_name.clone()))],
                }));
            }
            Stmt::SyntheticBlock(stmts)
        } else if mark_scalar_readonly {
            Stmt::SyntheticBlock(vec![Stmt::MarkReadonly(bound_name), stmt])
        } else if bind_to_var {
            // Non-readonly `:=` bind to a variable (e.g. `my $y := $x`):
            // wrap in SyntheticBlock with MarkBind so the compiler emits
            // alias metadata for `=:=` identity checks.
            Stmt::SyntheticBlock(vec![Stmt::MarkBind, stmt])
        } else {
            // Non-readonly `:=` bind to an expression (e.g. `my $proxy := Proxy.new(...)`)
            stmt
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
    let stmt = Stmt::VarDecl {
        name: name.clone(),
        expr,
        type_constraint,
        is_state,
        is_our,
        is_dynamic: has_dynamic_trait,
        is_export: has_export_trait,
        export_tags,
        custom_traits,
        where_constraint: where_constraint.clone(),
    };
    let stmt = wrap_with_will_leave(stmt, &name, will_leave_body);
    Ok((rest, stmt))
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
    /// Per-variable default value (e.g. `$x = 5` inside grouped declaration)
    default: Option<Expr>,
    /// Type constraint for this particular variable (e.g. `Foo $d`)
    per_var_type_constraint: Option<String>,
    /// Where constraint (e.g. `$a where 2`)
    where_constraint: Option<Expr>,
    /// Whether this is a sigilless variable (\c)
    sigilless: bool,
    /// Literal match value (e.g. `"foo"`)
    literal_value: Option<Expr>,
}

pub(super) fn parse_destructuring_decl(
    input: &str,
    is_state: bool,
    type_constraint: Option<String>,
) -> PResult<'_, Stmt> {
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

        // Try to parse a type constraint before the variable (e.g. `Foo $d`)
        let mut per_var_type_constraint = None;
        if let Some((after_tc, tc)) = parse_decl_type_constraint(r) {
            let (after_tc_ws, _) = ws(after_tc)?;
            // Only treat as type if followed by a sigil or sigilless backslash
            if after_tc_ws.starts_with('$')
                || after_tc_ws.starts_with('@')
                || after_tc_ws.starts_with('%')
                || after_tc_ws.starts_with('&')
                || after_tc_ws.starts_with('\\')
            {
                per_var_type_constraint = Some(tc);
                r = after_tc_ws;
            }
        }

        // Sigilless variable: \c or \name
        if let Some(after_backslash) = r.strip_prefix('\\') {
            let (r2, name) = ident(after_backslash)?;
            register_term_symbol_from_decl_name(&name);
            let (r2, _) = ws(r2)?;
            // Parse optional where constraint
            let (r2, where_constraint) = if keyword("where", r2).is_some() {
                let r3 = keyword("where", r2).unwrap();
                let (r3, _) = ws1(r3)?;
                let (r3, expr) = expression(r3)?;
                (r3, Some(expr))
            } else {
                (r2, None)
            };
            let (r2, _) = ws(r2)?;
            vars.push(DestructureVar {
                name,
                is_slurpy,
                is_optional: false,
                is_named,
                default: None,
                per_var_type_constraint,
                where_constraint,
                sigilless: true,
                literal_value: None,
            });
            if r2.starts_with(',') {
                let (r2, _) = parse_char(r2, ',')?;
                let (r2, _) = ws(r2)?;
                r = r2;
            } else {
                r = r2;
            }
            continue;
        }

        // Literal value: "foo" or 'bar' — acts as a match constraint
        if r.starts_with('"') || r.starts_with('\'') {
            let (r2, lit_expr) = expression(r)?;
            let (r2, _) = ws(r2)?;
            let anon_name = format!("__literal_match_{}", vars.len());
            vars.push(DestructureVar {
                name: anon_name,
                is_slurpy: false,
                is_optional: false,
                is_named: false,
                default: None,
                per_var_type_constraint: None,
                where_constraint: None,
                sigilless: false,
                literal_value: Some(lit_expr),
            });
            if r2.starts_with(',') {
                let (r2, _) = parse_char(r2, ',')?;
                let (r2, _) = ws(r2)?;
                r = r2;
            } else {
                r = r2;
            }
            continue;
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

            // Parse optional where constraint: $a where 2
            let (r2, where_constraint) = if keyword("where", r2).is_some() {
                let r3 = keyword("where", r2).unwrap();
                let (r3, _) = ws1(r3)?;
                let (r3, expr) = expression(r3)?;
                (r3, Some(expr))
            } else {
                (r2, None)
            };
            let (r2, _) = ws(r2)?;

            // Check for per-variable default value: ($x = 5)
            let (r2, default) =
                if r2.starts_with('=') && !r2.starts_with("==") && !r2.starts_with("=>") {
                    let r3 = &r2[1..];
                    let (r3, _) = ws(r3)?;
                    let (r3, expr) = expression(r3)?;
                    (r3, Some(expr))
                } else {
                    (r2, None)
                };
            let (r2, _) = ws(r2)?;

            vars.push(DestructureVar {
                name: full_name,
                is_slurpy,
                is_optional,
                is_named,
                default,
                per_var_type_constraint,
                where_constraint,
                sigilless: false,
                literal_value: None,
            });

            if r2.starts_with(',') {
                let (r2, _) = parse_char(r2, ',')?;
                let (r2, _) = ws(r2)?;
                r = r2;
            } else {
                r = r2;
            }
        } else {
            return Err(PError::expected(
                "variable sigil ($, @, %, &), sigilless (\\name), or literal",
            ));
        }
    }
    let (rest, _) = parse_char(r, ')')?;
    let (rest, _) = ws(rest)?;

    // Parse optional `is default(expr)` trait on grouped declaration
    let mut rest = rest;
    let mut group_default_expr: Option<Expr> = None;
    if let Some(r) = keyword("is", rest)
        && let Ok((r, _)) = ws1(r)
        && let Some(r) = keyword("default", r)
    {
        let (r, _) = ws(r)?;
        if let Some(inner) = r.strip_prefix('(') {
            let (inner, _) = ws(inner)?;
            let (inner, default_expr) = expression(inner)?;
            let (inner, _) = ws(inner)?;
            let inner = inner
                .strip_prefix(')')
                .ok_or_else(|| PError::expected("closing paren in is default"))?;
            group_default_expr = Some(default_expr);
            let (r2, _) = ws(inner)?;
            rest = r2;
        }
    }

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
                name: Symbol::intern("list"),
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
                where_constraint: None,
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
                        index: Box::new(Expr::Literal(Value::str(bare_name.to_string()))),
                    },
                    type_constraint: type_constraint.clone(),
                    is_state,
                    is_our: false,
                    is_dynamic: false,
                    is_export: false,
                    export_tags: Vec::new(),
                    custom_traits: Vec::new(),
                    where_constraint: None,
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
            where_constraint: None,
        }];
        let has_explicit_slurpy = vars.iter().any(|v| v.is_slurpy);
        for (i, dvar) in vars.iter().enumerate() {
            // Literal match values just consume a slot — no variable declaration
            if dvar.literal_value.is_some() {
                // TODO: enforce that the RHS value at this position matches the literal
                continue;
            }

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
            // Use per-variable type constraint if present, else fall back to
            // the outer type constraint from `my Type (...)`.
            let effective_tc = dvar
                .per_var_type_constraint
                .clone()
                .or_else(|| type_constraint.clone());
            let effective_where = dvar.where_constraint.clone().map(Box::new);
            stmts.push(Stmt::VarDecl {
                name: dvar.name.clone(),
                expr,
                type_constraint: effective_tc,
                is_state,
                is_our: false,
                is_dynamic: false,
                is_export: false,
                export_tags: Vec::new(),
                custom_traits: Vec::new(),
                where_constraint: effective_where,
            });
            // For sigilless variables, mark as readonly
            if dvar.sigilless {
                stmts.push(Stmt::MarkSigillessReadonly(dvar.name.clone()));
            }
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
        let expr = if let Some(ref def_expr) = group_default_expr {
            dvar.default.clone().unwrap_or_else(|| def_expr.clone())
        } else {
            dvar.default.clone().unwrap_or(Expr::Literal(Value::Nil))
        };
        let traits = if let Some(ref def_expr) = group_default_expr {
            vec![("default".to_string(), Some(def_expr.clone()))]
        } else {
            Vec::new()
        };
        stmts.push(Stmt::VarDecl {
            name: dvar.name.clone(),
            expr,
            type_constraint: type_constraint.clone(),
            is_state,
            is_our: false,
            is_dynamic: false,
            is_export: false,
            export_tags: Vec::new(),
            custom_traits: traits,
            where_constraint: None,
        });
    }
    Ok((rest, Stmt::SyntheticBlock(stmts)))
}

/// Parse parenthesized list form of `has`: `has ($a, $.b, $!c)`
/// Desugars into a SyntheticBlock containing multiple HasDecl statements.
fn has_decl_list(input: &str) -> PResult<'_, Stmt> {
    let (mut rest, _) = parse_char(input, '(')?;
    let mut stmts = Vec::new();
    loop {
        let (r, _) = ws(rest)?;
        rest = r;
        if rest.starts_with(')') {
            rest = &rest[1..];
            break;
        }
        // Parse sigil
        let sigil = rest.as_bytes().first().copied().unwrap_or(0);
        if sigil != b'$' && sigil != b'@' && sigil != b'%' {
            return Err(PError::expected("sigil ($, @, %) in has list"));
        }
        rest = &rest[1..];
        // Parse optional twigil (. or !)
        let (r, is_public, is_alias) = if let Some(stripped) = rest.strip_prefix('.') {
            (stripped, true, false)
        } else if let Some(stripped) = rest.strip_prefix('!') {
            (stripped, false, false)
        } else {
            (rest, false, true)
        };
        rest = r;
        // Parse name
        let (r, name) = take_while1(rest, |c: char| c.is_alphanumeric() || c == '_' || c == '-')?;
        let name = name.to_string();
        rest = r;
        stmts.push(Stmt::HasDecl {
            name: Symbol::intern(&name),
            is_public,
            default: None,
            handles: Vec::new(),
            is_rw: false,
            is_readonly: false,
            type_constraint: None,
            type_smiley: None,
            is_required: None,
            sigil: sigil as char,
            where_constraint: None,
            is_alias,
            is_our: false,
            is_my: false,
            is_default: None,
        });
        let (r, _) = ws(rest)?;
        rest = r;
        // Expect comma or closing paren
        if rest.starts_with(',') {
            rest = &rest[1..];
        }
    }
    let (mut rest, _) = ws(rest)?;

    // Parse optional `is default(expr)` trait on grouped has declaration
    if let Some(r) = keyword("is", rest)
        && let Ok((r, _)) = ws1(r)
        && let Some(r) = keyword("default", r)
    {
        let (r, _) = ws(r)?;
        if let Some(inner) = r.strip_prefix('(') {
            let (inner, _) = ws(inner)?;
            let (inner, default_expr) = expression(inner)?;
            let (inner, _) = ws(inner)?;
            let inner = inner
                .strip_prefix(')')
                .ok_or_else(|| PError::expected("closing paren in is default"))?;
            // Apply the default to all attributes in the list
            for stmt in &mut stmts {
                if let Stmt::HasDecl {
                    default,
                    is_default,
                    ..
                } = stmt
                {
                    *default = Some(default_expr.clone());
                    *is_default = Some(default_expr.clone());
                }
            }
            let (r2, _) = ws(inner)?;
            rest = r2;
        }
    }

    let (rest, _) = opt_char(rest, ';');
    Ok((rest, Stmt::SyntheticBlock(stmts)))
}

/// Parse `has` attribute declaration.
pub(super) fn has_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("has", input).ok_or_else(|| PError::expected("has declaration"))?;
    let (rest, _) = ws1(rest)?;

    // Handle parenthesized list form: has ($a, $.b, $!c)
    // This desugars into a Block containing multiple HasDecl statements.
    if rest.starts_with('(') {
        return has_decl_list(rest);
    }

    // Optional type constraint.
    let (rest, mut type_constraint, type_smiley) = {
        let saved = rest;
        if let Some((r, tc)) = parse_decl_type_constraint(rest) {
            let (r2, _) = ws(r)?;
            if r2.starts_with('$') || r2.starts_with('@') || r2.starts_with('%') {
                // Extract smiley suffix and strip it for the type_constraint name
                let (base, smiley) = if let Some(b) = tc.strip_suffix(":D") {
                    (b.to_string(), Some("D".to_string()))
                } else if let Some(b) = tc.strip_suffix(":U") {
                    (b.to_string(), Some("U".to_string()))
                } else if let Some(b) = tc.strip_suffix(":_") {
                    (b.to_string(), Some("_".to_string()))
                } else {
                    (tc.to_string(), None)
                };
                (r2, Some(base), smiley)
            } else {
                (saved, None, None)
            }
        } else {
            (saved, None, None)
        }
    };

    let sigil = rest.as_bytes().first().copied().unwrap_or(0);
    let (rest, _) = if sigil == b'$' || sigil == b'@' || sigil == b'%' {
        (&rest[1..], ())
    } else {
        return Err(PError::expected("sigil ($, @, %)"));
    };

    // Check for public accessor marker '.'
    let (rest, is_public, is_alias) = if let Some(stripped) = rest.strip_prefix('.') {
        (stripped, true, false)
    } else if let Some(stripped) = rest.strip_prefix('!') {
        (stripped, false, false)
    } else {
        // No twigil: `has $x` creates a private attribute with an alias
        (rest, false, true)
    };

    let (rest, name) = take_while1(rest, |c: char| c.is_alphanumeric() || c == '_' || c == '-')?;
    let name = name.to_string();

    // Optional shaped-array declaration suffix: has @.a[3, 3]
    let (rest, shape_dims) = if sigil == b'@' && rest.starts_with('[') {
        let (r, dims) = parse_array_shape_suffix(rest)?;
        (r, Some(dims))
    } else {
        (rest, None)
    };

    let (mut rest, _) = ws(rest)?;

    // `is` traits (may have multiple: `is rw is required`)
    // Traits come before default value: `has $.x is rw = 42`
    let mut is_rw = false;
    let mut is_readonly = false;
    let mut is_required: Option<Option<String>> = None;
    let mut is_default_trait: Option<Expr> = None;
    while let Some(r) = keyword("is", rest) {
        let (r, _) = ws1(r)?;
        let (r, trait_name) = ident(r)?;
        if trait_name == "rw" {
            is_rw = true;
        } else if trait_name == "readonly" {
            is_readonly = true;
        } else if trait_name == "default" {
            // `is default(expr)` — set the attribute's default value
            let (r_ws, _) = ws(r)?;
            if let Some(inner) = r_ws.strip_prefix('(') {
                let (inner, _) = ws(inner)?;
                let (inner, default_expr) = expression(inner)?;
                let (inner, _) = ws(inner)?;
                let inner = inner
                    .strip_prefix(')')
                    .ok_or_else(|| PError::expected("closing paren in is default"))?;
                is_default_trait = Some(default_expr);
                let (r2, _) = ws(inner)?;
                rest = r2;
                continue;
            }
            // `is default` without parens — just ignore (no-op)
        } else if trait_name == "required" {
            // Check for optional reason: `is required("reason")`
            let (r_ws, _) = ws(r)?;
            if let Some(inner) = r_ws.strip_prefix('(') {
                let (inner, _) = ws(inner)?;
                // Parse string literal for the reason
                if let Some(double_quoted) = inner.strip_prefix('"') {
                    let end = double_quoted
                        .find('"')
                        .ok_or_else(|| PError::expected("closing quote in is required reason"))?;
                    let reason = double_quoted[..end].to_string();
                    let after = &double_quoted[end + 1..];
                    let (after, _) = ws(after)?;
                    let after = after
                        .strip_prefix(')')
                        .ok_or_else(|| PError::expected("closing paren in is required"))?;
                    is_required = Some(Some(reason));
                    rest = after;
                    let (r2, _) = ws(rest)?;
                    rest = r2;
                    continue;
                } else if let Some(single_quoted) = inner.strip_prefix('\'') {
                    let end = single_quoted
                        .find('\'')
                        .ok_or_else(|| PError::expected("closing quote in is required reason"))?;
                    let reason = single_quoted[..end].to_string();
                    let after = &single_quoted[end + 1..];
                    let (after, _) = ws(after)?;
                    let after = after
                        .strip_prefix(')')
                        .ok_or_else(|| PError::expected("closing paren in is required"))?;
                    is_required = Some(Some(reason));
                    rest = after;
                    let (r2, _) = ws(rest)?;
                    rest = r2;
                    continue;
                }
            }
            is_required = Some(None);
        }
        let (r, _) = ws(r)?;
        rest = r;
    }

    // `handles` trait, e.g. `has $.x handles <a b>`
    let mut handles = Vec::new();
    while let Some(r) = keyword("handles", rest) {
        let (r, _) = ws1(r)?;
        parse_handle_specs(r, &mut handles, &mut rest)?;
        let (r, _) = ws(rest)?;
        rest = r;
    }

    // Postfix container typing: has @.a of Int; has %.h of Str;
    if (sigil == b'@' || sigil == b'%')
        && type_constraint.is_none()
        && let Some(r) = keyword("of", rest)
    {
        let (r, _) = ws1(r)?;
        let (r, tc) = parse_type_constraint_expr(r).ok_or_else(|| PError::expected("type"))?;
        let (r, _) = ws(r)?;
        type_constraint = Some(tc);
        rest = r;
    }

    // Optional `where` constraint
    let (rest, where_constraint) = if let Some(r) = keyword("where", rest) {
        let (r, _) = ws1(r)?;
        let (r, pred) = expression(r)?;
        let (r, _) = ws(r)?;
        (r, Some(Box::new(pred)))
    } else {
        (rest, None)
    };

    // Default value
    let (rest, mut default) = if let Some(stripped) = rest.strip_prefix(".=") {
        let (rest, _) = ws(stripped)?;
        let (rest, method_name) =
            take_while1(rest, |c: char| c.is_alphanumeric() || c == '_' || c == '-')?;
        let method_name = method_name.to_string();
        let (rest, args) = if rest.starts_with('(') {
            let (r, _) = parse_char(rest, '(')?;
            let (r, _) = ws(r)?;
            let (r, args) = super::super::primary::parse_call_arg_list(r)?;
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, ')')?;
            (r, args)
        } else if rest.starts_with(':') && !rest.starts_with("::") {
            let r = &rest[1..];
            let (r, _) = ws(r)?;
            let (r, first_arg) = parse_colon_method_arg(r)?;
            let mut args = vec![first_arg];
            let mut r_inner = r;
            loop {
                let (r2, _) = ws(r_inner)?;
                if r2.starts_with(':')
                    && !r2.starts_with("::")
                    && let Ok((r3, arg)) = crate::parser::primary::misc::colonpair_expr(r2)
                {
                    args.push(arg);
                    r_inner = r3;
                    continue;
                }
                if !r2.starts_with(',') {
                    break;
                }
                let r2 = &r2[1..];
                let (r2, _) = ws(r2)?;
                // Handle trailing comma before ';' or '}'
                if r2.starts_with(';') || r2.starts_with('}') || r2.is_empty() {
                    r_inner = r2;
                    break;
                }
                let (r2, next) = parse_colon_method_arg(r2)?;
                args.push(next);
                r_inner = r2;
            }
            (r_inner, args)
        } else {
            (rest, Vec::new())
        };
        let target_name = type_constraint.clone().unwrap_or_else(|| name.clone());
        let target_expr = if target_name == "::?CLASS" {
            Expr::Var("?CLASS".to_string())
        } else if target_name == "::?ROLE" {
            Expr::Var("?ROLE".to_string())
        } else {
            Expr::BareWord(target_name)
        };
        (
            rest,
            Some(Expr::MethodCall {
                target: Box::new(target_expr),
                name: Symbol::intern(&method_name),
                args,
                modifier: None,
                quoted: false,
            }),
        )
    } else if rest.starts_with('=') && !rest.starts_with("==") {
        let rest = &rest[1..];
        let (rest, _) = ws(rest)?;
        let (rest, expr) = expression(rest)?;
        // For @ and % sigils, parse comma-separated list of expressions
        if (sigil == b'@' || sigil == b'%') && rest.starts_with(',') {
            let mut items = vec![expr];
            let mut r = rest;
            while r.starts_with(',') {
                r = &r[1..];
                let (r2, _) = ws(r)?;
                let (r2, next_expr) = expression(r2)?;
                items.push(next_expr);
                r = r2;
            }
            (r, Some(Expr::ArrayLiteral(items)))
        } else {
            (rest, Some(expr))
        }
    } else if let Some(default_expr) = is_default_trait.clone() {
        // `is default(expr)` was used — apply it as the default value
        (rest, Some(default_expr))
    } else {
        (rest, None)
    };

    // Track whether user provided an explicit default (before auto-default)
    let has_explicit_default = default.is_some();

    // Apply `use attributes :D/:U/:_` pragma if no explicit smiley on the type
    let smiley_from_pragma = type_smiley.is_none() && type_constraint.is_some() && {
        let pragma = super::simple::current_attributes_pragma();
        matches!(pragma.as_str(), ":D" | ":U")
    };
    let type_smiley = if type_smiley.is_none() && type_constraint.is_some() {
        let pragma = super::simple::current_attributes_pragma();
        match pragma.as_str() {
            ":D" => Some("D".to_string()),
            ":U" => Some("U".to_string()),
            _ => type_smiley, // ":_" or empty - no change
        }
    } else {
        type_smiley
    };

    // Enforce type smiley constraints at parse time
    let effective_smiley = type_smiley.as_deref().unwrap_or("_");
    if effective_smiley == "D" && !has_explicit_default && is_required.is_none() {
        // :D attribute without a default or `is required` → X::Syntax::Variable::MissingInitializer
        let twigil = if is_public { "." } else { "!" };
        let tc_display = if let Some(ref tc) = type_constraint {
            format!("{}:D", tc)
        } else {
            "Any:D".to_string()
        };
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("type".to_string(), Value::str(tc_display));
        attrs.insert("name".to_string(), Value::str(format!("$!{}", name)));
        if smiley_from_pragma {
            attrs.insert(
                "implicit".to_string(),
                Value::str(":D by pragma".to_string()),
            );
        }
        let ex = Value::make_instance(
            Symbol::intern("X::Syntax::Variable::MissingInitializer"),
            attrs,
        );
        return Err(PError::fatal_with_exception(
            format!(
                "Variable '{}{}{}' of type '{}:D' must be initialized",
                sigil as char,
                twigil,
                name,
                type_constraint.as_deref().unwrap_or("Any")
            ),
            Box::new(ex),
        ));
    }

    // Auto-default: typed scalar attribute with no explicit default → use type object
    // But not when `is required` — the attribute must be explicitly provided
    // For @ and % sigils, the type constraint is an element type, not the
    // container type, so we don't set a default (empty container is used).
    if !has_explicit_default
        && is_required.is_none()
        && sigil == b'$'
        && let Some(ref tc) = type_constraint
    {
        // ::?CLASS / ::?ROLE resolve via the compile-time variable, not as a bare word
        if tc == "::?CLASS" {
            default = Some(Expr::Var("?CLASS".to_string()));
        } else if tc == "::?ROLE" {
            default = Some(Expr::Var("?ROLE".to_string()));
        } else {
            default = Some(Expr::BareWord(tc.clone()));
        }
    }

    if sigil == b'@' {
        if let Some(dims) = shape_dims {
            // Shaped array attribute: has @.a[3, 3]
            if let Some(data_expr) = default.take() {
                default = Some(shaped_array_new_with_data_expr(dims, data_expr));
            } else {
                default = Some(shaped_array_new_expr(dims));
            }
        } else if let Some(expr) = default.take() {
            default = Some(match expr {
                Expr::ArrayLiteral(_)
                | Expr::BracketArray(..)
                | Expr::ArrayVar(_)
                | Expr::Var(_)
                | Expr::Index { .. } => expr,
                other => Expr::ArrayLiteral(vec![other]),
            });
        }
    }
    let (rest, _) = ws(rest)?;

    let (rest, _) = opt_char(rest, ';');
    Ok((
        rest,
        Stmt::HasDecl {
            name: Symbol::intern(&name),
            is_public,
            default,
            handles,
            is_rw,
            is_readonly,
            type_constraint,
            type_smiley,
            is_required,
            sigil: sigil as char,
            where_constraint,
            is_alias,
            is_our: false,
            is_my: false,
            is_default: is_default_trait,
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
    let (rest, variants) = if input.starts_with("<<") || input.starts_with('\u{ab}') {
        parse_double_angle_enum_variants(input)?
    } else if input.starts_with('<') {
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
            name: Symbol::intern(""),
            variants,
            is_export: false,
            base_type: None,
            language_version: super::simple::current_language_version(),
        },
    ))
}

/// Parse `<< ... >>` or `\u{ab} ... \u{bb}` enum variant list.
/// Supports plain words and colonpairs like `:key<value>` or `:key[expr, ...]`.
fn parse_double_angle_enum_variants(input: &str) -> PResult<'_, Vec<(String, Option<Expr>)>> {
    let (r, use_unicode_close) = if let Some(r) = input.strip_prefix("<<") {
        (r, false)
    } else if let Some(r) = input.strip_prefix('\u{ab}') {
        // «
        (r, true)
    } else {
        return Err(PError::expected("<< or \u{ab}"));
    };
    let mut variants = Vec::new();
    let mut r = r;
    loop {
        // Skip whitespace (including newlines)
        let (r2, _) = ws(r)?;
        r = r2;
        // Check for closing >> or »
        if use_unicode_close {
            if let Some(r2) = r.strip_prefix('\u{bb}') {
                return Ok((r2, variants));
            }
        } else if let Some(r2) = r.strip_prefix(">>") {
            return Ok((r2, variants));
        }
        // Colonpair: :key<value> or :key[expr, ...] or :!key
        if r.starts_with(':') && !r.starts_with("::") {
            let after_colon = &r[1..];
            // Handle :!key (negated boolean)
            let (after_neg, negated) = if let Some(stripped) = after_colon.strip_prefix('!') {
                (stripped, true)
            } else {
                (after_colon, false)
            };
            // Parse the key (identifier)
            let (after_key, key) = take_while1(after_neg, |c: char| {
                c.is_alphanumeric() || c == '_' || c == '-'
            })?;
            if negated {
                // :!key => value is 0 (false)
                variants.push((key.to_string(), Some(Expr::Literal(Value::Int(0.into())))));
                r = after_key;
            } else if let Some(after_open) = after_key.strip_prefix('<') {
                // :key<value>
                let (after_val, val) = take_while1(after_open, |c: char| c != '>')?;
                let after_close = after_val
                    .strip_prefix('>')
                    .ok_or_else(|| PError::expected(">"))?;
                variants.push((key.to_string(), Some(Expr::Literal(Value::str_from(val)))));
                r = after_close;
            } else if after_key.starts_with('[') {
                // :key[expr, ...] — parse as array expression
                let (after_expr, expr) = expression(after_key)?;
                variants.push((key.to_string(), Some(expr)));
                r = after_expr;
            } else if after_key.starts_with('(') {
                // :key(expr) — parse as expression in parens
                let (after_expr, expr) = expression(after_key)?;
                variants.push((key.to_string(), Some(expr)));
                r = after_expr;
            } else {
                // :key with no value — treat as boolean true (no explicit value)
                variants.push((key.to_string(), None));
                r = after_key;
            }
        } else {
            // Plain word
            let close_char = if use_unicode_close { '\u{bb}' } else { '>' };
            let (r2, word) = take_while1(r, |c: char| {
                !c.is_whitespace() && c != close_char && c != '>' && c != ':'
            })?;
            variants.push((word.to_string(), None));
            r = r2;
        }
    }
}

fn parse_enum_variant_entry(input: &str) -> PResult<'_, (String, Option<Expr>)> {
    let (rest, expr) = expression(input)?;
    match expr {
        Expr::BareWord(name) => Ok((rest, (name, None))),
        Expr::Literal(Value::Str(name)) => Ok((rest, (name.to_string(), None))),
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
                Ok((rest, (name.to_string(), value_expr)))
            }
            _ => Err(PError::expected("enum variant name")),
        },
        _ => Err(PError::expected("enum variant")),
    }
}

pub(super) fn parse_enum_decl_body(input: &str) -> PResult<'_, Stmt> {
    parse_enum_decl_body_with_type(input, None)
}

fn parse_enum_decl_body_with_type(input: &str, base_type: Option<String>) -> PResult<'_, Stmt> {
    let (rest, name_str) = qualified_ident(input)?;
    let name = Symbol::intern(&name_str);
    let (rest, _) = ws(rest)?;

    // Parse `is <trait>` clauses (e.g., `is export`)
    let mut rest = rest;
    let mut is_export = false;
    while let Some(r) = keyword("is", rest) {
        let (r, _) = ws1(r)?;
        let (r, trait_name) = ident(r)?;
        if trait_name == "export" {
            is_export = true;
        }
        let (r, _) = ws(r)?;
        rest = r;
    }

    // Enum variants in << >>, « », <> or ()
    let (rest, variants) = if rest.starts_with("<<") || rest.starts_with('\u{ab}') {
        parse_double_angle_enum_variants(rest)?
    } else if rest.starts_with('<') {
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
        // Handle `(@variable)` — dynamic enum body from a variable
        if r.starts_with('@') {
            let (r2, expr) = expression(r)?;
            let (r2, _) = ws(r2)?;
            if let Some(r2) = r2.strip_prefix(')') {
                return Ok((
                    r2,
                    Stmt::EnumDecl {
                        name,
                        variants: vec![("__DYNAMIC__".to_string(), Some(expr))],
                        is_export,
                        base_type: base_type.clone(),
                        language_version: super::simple::current_language_version(),
                    },
                ));
            }
        }
        let mut variants = Vec::new();
        let mut r = r;
        loop {
            if let Some(r) = r.strip_prefix(')') {
                return Ok((
                    r,
                    Stmt::EnumDecl {
                        name,
                        variants,
                        is_export,
                        base_type: base_type.clone(),
                        language_version: super::simple::current_language_version(),
                    },
                ));
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
    Ok((
        rest,
        Stmt::EnumDecl {
            name,
            variants,
            is_export,
            base_type,
            language_version: super::simple::current_language_version(),
        },
    ))
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
            super::simple::register_compile_time_constant(&name, s.to_string());
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
                custom_traits: vec![("__constant".to_string(), None)],
                where_constraint: None,
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
            custom_traits: vec![("__constant".to_string(), None)],
            where_constraint: None,
        },
    ))
}

/// Parse `subset` declaration.
pub(super) fn subset_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("subset", input).ok_or_else(|| PError::expected("subset declaration"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, name) = qualified_ident(rest)?;
    let (mut rest, _) = ws(rest)?;
    while let Some(r) = keyword("is", rest) {
        let (r, _) = ws1(r)?;
        let (r, trait_name) = ident(r)?;
        let (r, _) = ws(r)?;
        // Keep parsing permissive for subset traits; currently only `is export`
        // has runtime meaning in roast modules.
        if trait_name == "export" {
            rest = skip_balanced_parens(r);
            let (r2, _) = ws(rest)?;
            rest = r2;
        } else {
            rest = r;
        }
    }
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
            name: Symbol::intern(&name),
            base,
            predicate,
            version: super::simple::current_language_version(),
        },
    ))
}

/// Parse a single handle spec item (colon-pair, word list, regex, wildcard, etc.)
fn parse_single_handle_spec<'a>(input: &'a str, specs: &mut Vec<HandleSpec>) -> PResult<'a, ()> {
    let r = input;
    // Colon-pair: :exposed<target> or :exposed('target')
    if let Some(after_colon) = r.strip_prefix(':') {
        let (after_name, name) = take_while1(after_colon, |c: char| {
            c.is_alphanumeric() || c == '_' || c == '-'
        })?;
        if let Some(after_angle) = after_name.strip_prefix('<') {
            // :name<target>
            let end = after_angle
                .find('>')
                .ok_or_else(|| PError::expected("closing > in handles pair"))?;
            let target = after_angle[..end].to_string();
            specs.push(HandleSpec::Rename {
                exposed: name.to_string(),
                target,
            });
            return Ok((&after_angle[end + 1..], ()));
        } else if let Some(after_paren) = after_name.strip_prefix('(') {
            // :name('target')
            let after_paren = after_paren.trim_start();
            if after_paren.starts_with('\'') || after_paren.starts_with('"') {
                let quote = after_paren.as_bytes()[0] as char;
                let after_quote = &after_paren[1..];
                let end = after_quote
                    .find(quote)
                    .ok_or_else(|| PError::expected("closing quote in handles pair"))?;
                let target = after_quote[..end].to_string();
                let after_close_quote = &after_quote[end + 1..];
                let after_close_quote = after_close_quote.trim_start();
                let after_close_paren = after_close_quote
                    .strip_prefix(')')
                    .ok_or_else(|| PError::expected("closing ) in handles pair"))?;
                specs.push(HandleSpec::Rename {
                    exposed: name.to_string(),
                    target,
                });
                return Ok((after_close_paren, ()));
            }
        }
        return Err(PError::expected("handles pair value after colon-pair name"));
    }
    Err(PError::expected("handle spec"))
}

/// Parse handle specifications after the `handles` keyword.
fn parse_handle_specs<'a>(
    input: &'a str,
    specs: &mut Vec<HandleSpec>,
    rest_out: &mut &'a str,
) -> Result<(), PError> {
    let r = input;
    if let Some(r_inner) = r.strip_prefix('<') {
        // Word list: <a b c>
        let mut cursor = r_inner;
        loop {
            let (r_ws, _) = ws(cursor)?;
            cursor = r_ws;
            if let Some(r_end) = cursor.strip_prefix('>') {
                *rest_out = r_end;
                break;
            }
            let (r_name, method_name) = take_while1(cursor, |c: char| {
                c.is_alphanumeric() || c == '_' || c == '-'
            })?;
            specs.push(HandleSpec::Name(method_name.to_string()));
            cursor = r_name;
        }
    } else if let Some(after_star) = r.strip_prefix('*') {
        // Wildcard: *
        specs.push(HandleSpec::Wildcard);
        *rest_out = after_star;
    } else if let Some(after_slash) = r.strip_prefix('/') {
        // Regex: /pattern/
        let end = after_slash
            .find('/')
            .ok_or_else(|| PError::expected("closing / in handles regex"))?;
        let pattern = after_slash[..end].to_string();
        specs.push(HandleSpec::Regex(pattern));
        *rest_out = &after_slash[end + 1..];
    } else if let Some(after_paren) = r.strip_prefix('(') {
        // Parenthesized list: (:name<target>, :name2('target2'))
        let mut cursor = after_paren;
        loop {
            let (r_ws, _) = ws(cursor)?;
            cursor = r_ws;
            if let Some(r_end) = cursor.strip_prefix(')') {
                *rest_out = r_end;
                break;
            }
            // Skip commas
            if let Some(after_comma) = cursor.strip_prefix(',') {
                let (r_ws, _) = ws(after_comma)?;
                cursor = r_ws;
                continue;
            }
            let (after_spec, _) = parse_single_handle_spec(cursor, specs)?;
            cursor = after_spec;
        }
    } else if r.starts_with(':') {
        // Single colon-pair: :exposed<target>
        let (after_spec, _) = parse_single_handle_spec(r, specs)?;
        *rest_out = after_spec;
    } else if r.starts_with('\'') || r.starts_with('"') {
        // Quoted string: 'method' or "method"
        let quote = r.as_bytes()[0] as char;
        let after_open = &r[1..];
        let end = after_open
            .find(quote)
            .ok_or_else(|| PError::expected("closing quote in handles"))?;
        let method_name = &after_open[..end];
        specs.push(HandleSpec::Name(method_name.to_string()));
        *rest_out = &after_open[end + 1..];
    } else {
        // Bare identifier: could be a method name or type name
        let (r_name, method_name) = ident(r)?;
        specs.push(HandleSpec::Name(method_name.to_string()));
        *rest_out = r_name;
    }
    Ok(())
}
