use super::super::super::expr::expression;
use super::super::super::helpers::{skip_balanced_parens, ws, ws1};
use super::super::super::parse_result::{PError, PResult, opt_char, take_while1};
use super::super::{ident, keyword, qualified_ident};
use super::helpers::normalize_language_version;
use crate::ast::{Expr, Stmt};
use crate::symbol::Symbol;
use crate::value::Value;

/// Parse a `use` statement.
pub(in crate::parser::stmt) fn use_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("use", input).ok_or_else(|| PError::expected("use statement"))?;
    let (rest, _) = ws1(rest)?;

    // Handle `use v6`, `use v6.d`, etc.
    // Only match when 'v' is followed by a digit (version number), not other identifiers.
    if rest.starts_with('v') && rest[1..].chars().next().is_some_and(|c| c.is_ascii_digit()) {
        let (r, version_token) = take_while1(rest, |c: char| {
            c.is_alphanumeric() || c == '.' || c == '*' || c == '+'
        })?;
        let version = normalize_language_version(version_token);
        super::super::simple::set_current_language_version(&version);
        let (r, _) = ws(r)?;
        let _ = opt_char(r, ';');
        let (r, _) = opt_char(r, ';');
        return Ok((
            r,
            Stmt::Use {
                module: "v6".to_string(),
                arg: Some(Expr::Literal(Value::str(version))),
                tags: Vec::new(),
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
        let (rest, arg) = super::super::super::primary::colonpair_expr(rest)?;
        let (rest, _) = ws(rest)?;
        let (rest, _) = opt_char(rest, ';');
        return Ok((
            rest,
            Stmt::Use {
                module,
                arg: Some(arg),
                tags: Vec::new(),
            },
        ));
    }

    // Collect adverbs/colonpairs on use (e.g. `use Foo :ALL`, `use Foo :tag1, :tag2`)
    let mut rest = rest;
    let mut use_tags: Vec<String> = Vec::new();
    loop {
        if rest.starts_with(':') && !rest.starts_with("::") {
            let r = &rest[1..];
            // :!name
            let r = r.strip_prefix('!').unwrap_or(r);
            if let Ok((r, tag_name)) = ident(r) {
                use_tags.push(tag_name.to_string());
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
        let (r, expr) = super::super::super::primary::primary(rest)?;
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
        super::super::simple::try_add_parse_time_lib_path(expr);
    }
    // Register exported function names so they are recognized as calls without parens.
    super::super::simple::register_module_exports(&module);
    Ok((
        rest,
        Stmt::Use {
            module,
            arg,
            tags: use_tags,
        },
    ))
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
            tags: Vec::new(),
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
        super::super::simple::set_attributes_pragma(s);
    }
    Ok(result)
}

/// Parse `import Module [:TAG ...];`
pub(in crate::parser::stmt) fn import_stmt(input: &str) -> PResult<'_, Stmt> {
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
    // Register exported operators from inline modules at parse time
    // so that custom infix operators are recognized in expression parsing.
    super::super::simple::import_inline_module_exports(&module);
    Ok((rest, Stmt::Import { module, tags }))
}

/// Parse `need Module;` — load module without importing its exports.
pub(in crate::parser::stmt) fn need_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("need", input).ok_or_else(|| PError::expected("need statement"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, module) = qualified_ident(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, _) = opt_char(rest, ';');
    Ok((rest, Stmt::Need { module }))
}

/// Parse a `no` statement.
pub(in crate::parser::stmt) fn no_stmt(input: &str) -> PResult<'_, Stmt> {
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
        let (r, expr) = super::super::super::primary::primary(rest)?;
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
