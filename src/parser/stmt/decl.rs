use super::super::expr::expression;
use super::super::helpers::{skip_balanced_parens, ws, ws1};
use super::super::parse_result::{PError, PResult, opt_char, parse_char, take_while1};

use crate::ast::{Expr, Stmt};
use crate::value::Value;

use super::{
    class::{module_decl, package_decl, proto_decl},
    ident, keyword, parse_assign_expr_or_comma, parse_statement_modifier, qualified_ident,
    var_name,
};

use super::super::parse_result::take_while_opt;
use super::{class_decl_body, method_decl_body, parse_comma_or_expr, sub_decl_body};

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

/// Parse `need Module;` — load module without importing its exports.
pub(super) fn need_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("need", input).ok_or_else(|| PError::expected("need statement"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, module) = qualified_ident(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, _) = opt_char(rest, ';');
    Ok((rest, Stmt::Need { module }))
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

    // my sub name(...) { ... }
    if let Some(r) = keyword("sub", rest) {
        let (r, _) = ws1(r)?;
        return sub_decl_body(r, false, false);
    }

    // my method name(...) { ... }
    if let Some(r) = keyword("method", rest) {
        let (r, _) = ws1(r)?;
        return method_decl_body(r, false);
    }

    // my class Name is Parent { ... }
    if let Some(r) = keyword("class", rest) {
        let (r, _) = ws1(r)?;
        return class_decl_body(r);
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
    // my regex Name { ... }
    // Reuse token/regex/rule declaration parsing so `<Name>` works in regexes.
    if keyword("regex", rest).is_some() {
        let (rest, stmt) = super::class::token_decl(rest)?;
        if apply_modifier {
            return parse_statement_modifier(rest, stmt);
        }
        return Ok((rest, stmt));
    }

    // Sigilless variable: my \name = expr
    if let Some(r) = rest.strip_prefix('\\') {
        let (r, name) = ident(r)?;
        let (r, _) = ws(r)?;
        if r.starts_with('=') && !r.starts_with("==") && !r.starts_with("=>") {
            let r = &r[1..];
            let (r, _) = ws(r)?;
            let (r, expr) = expression(r)?;
            let stmt = Stmt::VarDecl {
                name,
                expr,
                type_constraint: None,
                is_state,
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
            },
        ));
    }

    // Optional type constraint: my Int $x or my Str(Match) $x (coercion type)
    let (rest, type_constraint) = {
        // Try to parse a type name followed by a sigil or \
        let saved = rest;
        if let Ok((r, tc)) = ident(rest) {
            // Check for coercion type syntax: Type(FromType)
            let (r, tc) = if let Some(inner) = r.strip_prefix('(') {
                // Parse the coercion type: e.g. Str(Match)
                if let Ok((r2, _from_type)) = ident(inner) {
                    if let Some(after) = r2.strip_prefix(')') {
                        // Successfully parsed Type(FromType) — use the target type
                        (after, tc)
                    } else {
                        (r, tc)
                    }
                } else {
                    (r, tc)
                }
            } else {
                (r, tc)
            };
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

    // Sigilless variable after type: my Int \name = expr
    if let Some(r) = rest.strip_prefix('\\') {
        let (r, name) = ident(r)?;
        let (r, _) = ws(r)?;
        if r.starts_with('=') && !r.starts_with("==") && !r.starts_with("=>") {
            let r = &r[1..];
            let (r, _) = ws(r)?;
            let (r, expr) = expression(r)?;
            let stmt = Stmt::VarDecl {
                name,
                expr,
                type_constraint,
                is_state,
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
                type_constraint,
                is_state,
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

    let (rest, _) = ws(rest)?;

    // Skip `is default(...)` or other `is` traits on variables
    let rest = {
        let mut r = rest;
        while let Some(after_is) = keyword("is", r) {
            let (r2, _) = ws1(after_is)?;
            // Parse trait name
            let (r2, _trait_name) = ident(r2)?;
            let (r2, _) = ws(r2)?;
            // Parse optional trait argument: (expr)
            if let Some(r3) = r2.strip_prefix('(') {
                let (r3, _) = ws(r3)?;
                let mut depth = 1;
                let mut end = 0;
                for (i, c) in r3.char_indices() {
                    match c {
                        '(' => depth += 1,
                        ')' => {
                            depth -= 1;
                            if depth == 0 {
                                end = i;
                                break;
                            }
                        }
                        _ => {}
                    }
                }
                let r3 = &r3[end + 1..];
                let (r3, _) = ws(r3)?;
                r = r3;
            } else {
                r = r2;
            }
        }
        r
    };

    // Assignment
    if rest.starts_with('=') && !rest.starts_with("==") && !rest.starts_with("=>") {
        #[cfg(test)]
        eprintln!("my_decl: taking assignment branch");
        let rest = &rest[1..];
        let (rest, _) = ws(rest)?;
        let (rest, expr) = parse_assign_expr_or_comma(rest)?;
        let stmt = Stmt::VarDecl {
            name,
            expr,
            type_constraint,
            is_state,
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
        };
        let stmt = Stmt::VarDecl {
            name,
            expr,
            type_constraint,
            is_state,
        };
        if apply_modifier {
            return parse_statement_modifier(rest, stmt);
        }
        return Ok((rest, stmt));
    }
    // Binding :=
    if let Some(stripped) = rest.strip_prefix(":=") {
        let (rest, _) = ws(stripped)?;
        let (rest, expr) = parse_assign_expr_or_comma(rest)?;
        let stmt = Stmt::VarDecl {
            name,
            expr,
            type_constraint,
            is_state,
        };
        if apply_modifier {
            return parse_statement_modifier(rest, stmt);
        }
        return Ok((rest, stmt));
    }

    let (rest, _) = opt_char(rest, ';');
    let expr = if is_array {
        Expr::Literal(Value::real_array(Vec::new()))
    } else if is_hash {
        Expr::Hash(Vec::new())
    } else {
        Expr::Literal(Value::Nil)
    };
    Ok((
        rest,
        Stmt::VarDecl {
            name,
            expr,
            type_constraint,
            is_state,
        },
    ))
}

/// Parse destructuring: ($a, $b, $c) = expr
pub(super) fn parse_destructuring_decl(input: &str) -> PResult<'_, Stmt> {
    let (rest, _) = parse_char(input, '(')?;
    let (rest, _) = ws(rest)?;
    let mut names = Vec::new();
    let mut r = rest;
    loop {
        if r.starts_with(')') {
            break;
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
            names.push(format!("{}{}", prefix, n));
            let (r2, _) = ws(r2)?;
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
    if rest.starts_with('=') || rest.starts_with(":=") {
        let rest = if let Some(stripped) = rest.strip_prefix(":=") {
            stripped
        } else {
            &rest[1..]
        };
        let (rest, _) = ws(rest)?;
        let (rest, rhs) = parse_comma_or_expr(rest)?;
        let (rest, _) = ws(rest)?;
        let (rest, _) = opt_char(rest, ';');
        // Desugar into block
        let tmp_name = "@__destructure_tmp__".to_string();
        let array_bare = "__destructure_tmp__".to_string();
        let mut stmts = vec![Stmt::VarDecl {
            name: tmp_name,
            expr: rhs,
            type_constraint: None,
            is_state: false,
        }];
        for (i, var_name) in names.iter().enumerate() {
            stmts.push(Stmt::VarDecl {
                name: var_name.clone(),
                expr: Expr::Index {
                    target: Box::new(Expr::ArrayVar(array_bare.clone())),
                    index: Box::new(Expr::Literal(Value::Int(i as i64))),
                },
                type_constraint: None,
                is_state: false,
            });
        }
        return Ok((rest, Stmt::Block(stmts)));
    }
    // No assignment
    let (rest, _) = ws(rest)?;
    let (rest, _) = opt_char(rest, ';');
    let mut stmts = Vec::new();
    for name in &names {
        stmts.push(Stmt::VarDecl {
            name: name.clone(),
            expr: Expr::Literal(Value::Nil),
            type_constraint: None,
            is_state: false,
        });
    }
    Ok((rest, Stmt::Block(stmts)))
}

/// Parse `has` attribute declaration.
pub(super) fn has_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("has", input).ok_or_else(|| PError::expected("has declaration"))?;
    let (rest, _) = ws1(rest)?;

    // Optional type constraint
    let rest = if let Ok((r, _tc)) = ident(rest) {
        let (r2, _) = ws(r)?;
        if r2.starts_with('$') || r2.starts_with('@') || r2.starts_with('%') {
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

    // Default value
    let (rest, default) = if rest.starts_with('=') && !rest.starts_with("==") {
        let rest = &rest[1..];
        let (rest, _) = ws(rest)?;
        let (rest, expr) = expression(rest)?;
        (rest, Some(expr))
    } else {
        (rest, None)
    };
    let (rest, _) = ws(rest)?;

    let (rest, _) = opt_char(rest, ';');
    Ok((
        rest,
        Stmt::HasDecl {
            name,
            is_public,
            default,
            is_rw,
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
            let (r2, vname) = ident(r)?;
            let (r2, _) = ws(r2)?;
            let (r2, val) = if let Some(stripped) = r2.strip_prefix("=>") {
                let (r2, _) = ws(stripped)?;
                let (r2, expr) = expression(r2)?;
                (r2, Some(expr))
            } else {
                (r2, None)
            };
            variants.push((vname, val));
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
            let (r2, vname) = ident(r)?;
            let (r2, _) = ws(r2)?;
            let (r2, val) = if let Some(stripped) = r2.strip_prefix("=>") {
                let (r2, _) = ws(stripped)?;
                let (r2, expr) = expression(r2)?;
                (r2, Some(expr))
            } else {
                (r2, None)
            };
            variants.push((vname, val));
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
    let (rest, name) = if matches!(sigil, b'$' | b'@' | b'%' | b'&') {
        let prefix = match sigil {
            b'@' => "@",
            b'%' => "%",
            b'&' => "&",
            _ => "",
        };
        let (r, n) = var_name(rest)?;
        (r, format!("{prefix}{n}"))
    } else {
        ident(rest)?
    };
    let (rest, _) = ws(rest)?;
    if rest.starts_with('=') || rest.starts_with(":=") {
        let rest = if let Some(stripped) = rest.strip_prefix(":=") {
            stripped
        } else {
            &rest[1..]
        };
        let (rest, _) = ws(rest)?;
        let (rest, expr) = parse_comma_or_expr(rest)?;
        let (rest, _) = ws(rest)?;
        let (rest, _) = opt_char(rest, ';');
        return Ok((
            rest,
            Stmt::VarDecl {
                name,
                expr,
                type_constraint: None,
                is_state: false,
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
        let (r, base) = ident(r)?;
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
