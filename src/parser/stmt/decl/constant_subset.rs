use super::super::super::expr::expression;
use super::super::super::helpers::{ws, ws1};
use super::super::super::parse_result::{PError, PResult, opt_char};
use super::super::{ident, keyword, qualified_ident, var_name};
use super::helpers::{
    parse_export_trait_tags, parse_sigilless_decl_name, register_term_symbol_from_decl_name,
};
use super::parse_comma_or_expr;
use crate::ast::{Expr, Stmt};
use crate::symbol::Symbol;
use crate::value::Value;

use super::super::sub::parse_type_constraint_expr;

/// Parse `constant` declaration.
pub(in crate::parser::stmt) fn constant_decl(input: &str) -> PResult<'_, Stmt> {
    let rest =
        keyword("constant", input).ok_or_else(|| PError::expected("constant declaration"))?;
    let (rest, _) = ws1(rest)?;
    // `constant ($a, $b) = ...` is a syntax error (X::Syntax::Missing)
    if rest.starts_with('(') {
        return Err(missing_initializer_error());
    }
    // The name can be $var, @var, %var, &var, or bare identifier.
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
    } else if rest.starts_with("term:<") || rest.starts_with("term:\u{ab}") {
        // `constant term:<♥> = "♥"` defines a term named `♥`, resolvable as a
        // bareword. Parse the operator name (`term:<♥>`) and use its inner
        // symbol (`♥`) as the constant's name, exactly as `constant ♥ = ...`
        // would if `♥` were a legal identifier.
        let (r, full) = parse_sigilless_decl_name(rest)?;
        let inner = full
            .strip_prefix("term:<")
            .and_then(|s| s.strip_suffix('>'))
            .unwrap_or(&full)
            .to_string();
        register_term_symbol_from_decl_name(&inner);
        (r, inner)
    } else {
        let (r, n) = ident(rest)?;
        register_term_symbol_from_decl_name(&n);
        (r, n)
    };
    // A constant with a `?` twigil (e.g. `constant $?FILE = ...`) is not
    // implemented; Raku rejects it at compile time with X::Comp::NYI.
    if name.starts_with('?') || name.starts_with("@?") || name.starts_with("%?") {
        return Err(constant_twigil_nyi_error('?'));
    }
    // Record the source sigil so the compiler can distinguish a scalar
    // `constant $x` from a sigilless `constant x` for X::Redeclaration purposes
    // (the VarDecl `name` strips the `$`, so both would otherwise collide).
    let sigil_marker = match sigil {
        b'$' => "$",
        b'@' => "@",
        b'%' => "%",
        b'&' => "&",
        _ => "",
    };
    let constant_traits = vec![
        ("__constant".to_string(), None),
        (
            "__constant_sigil".to_string(),
            Some(Expr::Literal(Value::str(sigil_marker.to_string()))),
        ),
    ];
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
    // .= mutating method call: constant foo .= new => constant foo = Mu.new
    if let Some(stripped) = rest.strip_prefix(".=") {
        let (r, _) = ws(stripped)?;
        let (r, method_name) = crate::parser::parse_result::take_while1(r, |c: char| {
            c.is_alphanumeric() || c == '_' || c == '-'
        })?;
        let method_name = method_name.to_string();
        let r_before_ws = r;
        let (r, _) = ws(r)?;
        // Parse optional args (parenthesized, colon-form, or fake-infix adverbs)
        let (r, args) = if let Some(inner) = r.strip_prefix('(') {
            let r = inner;
            let (r, _) = ws(r)?;
            let (r, args) = crate::parser::primary::parse_call_arg_list(r)?;
            let (r, _) = ws(r)?;
            let r = r.strip_prefix(')').ok_or_else(|| PError::expected(")"))?;
            (r, args)
        } else if r_before_ws.starts_with(':') && !r_before_ws.starts_with("::") {
            super::my_decl_assign::parse_colon_args(r_before_ws)?
        } else if r.starts_with(':') && !r.starts_with("::") {
            super::my_decl_assign::parse_fake_infix_adverbs(r)?
        } else {
            (r, Vec::new())
        };
        let expr = crate::ast::Expr::MethodCall {
            target: Box::new(crate::ast::Expr::BareWord("Mu".to_string())),
            name: crate::symbol::Symbol::intern(&method_name),
            args,
            modifier: None,
            quoted: false,
        };
        let (r, _) = ws(r)?;
        let (r, _) = opt_char(r, ';');
        return Ok((
            r,
            Stmt::VarDecl {
                name,
                expr,
                type_constraint: None,
                is_state: false,
                is_our: true,
                is_dynamic: false,
                is_export,
                export_tags: export_tags.clone(),
                custom_traits: constant_traits.clone(),
                where_constraint: None,
            },
        ));
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
        if let crate::ast::Expr::Literal(v) = &expr
            && let crate::value::ValueView::Str(s) = v.view()
        {
            super::super::simple::register_compile_time_constant(&name, s.to_string());
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
                custom_traits: constant_traits.clone(),
                where_constraint: None,
            },
        ));
    }
    // A `constant` with no `=`/`:=` initializer is a compile-time error
    // (`constant foo;` → X::Syntax::Missing, what => 'initializer').
    Err(missing_initializer_error())
}

/// X::Syntax::Missing for a `constant` declaration that has no initializer.
fn missing_initializer_error() -> PError {
    let msg = "X::Syntax::Missing: Missing initializer on constant declaration".to_string();
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("what".to_string(), Value::str("initializer".to_string()));
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    let ex = Value::make_instance(Symbol::intern("X::Syntax::Missing"), attrs);
    PError::fatal_with_exception(msg, Box::new(ex))
}

/// `constant $?FILE = ...` — a constant declared with a twigil is not yet
/// implemented; Raku rejects it at compile time with X::Comp::NYI.
fn constant_twigil_nyi_error(twigil: char) -> PError {
    let msg = format!(
        "Constants with a '{}' twigil not yet implemented. Sorry.",
        twigil
    );
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("feature".to_string(), Value::str(msg.clone()));
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    let ex = Value::make_instance(Symbol::intern("X::Comp::NYI"), attrs);
    PError::fatal_with_exception(msg, Box::new(ex))
}

/// Parse `subset` declaration.
pub(in crate::parser::stmt) fn subset_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("subset", input).ok_or_else(|| PError::expected("subset declaration"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, name) = qualified_ident(rest)?;
    // Register the subset as a user-declared type so a later `$x ~~ S` parses `S`
    // as a type term, not the `S///` non-destructive-substitution operator
    // (`~~ S/.../.../` is valid, so the bareword `S` after `~~` is otherwise taken
    // as a substitution — subtypes.t 68). Classes/roles/grammars already do this.
    super::super::simple::register_user_type(&name);
    let (mut rest, _) = ws(rest)?;
    let mut is_export = false;
    let mut export_tags: Vec<String> = Vec::new();
    let mut base: Option<String> = None;
    // `of Base` and `is trait` may appear in either order before `where`:
    // `subset S of T is export where …` and `subset S is export of T where …`
    // both occur in the wild (Business::CreditCard uses the former). Loop over
    // `of`/`is` until neither matches, so a trait after `of` is not left
    // unparsed (which stranded the following `where …` as a bare statement).
    loop {
        if base.is_none()
            && let Some(r) = keyword("of", rest)
        {
            let (r, _) = ws1(r)?;
            let (r, b) = parse_type_constraint_expr(r).ok_or_else(|| PError::expected("type"))?;
            let (r, _) = ws(r)?;
            base = Some(b);
            rest = r;
            continue;
        }
        if let Some(r) = keyword("is", rest) {
            let (r, _) = ws1(r)?;
            let (r, trait_name) = ident(r)?;
            if trait_name == "export" {
                is_export = true;
                let (r2, tags) = parse_export_trait_tags(r)?;
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
                let (r2, _) = ws(r2)?;
                rest = r2;
            } else {
                let (r, _) = ws(r)?;
                rest = r;
            }
            continue;
        }
        break;
    }
    let base = base.unwrap_or_else(|| "Any".to_string());
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
            version: super::super::simple::current_language_version(),
            is_export,
            export_tags,
            is_my: false,
        },
    ))
}

/// Monotonic counter naming each anonymous inline subset uniquely.
static ANON_SUBSET_COUNTER: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);

/// Parse an inline `subset` used as a TERM (an expression yielding the subset's
/// type object), e.g. `$x ~~ (subset :: where Int|Str)` or a named
/// `subset Foo where Int|Str`. `input` starts right after the `subset` keyword.
///
/// Lowered to `do { subset <name> …; <name> }`, reusing the ordinary subset
/// declaration machinery — mutsu already evaluates a subset name in term
/// position to its type object. `::` is the anonymous package name and gets a
/// unique synthesized name. Requires a following `of`/`where` (a bare `subset`
/// term is meaningless) so a stray `subset` bareword still falls through.
pub(in crate::parser) fn inline_subset_term(input: &str) -> PResult<'_, Expr> {
    let (rest, _) = ws1(input)?;
    // Anonymous `::` name (standalone, followed by whitespace), or an explicit
    // (qualified) name.
    let (rest, name) = if let Some(r) = rest.strip_prefix("::")
        && r.starts_with(char::is_whitespace)
    {
        let n = format!(
            "__mutsu_anon_subset_{}",
            ANON_SUBSET_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
        );
        (r, n)
    } else {
        qualified_ident(rest)?
    };
    let (rest, _) = ws(rest)?;
    // Require `of` and/or `where` so a bare `subset` word is not misread as a term.
    if keyword("of", rest).is_none() && keyword("where", rest).is_none() {
        return Err(PError::expected("inline subset term"));
    }
    super::super::simple::register_user_type(&name);
    let mut rest = rest;
    let mut base: Option<String> = None;
    if let Some(r) = keyword("of", rest) {
        let (r, _) = ws1(r)?;
        let (r, b) = parse_type_constraint_expr(r).ok_or_else(|| PError::expected("type"))?;
        let (r, _) = ws(r)?;
        base = Some(b);
        rest = r;
    }
    let base = base.unwrap_or_else(|| "Any".to_string());
    let (rest, predicate) = if let Some(r) = keyword("where", rest) {
        let (r, _) = ws1(r)?;
        let (r, pred) = expression(r)?;
        (r, Some(pred))
    } else {
        (rest, None)
    };
    let decl = Stmt::SubsetDecl {
        name: Symbol::intern(&name),
        base,
        predicate,
        version: super::super::simple::current_language_version(),
        is_export: false,
        export_tags: Vec::new(),
        is_my: false,
    };
    Ok((
        rest,
        Expr::DoBlock {
            body: vec![decl, Stmt::Expr(Expr::BareWord(name))],
            label: None,
        },
    ))
}
