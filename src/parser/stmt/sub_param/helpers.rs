use crate::ast::{Expr, ParamDef};
use crate::parser::parse_result::{PError, PResult};
use crate::symbol::Symbol;
use crate::value::Value;
use std::collections::HashMap;

/// Helper to construct a default ParamDef with only required fields.
pub(crate) fn make_param(name: String) -> ParamDef {
    ParamDef {
        name,
        default: None,
        multi_invocant: true,
        required: false,
        named: false,
        named_alias: false,
        slurpy: false,
        double_slurpy: false,
        onearg: false,
        sigilless: false,
        type_constraint: None,
        literal_value: None,
        sub_signature: None,
        where_constraint: None,
        traits: Vec::new(),
        optional_marker: false,
        outer_sub_signature: None,
        code_signature: None,
        is_invocant: false,
        shape_constraints: None,
        block_param: false,
    }
}

/// Build an `X::Parameter::AfterDefault` error for a trait or post-constraint
/// that appears after the parameter's default value (e.g. `$x = 60 is rw` or
/// `$x = 60 where Int`). `kind` is `"trait"` or `"post constraint"`; `modifier`
/// is the offending source fragment (`is rw`, `where Int`); `default` is the
/// default value source (`60`).
pub(crate) fn after_default_error(kind: &str, modifier: &str, default: &str) -> PError {
    let msg = format!(
        "The {kind} '{modifier}' came after the default value.  Did you mean: ...{modifier} = {default} ?"
    );
    let mut attrs = HashMap::new();
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    attrs.insert("type".to_string(), Value::str(kind.to_string()));
    attrs.insert("modifier".to_string(), Value::str(modifier.to_string()));
    attrs.insert("default".to_string(), Value::str(default.to_string()));
    let ex = Value::make_instance(Symbol::intern("X::Parameter::AfterDefault"), attrs);
    PError::fatal_with_exception(msg, Box::new(ex))
}

pub(crate) fn parse_param_default_expr(input: &str) -> PResult<'_, Expr> {
    if let Ok((rest, expr)) = crate::parser::expr::expression(input) {
        return Ok((rest, expr));
    }
    if let Some(after_my) = input.strip_prefix("my ")
        && let Ok((rest, expr)) = crate::parser::expr::expression(after_my)
    {
        return Ok((rest, expr));
    }
    Err(PError::expected("parameter default expression"))
}

pub(crate) fn is_anonymous_sigil_param(param: &ParamDef) -> bool {
    matches!(
        param.name.as_str(),
        "__ANON_STATE__" | "__ANON_ARRAY__" | "__ANON_HASH__" | "__ANON_CODE__"
    )
}

pub(crate) fn starts_with_sigil_param(input: &str) -> bool {
    matches!(input.as_bytes().first(), Some(b'$' | b'@' | b'%' | b'&'))
}

/// The optional tail a parameter may carry after a destructuring sub-signature:
/// `is` traits, a `where` post-constraint, and a default value.
pub(crate) struct SubsigTail {
    pub(crate) traits: Vec<String>,
    pub(crate) where_constraint: Option<Box<Expr>>,
    pub(crate) default: Option<Expr>,
}

/// Parse whatever follows a parameter's sub-signature.
///
/// A parameter may carry a sub-signature *and* a post-constraint at once —
/// `sub f($x ($a, $b) where { ... })`, `:$x! (*@a) where { ... }` — with the
/// `where` testing the parameter's own value while the sub-signature unpacks it.
/// Each sub-signature branch in `param_inner` used to stop after its `is` trait
/// loop, so the `where` was left unconsumed and the signature failed to parse at
/// the closing paren. Sharing one tail parser keeps the four branches (bare
/// `(...)`, `$x (...)`, `&cb (...)`, `@a [...]`) in step.
pub(crate) fn parse_subsig_tail(input: &str) -> PResult<'_, SubsigTail> {
    use crate::parser::helpers::{ws, ws1};

    let (mut rest, _) = ws(input)?;
    let mut traits = Vec::new();
    while let Some(r) = super::super::keyword("is", rest) {
        let (r, _) = ws1(r)?;
        let (r, trait_name) = super::super::ident(r)?;
        let (r, _) = super::super::sub::validate_param_trait(&trait_name, &traits, r)?;
        traits.push(trait_name);
        let (r, _) = ws(r)?;
        rest = r;
    }
    let (rest, where_constraint) = if let Some(r) = super::super::keyword("where", rest) {
        let (r, _) = ws1(r)?;
        let (r, constraint) = super::where_constraint::parse_where_constraint_expr(r)?;
        (r, Some(Box::new(constraint)))
    } else {
        (rest, None)
    };
    let (rest, _) = ws(rest)?;
    let (rest, default) = if rest.starts_with('=') && !rest.starts_with("==") {
        let (r, _) = ws(&rest[1..])?;
        let (r, expr) = crate::parser::expr::expression(r)?;
        (r, Some(expr))
    } else {
        (rest, None)
    };
    Ok((
        rest,
        SubsigTail {
            traits,
            where_constraint,
            default,
        },
    ))
}

/// An anonymous parameter that is nothing but a type — `multi infix:<->(e1, e2)`,
/// `sub f(Int)`, `multi prefix:<-->(::?CLASS)` — together with the `is` traits and
/// `where` clause it may still carry. `input` starts right after the type.
pub(crate) fn type_only_param(
    input: &str,
    type_constraint: String,
    named: bool,
    slurpy: bool,
) -> PResult<'_, ParamDef> {
    use crate::parser::helpers::{ws, ws1};
    let mut p = make_param("__type_only__".to_string());
    p.type_constraint = Some(type_constraint);
    p.named = named;
    p.slurpy = slurpy;
    let mut param_traits = Vec::new();
    let (mut r, _) = ws(input)?;
    while let Some(r2) = crate::parser::stmt::keyword("is", r) {
        let (r2, _) = ws1(r2)?;
        let (r2, trait_name) = crate::parser::stmt::ident(r2)?;
        let (r2, _) =
            crate::parser::stmt::sub::validate_param_trait(&trait_name, &param_traits, r2)?;
        param_traits.push(trait_name);
        let (r2, _) = ws(r2)?;
        r = r2;
    }
    p.traits = param_traits;
    if let Some(r2) = crate::parser::stmt::keyword("where", r) {
        let (r2, _) = ws1(r2)?;
        let (r2, constraint) = super::where_constraint::parse_where_constraint_expr(r2)?;
        p.where_constraint = Some(Box::new(constraint));
        r = r2;
    }
    Ok((r, p))
}

/// A literal-value parameter (`multi sub foo(0)`, `foo(-١)`, `foo(Inf)`, `foo("x")`).
///
/// Two passes, because the two grammars disagree about `-->`. The broad pass runs the
/// full expression parser, which is what recognizes every literal spelling mutsu
/// supports; but nothing stops its postfix layer from lexing the `--` of a signature's
/// `-->` onto the literal it just read, so `norm(M:D: 'column-sum'--> Numeric)`
/// (Math::Matrix, #7954) came back as `('column-sum'--) > Numeric` — not a literal, and
/// the whole parameter list then failed. The narrow pass is the literal-parameter
/// grammar itself: an optional sign and one primary term, matching exactly what
/// [`literal_value_from_expr`](crate::parser::stmt::sub::literal_value_from_expr)
/// accepts. It structurally cannot reach an infix or postfix operator, so the `-->`
/// survives for the caller to read.
pub(crate) fn parse_literal_param_value(input: &str) -> Option<(&str, Value)> {
    fn finish<'a>(lit_rest: &'a str, expr: &Expr) -> Option<(&'a str, Value)> {
        let v = crate::parser::stmt::sub::literal_value_from_expr(expr)?;
        let (after_lit, _) = crate::parser::helpers::ws(lit_rest).ok()?;
        let at_param_end = after_lit.starts_with([')', ',', ';', ']', '{'])
            || after_lit.starts_with("-->")
            || after_lit.is_empty();
        at_param_end.then_some((after_lit, v))
    }
    if let Ok((lit_rest, lit_expr)) = crate::parser::expr::expression(input)
        && let Some(found) = finish(lit_rest, &lit_expr)
    {
        return Some(found);
    }
    let (signed, sign) = match input.as_bytes().first() {
        Some(b'-') => (&input[1..], Some(crate::token_kind::TokenKind::Minus)),
        Some(b'+') => (&input[1..], Some(crate::token_kind::TokenKind::Plus)),
        _ => (input, None),
    };
    let (lit_rest, term) = crate::parser::primary::primary(signed).ok()?;
    let expr = match sign {
        Some(op) => Expr::Unary {
            op,
            expr: Box::new(term),
        },
        None => term,
    };
    finish(lit_rest, &expr)
}

/// Returns (rest, required, optional_marker).
/// `!` → required=true, optional_marker=false
/// `?` → required=false, optional_marker=true
/// neither → required=false, optional_marker=false
pub(crate) fn parse_required_suffix(input: &str) -> (&str, bool, bool) {
    if let Some(rest) = input.strip_prefix('!') {
        (rest, true, false)
    } else if let Some(rest) = input.strip_prefix('?') {
        (rest, false, true)
    } else {
        (input, false, false)
    }
}
