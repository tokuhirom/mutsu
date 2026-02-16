use super::parse_result::{PResult, parse_char, parse_tag, take_while1};

use crate::ast::{Expr, Stmt};
use crate::token_kind::TokenKind;
use crate::value::Value;

use super::helpers::ws;
use super::primary::{parse_call_arg_list, primary};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ComparisonOp {
    StrictEq,
    NumEq,
    NotDivisibleBy,
    NumNe,
    SmartNotMatch,
    SmartMatch,
    Spaceship,
    NumLe,
    NumGe,
    NumLt,
    NumGt,
    StrEq,
    StrNe,
    StrLt,
    StrGt,
    StrLe,
    StrGe,
    Leg,
    Cmp,
    Eqv,
    Before,
    After,
}

impl ComparisonOp {
    fn token_kind(self) -> TokenKind {
        match self {
            ComparisonOp::StrictEq => TokenKind::EqEqEq,
            ComparisonOp::NumEq => TokenKind::EqEq,
            ComparisonOp::NotDivisibleBy => TokenKind::BangPercentPercent,
            ComparisonOp::NumNe => TokenKind::BangEq,
            ComparisonOp::SmartNotMatch => TokenKind::BangTilde,
            ComparisonOp::SmartMatch => TokenKind::SmartMatch,
            ComparisonOp::Spaceship => TokenKind::LtEqGt,
            ComparisonOp::NumLe => TokenKind::Lte,
            ComparisonOp::NumGe => TokenKind::Gte,
            ComparisonOp::NumLt => TokenKind::Lt,
            ComparisonOp::NumGt => TokenKind::Gt,
            ComparisonOp::StrEq => TokenKind::Ident("eq".to_string()),
            ComparisonOp::StrNe => TokenKind::Ident("ne".to_string()),
            ComparisonOp::StrLt => TokenKind::Ident("lt".to_string()),
            ComparisonOp::StrGt => TokenKind::Ident("gt".to_string()),
            ComparisonOp::StrLe => TokenKind::Ident("le".to_string()),
            ComparisonOp::StrGe => TokenKind::Ident("ge".to_string()),
            ComparisonOp::Leg => TokenKind::Ident("leg".to_string()),
            ComparisonOp::Cmp => TokenKind::Ident("cmp".to_string()),
            ComparisonOp::Eqv => TokenKind::Ident("eqv".to_string()),
            ComparisonOp::Before => TokenKind::Ident("before".to_string()),
            ComparisonOp::After => TokenKind::Ident("after".to_string()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ConcatOp {
    Concat,
    Repeat,
}

impl ConcatOp {
    fn token_kind(self) -> TokenKind {
        match self {
            ConcatOp::Concat => TokenKind::Tilde,
            ConcatOp::Repeat => TokenKind::Ident("x".to_string()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AdditiveOp {
    Add,
    Sub,
}

impl AdditiveOp {
    fn token_kind(self) -> TokenKind {
        match self {
            AdditiveOp::Add => TokenKind::Plus,
            AdditiveOp::Sub => TokenKind::Minus,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum MultiplicativeOp {
    Mul,
    Div,
    Mod,
    DivisibleBy,
    IntDiv,
    IntMod,
    Gcd,
    Lcm,
}

impl MultiplicativeOp {
    fn token_kind(self) -> TokenKind {
        match self {
            MultiplicativeOp::Mul => TokenKind::Star,
            MultiplicativeOp::Div => TokenKind::Slash,
            MultiplicativeOp::Mod => TokenKind::Percent,
            MultiplicativeOp::DivisibleBy => TokenKind::PercentPercent,
            MultiplicativeOp::IntDiv => TokenKind::Ident("div".to_string()),
            MultiplicativeOp::IntMod => TokenKind::Ident("mod".to_string()),
            MultiplicativeOp::Gcd => TokenKind::Ident("gcd".to_string()),
            MultiplicativeOp::Lcm => TokenKind::Ident("lcm".to_string()),
        }
    }
}

fn parse_concat_op(r: &str) -> Option<(ConcatOp, usize)> {
    if r.starts_with('~') && !r.starts_with("~~") && !r.starts_with("~=") {
        Some((ConcatOp::Concat, 1))
    } else if r.starts_with('x') && !is_ident_char(r.as_bytes().get(1).copied()) {
        Some((ConcatOp::Repeat, 1))
    } else {
        None
    }
}

fn parse_additive_op(r: &str) -> Option<(AdditiveOp, usize)> {
    if r.starts_with('+') && !r.starts_with("++") && !r.starts_with("+=") {
        Some((AdditiveOp::Add, 1))
    } else if r.starts_with('-')
        && !r.starts_with("--")
        && !r.starts_with("-=")
        && !r.starts_with("->")
    {
        Some((AdditiveOp::Sub, 1))
    } else {
        None
    }
}

fn parse_multiplicative_op(r: &str) -> Option<(MultiplicativeOp, usize)> {
    if r.starts_with('*') && !r.starts_with("**") && !r.starts_with("*=") {
        Some((MultiplicativeOp::Mul, 1))
    } else if r.starts_with('/') && !r.starts_with("//") {
        Some((MultiplicativeOp::Div, 1))
    } else if r.starts_with('%')
        && !r.starts_with("%%")
        && !is_ident_char(r.as_bytes().get(1).copied())
    {
        Some((MultiplicativeOp::Mod, 1))
    } else if r.starts_with("%%") {
        Some((MultiplicativeOp::DivisibleBy, 2))
    } else if r.starts_with("div") && !is_ident_char(r.as_bytes().get(3).copied()) {
        Some((MultiplicativeOp::IntDiv, 3))
    } else if r.starts_with("mod") && !is_ident_char(r.as_bytes().get(3).copied()) {
        Some((MultiplicativeOp::IntMod, 3))
    } else if r.starts_with("gcd") && !is_ident_char(r.as_bytes().get(3).copied()) {
        Some((MultiplicativeOp::Gcd, 3))
    } else if r.starts_with("lcm") && !is_ident_char(r.as_bytes().get(3).copied()) {
        Some((MultiplicativeOp::Lcm, 3))
    } else {
        None
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PrefixUnaryOp {
    Not,
    Boolify,
    PreInc,
    PreDec,
    Negate,
    Positive,
    Stringify,
}

impl PrefixUnaryOp {
    fn token_kind(self) -> TokenKind {
        match self {
            PrefixUnaryOp::Not => TokenKind::Bang,
            PrefixUnaryOp::Boolify => TokenKind::Question,
            PrefixUnaryOp::PreInc => TokenKind::PlusPlus,
            PrefixUnaryOp::PreDec => TokenKind::MinusMinus,
            PrefixUnaryOp::Negate => TokenKind::Minus,
            PrefixUnaryOp::Positive => TokenKind::Plus,
            PrefixUnaryOp::Stringify => TokenKind::Tilde,
        }
    }

    fn consumes_ws(self) -> bool {
        matches!(self, PrefixUnaryOp::Not | PrefixUnaryOp::Boolify)
    }

    fn parses_postfix_target(self) -> bool {
        matches!(self, PrefixUnaryOp::PreInc | PrefixUnaryOp::PreDec)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PostfixUpdateOp {
    Inc,
    Dec,
}

impl PostfixUpdateOp {
    fn token_kind(self) -> TokenKind {
        match self {
            PostfixUpdateOp::Inc => TokenKind::PlusPlus,
            PostfixUpdateOp::Dec => TokenKind::MinusMinus,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LogicalOp {
    Or,        // or (word)
    And,       // and (word)
    OrOr,      // ||
    AndAnd,    // &&
    DefinedOr, // //
}

impl LogicalOp {
    fn token_kind(self) -> TokenKind {
        match self {
            LogicalOp::Or => TokenKind::OrWord,
            LogicalOp::And => TokenKind::AndAnd,
            LogicalOp::OrOr => TokenKind::OrOr,
            LogicalOp::AndAnd => TokenKind::AndAnd,
            LogicalOp::DefinedOr => TokenKind::SlashSlash,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum JunctiveOp {
    Or,  // ?|
    And, // ?&
    Xor, // ?^
}

impl JunctiveOp {
    fn token_kind(self) -> TokenKind {
        match self {
            JunctiveOp::Or => TokenKind::Ident("?|".to_string()),
            JunctiveOp::And => TokenKind::Ident("?&".to_string()),
            JunctiveOp::Xor => TokenKind::Ident("?^".to_string()),
        }
    }
}

fn parse_prefix_unary_op(input: &str) -> Option<(PrefixUnaryOp, usize)> {
    if input.starts_with('!')
        && !input.starts_with("!!")
        && !input.starts_with("!~~")
        && !input.starts_with("!%%")
    {
        Some((PrefixUnaryOp::Not, 1))
    } else if input.starts_with('?')
        && !input.starts_with("??")
        && !input.starts_with("?|")
        && !input.starts_with("?&")
        && !input.starts_with("?^")
    {
        Some((PrefixUnaryOp::Boolify, 1))
    } else if input.starts_with("so") && !is_ident_char(input.as_bytes().get(2).copied()) {
        Some((PrefixUnaryOp::Boolify, 2))
    } else if input.starts_with("++") {
        Some((PrefixUnaryOp::PreInc, 2))
    } else if input.starts_with("--") {
        Some((PrefixUnaryOp::PreDec, 2))
    } else if input.starts_with('-')
        && !input.starts_with("--")
        && !input.starts_with("->")
        && let Some(&c) = input.as_bytes().get(1)
        && (c == b'$' || c == b'@' || c == b'(' || c.is_ascii_digit() || c.is_ascii_alphabetic())
    {
        Some((PrefixUnaryOp::Negate, 1))
    } else if input.starts_with('+')
        && !input.starts_with("++")
        && let Some(&c) = input.as_bytes().get(1)
        && (c == b'$' || c == b'@' || c == b'(' || c.is_ascii_digit() || c.is_ascii_alphabetic())
    {
        Some((PrefixUnaryOp::Positive, 1))
    } else if input.starts_with('~')
        && !input.starts_with("~~")
        && let Some(&c) = input.as_bytes().get(1)
        && (c == b'$'
            || c == b'@'
            || c == b'('
            || c == b'"'
            || c == b'\''
            || c.is_ascii_digit()
            || c.is_ascii_alphabetic())
    {
        Some((PrefixUnaryOp::Stringify, 1))
    } else {
        None
    }
}

fn parse_postfix_update_op(input: &str) -> Option<(PostfixUpdateOp, usize)> {
    if input.starts_with("++") {
        Some((PostfixUpdateOp::Inc, 2))
    } else if input.starts_with("--") {
        Some((PostfixUpdateOp::Dec, 2))
    } else {
        None
    }
}

fn parse_junctive_op(input: &str) -> Option<(JunctiveOp, usize)> {
    if input.starts_with("?|") {
        Some((JunctiveOp::Or, 2))
    } else if input.starts_with("?&") {
        Some((JunctiveOp::And, 2))
    } else if input.starts_with("?^") {
        Some((JunctiveOp::Xor, 2))
    } else {
        None
    }
}

fn parse_or_or_op(input: &str) -> Option<(LogicalOp, usize)> {
    if input.starts_with("||") {
        Some((LogicalOp::OrOr, 2))
    } else if input.starts_with("//") && !input.starts_with("///") {
        Some((LogicalOp::DefinedOr, 2))
    } else {
        None
    }
}

fn parse_and_and_op(input: &str) -> Option<(LogicalOp, usize)> {
    if input.starts_with("&&") {
        Some((LogicalOp::AndAnd, 2))
    } else {
        None
    }
}

fn parse_word_logical_op(input: &str) -> Option<(LogicalOp, usize)> {
    if input.starts_with("or") && !is_ident_char(input.as_bytes().get(2).copied()) {
        Some((LogicalOp::Or, 2))
    } else if input.starts_with("and") && !is_ident_char(input.as_bytes().get(3).copied()) {
        Some((LogicalOp::And, 3))
    } else {
        None
    }
}

/// Parse an expression (full precedence).
pub(super) fn expression(input: &str) -> PResult<'_, Expr> {
    let (rest, mut expr) = ternary(input)?;
    // Handle => (fat arrow / pair constructor) - lower precedence than ternary
    let (r, _) = ws(rest)?;
    if r.starts_with("=>") && !r.starts_with("==>") {
        let r = &r[2..];
        let (r, _) = ws(r)?;
        let (r, value) = or_expr(r)?;
        // Auto-quote bareword on LHS of => to a string literal
        let left = match expr {
            Expr::BareWord(ref name) => Expr::Literal(Value::Str(name.clone())),
            _ => expr,
        };
        return Ok((
            r,
            Expr::Binary {
                left: Box::new(left),
                op: TokenKind::FatArrow,
                right: Box::new(value),
            },
        ));
    }
    // Wrap WhateverCode expressions in a lambda
    if contains_whatever(&expr) {
        let body_expr = replace_whatever(&expr);
        expr = Expr::Lambda {
            param: "_".to_string(),
            body: vec![Stmt::Expr(body_expr)],
        };
    }
    Ok((rest, expr))
}

fn is_whatever(expr: &Expr) -> bool {
    matches!(expr, Expr::Literal(Value::Num(f)) if f.is_infinite() && f.is_sign_positive())
}

fn contains_whatever(expr: &Expr) -> bool {
    match expr {
        e if is_whatever(e) => true,
        Expr::Binary { left, right, .. } => contains_whatever(left) || contains_whatever(right),
        Expr::Unary { expr, .. } => contains_whatever(expr),
        _ => false,
    }
}

fn replace_whatever(expr: &Expr) -> Expr {
    match expr {
        e if is_whatever(e) => Expr::Var("_".to_string()),
        Expr::Binary { left, op, right } => Expr::Binary {
            left: Box::new(replace_whatever(left)),
            op: op.clone(),
            right: Box::new(replace_whatever(right)),
        },
        Expr::Unary { op, expr } => Expr::Unary {
            op: op.clone(),
            expr: Box::new(replace_whatever(expr)),
        },
        _ => expr.clone(),
    }
}

/// Ternary: expr ?? expr !! expr
fn ternary(input: &str) -> PResult<'_, Expr> {
    let (input, cond) = or_expr(input)?;
    let (input, _) = ws(input)?;
    if let Ok((input, _)) = parse_tag(input, "??") {
        let (input, _) = ws(input)?;
        let (input, then_expr) = expression(input)?;
        let (input, _) = ws(input)?;
        let (input, _) = parse_tag(input, "!!")?;
        let (input, _) = ws(input)?;
        let (input, else_expr) = expression(input)?;
        return Ok((
            input,
            Expr::Ternary {
                cond: Box::new(cond),
                then_expr: Box::new(then_expr),
                else_expr: Box::new(else_expr),
            },
        ));
    }
    Ok((input, cond))
}

/// Low-precedence: or / and / not
fn or_expr(input: &str) -> PResult<'_, Expr> {
    let (mut rest, mut left) = and_expr(input)?;
    loop {
        let (r, _) = ws(rest)?;
        if let Some((op @ LogicalOp::Or, len)) = parse_word_logical_op(r) {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = and_expr(r)?;
            left = Expr::Binary {
                left: Box::new(left),
                op: op.token_kind(),
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        break;
    }
    Ok((rest, left))
}

fn and_expr(input: &str) -> PResult<'_, Expr> {
    let (mut rest, mut left) = not_expr(input)?;
    loop {
        let (r, _) = ws(rest)?;
        if let Some((op @ LogicalOp::And, len)) = parse_word_logical_op(r) {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = not_expr(r)?;
            left = Expr::Binary {
                left: Box::new(left),
                op: op.token_kind(),
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        break;
    }
    Ok((rest, left))
}

fn not_expr(input: &str) -> PResult<'_, Expr> {
    if input.starts_with("not") && !is_ident_char(input.as_bytes().get(3).copied()) {
        let r = &input[3..];
        // When `not(` — no space before paren — parens delimit the argument,
        // and the result participates in the full expression (handled in prefix_expr).
        if r.starts_with('(') {
            // Fall through to or_or_expr which will eventually call prefix_expr
            // where not(...) is handled as a tight binding.
        } else {
            let (r, _) = ws(r)?;
            let (r, expr) = not_expr(r)?;
            return Ok((
                r,
                Expr::Unary {
                    op: TokenKind::Bang,
                    expr: Box::new(expr),
                },
            ));
        }
    }
    or_or_expr(input)
}

/// || and //
fn or_or_expr(input: &str) -> PResult<'_, Expr> {
    let (mut rest, mut left) = and_and_expr(input)?;
    loop {
        let (r, _) = ws(rest)?;
        if let Some((op, len)) = parse_or_or_op(r) {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = and_and_expr(r)?;
            left = Expr::Binary {
                left: Box::new(left),
                op: op.token_kind(),
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        break;
    }
    Ok((rest, left))
}

/// &&
fn and_and_expr(input: &str) -> PResult<'_, Expr> {
    let (mut rest, mut left) = junctive_expr(input)?;
    loop {
        let (r, _) = ws(rest)?;
        if let Some((op, len)) = parse_and_and_op(r) {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = junctive_expr(r)?;
            left = Expr::Binary {
                left: Box::new(left),
                op: op.token_kind(),
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        break;
    }
    Ok((rest, left))
}

/// Boolean bitwise: ?| ?& ?^
fn junctive_expr(input: &str) -> PResult<'_, Expr> {
    let (mut rest, mut left) = comparison_expr(input)?;
    loop {
        let (r, _) = ws(rest)?;
        if let Some((op, len)) = parse_junctive_op(r) {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = comparison_expr(r)?;
            left = Expr::Binary {
                left: Box::new(left),
                op: op.token_kind(),
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        break;
    }
    Ok((rest, left))
}

/// Comparison: ==, !=, <, >, <=, >=, eq, ne, lt, gt, le, ge, ~~, !~~, ===, <=>
fn comparison_expr(input: &str) -> PResult<'_, Expr> {
    let (rest, left) = range_expr(input)?;
    let (r, _) = ws(rest)?;
    if let Some((op, len)) = parse_comparison_op(r) {
        let r = &r[len..];
        let (r, _) = ws(r)?;
        let (r, right) = range_expr(r)?;
        let mut result = Expr::Binary {
            left: Box::new(left),
            op: op.token_kind(),
            right: Box::new(right.clone()),
        };
        // Chained comparisons: 2 < $_ < 4 → (2 < $_) && ($_ < 4)
        let mut prev_right = right;
        let mut r = r;
        loop {
            let (r2, _) = ws(r)?;
            if let Some((cop, chain_len)) = parse_comparison_op(r2) {
                let r2 = &r2[chain_len..];
                let (r2, _) = ws(r2)?;
                let (r2, next_right) = range_expr(r2)?;
                let next_cmp = Expr::Binary {
                    left: Box::new(prev_right),
                    op: cop.token_kind(),
                    right: Box::new(next_right.clone()),
                };
                result = Expr::Binary {
                    left: Box::new(result),
                    op: TokenKind::AndAnd,
                    right: Box::new(next_cmp),
                };
                prev_right = next_right;
                r = r2;
            } else {
                break;
            }
        }
        return Ok((r, result));
    }

    Ok((rest, left))
}

/// Extract a comparison operator from the start of the input, returning the op and its length.
fn parse_comparison_op(r: &str) -> Option<(ComparisonOp, usize)> {
    if r.starts_with("===") {
        Some((ComparisonOp::StrictEq, 3))
    } else if r.starts_with("==") && !r.starts_with("===") {
        Some((ComparisonOp::NumEq, 2))
    } else if r.starts_with("!%%") {
        Some((ComparisonOp::NotDivisibleBy, 3))
    } else if r.starts_with("!=") {
        Some((ComparisonOp::NumNe, 2))
    } else if r.starts_with("!~~") {
        Some((ComparisonOp::SmartNotMatch, 3))
    } else if r.starts_with("~~") {
        Some((ComparisonOp::SmartMatch, 2))
    } else if r.starts_with("<=>") {
        Some((ComparisonOp::Spaceship, 3))
    } else if r.starts_with("<=") && !r.starts_with("<=>") {
        Some((ComparisonOp::NumLe, 2))
    } else if r.starts_with(">=") {
        Some((ComparisonOp::NumGe, 2))
    } else if r.starts_with('<') && !r.starts_with("<<") && !r.starts_with("<=") {
        Some((ComparisonOp::NumLt, 1))
    } else if r.starts_with('>') && !r.starts_with(">>") && !r.starts_with(">=") {
        Some((ComparisonOp::NumGt, 1))
    } else if r.starts_with("eq") && !is_ident_char(r.as_bytes().get(2).copied()) {
        Some((ComparisonOp::StrEq, 2))
    } else if r.starts_with("ne") && !is_ident_char(r.as_bytes().get(2).copied()) {
        Some((ComparisonOp::StrNe, 2))
    } else if r.starts_with("lt") && !is_ident_char(r.as_bytes().get(2).copied()) {
        Some((ComparisonOp::StrLt, 2))
    } else if r.starts_with("gt") && !is_ident_char(r.as_bytes().get(2).copied()) {
        Some((ComparisonOp::StrGt, 2))
    } else if r.starts_with("le") && !is_ident_char(r.as_bytes().get(2).copied()) {
        Some((ComparisonOp::StrLe, 2))
    } else if r.starts_with("ge") && !is_ident_char(r.as_bytes().get(2).copied()) {
        Some((ComparisonOp::StrGe, 2))
    } else if r.starts_with("leg") && !is_ident_char(r.as_bytes().get(3).copied()) {
        Some((ComparisonOp::Leg, 3))
    } else if r.starts_with("cmp") && !is_ident_char(r.as_bytes().get(3).copied()) {
        Some((ComparisonOp::Cmp, 3))
    } else if r.starts_with("eqv") && !is_ident_char(r.as_bytes().get(3).copied()) {
        Some((ComparisonOp::Eqv, 3))
    } else if r.starts_with("before") && !is_ident_char(r.as_bytes().get(6).copied()) {
        Some((ComparisonOp::Before, 6))
    } else if r.starts_with("after") && !is_ident_char(r.as_bytes().get(5).copied()) {
        Some((ComparisonOp::After, 5))
    } else {
        None
    }
}

/// Range: ..  ..^  ^..  ^..^
fn range_expr(input: &str) -> PResult<'_, Expr> {
    let (rest, left) = structural_expr(input)?;
    let (r, _) = ws(rest)?;

    if let Some(stripped) = r.strip_prefix("^..^") {
        let (r, _) = ws(stripped)?;
        let (r, right) = structural_expr(r)?;
        return Ok((
            r,
            Expr::Binary {
                left: Box::new(left),
                op: TokenKind::CaretDotDotCaret,
                right: Box::new(right),
            },
        ));
    }
    if let Some(stripped) = r.strip_prefix("^..") {
        let (r, _) = ws(stripped)?;
        let (r, right) = structural_expr(r)?;
        return Ok((
            r,
            Expr::Binary {
                left: Box::new(left),
                op: TokenKind::CaretDotDot,
                right: Box::new(right),
            },
        ));
    }
    if let Some(stripped) = r.strip_prefix("..^") {
        let (r, _) = ws(stripped)?;
        let (r, right) = structural_expr(r)?;
        return Ok((
            r,
            Expr::Binary {
                left: Box::new(left),
                op: TokenKind::DotDotCaret,
                right: Box::new(right),
            },
        ));
    }
    if r.starts_with("..") && !r.starts_with("...") {
        let r = &r[2..];
        let (r, _) = ws(r)?;
        let (r, right) = structural_expr(r)?;
        return Ok((
            r,
            Expr::Binary {
                left: Box::new(left),
                op: TokenKind::DotDot,
                right: Box::new(right),
            },
        ));
    }
    Ok((rest, left))
}

/// Structural infix: but, does
fn structural_expr(input: &str) -> PResult<'_, Expr> {
    let (mut rest, mut left) = concat_expr(input)?;
    loop {
        let (r, _) = ws(rest)?;
        if r.starts_with("but") && !is_ident_char(r.as_bytes().get(3).copied()) {
            let r = &r[3..];
            let (r, _) = ws(r)?;
            let (r, right) = concat_expr(r)?;
            left = Expr::Binary {
                left: Box::new(left),
                op: TokenKind::Ident("but".to_string()),
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        if r.starts_with("does") && !is_ident_char(r.as_bytes().get(4).copied()) {
            let r = &r[4..];
            let (r, _) = ws(r)?;
            let (r, right) = concat_expr(r)?;
            left = Expr::Binary {
                left: Box::new(left),
                op: TokenKind::Ident("does".to_string()),
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        break;
    }
    Ok((rest, left))
}

/// String concatenation: ~
fn concat_expr(input: &str) -> PResult<'_, Expr> {
    let (mut rest, mut left) = additive_expr(input)?;
    loop {
        let (r, _) = ws(rest)?;
        if let Some((op, len)) = parse_concat_op(r) {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = additive_expr(r)?;
            left = Expr::Binary {
                left: Box::new(left),
                op: op.token_kind(),
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        break;
    }
    Ok((rest, left))
}

/// Addition/subtraction: + -
fn additive_expr(input: &str) -> PResult<'_, Expr> {
    let (mut rest, mut left) = multiplicative_expr(input)?;
    loop {
        let (r, _) = ws(rest)?;
        if let Some((op, len)) = parse_additive_op(r) {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = multiplicative_expr(r)?;
            left = Expr::Binary {
                left: Box::new(left),
                op: op.token_kind(),
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        break;
    }
    Ok((rest, left))
}

/// Multiplication/division: * / % div mod gcd lcm
fn multiplicative_expr(input: &str) -> PResult<'_, Expr> {
    let (mut rest, mut left) = power_expr(input)?;
    loop {
        let (r, _) = ws(rest)?;
        if let Some((op, len)) = parse_multiplicative_op(r) {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = power_expr(r)?;
            left = Expr::Binary {
                left: Box::new(left),
                op: op.token_kind(),
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        break;
    }
    Ok((rest, left))
}

/// Exponentiation: **
fn power_expr(input: &str) -> PResult<'_, Expr> {
    let (rest, base) = prefix_expr(input)?;
    let (r, _) = ws(rest)?;
    if let Some(stripped) = r.strip_prefix("**") {
        let (r, _) = ws(stripped)?;
        let (r, exp) = power_expr(r)?; // right-associative
        return Ok((
            r,
            Expr::Binary {
                left: Box::new(base),
                op: TokenKind::StarStar,
                right: Box::new(exp),
            },
        ));
    }
    Ok((rest, base))
}

/// Prefix unary: !, ?, +, -, ~, so, not, ++, --
fn prefix_expr(input: &str) -> PResult<'_, Expr> {
    if let Some((op, len)) = parse_prefix_unary_op(input) {
        let mut rest = &input[len..];
        if op.consumes_ws() {
            let (r, _) = ws(rest)?;
            rest = r;
        }
        let (rest, expr) = if op.parses_postfix_target() {
            postfix_expr(rest)?
        } else {
            prefix_expr(rest)?
        };
        return Ok((
            rest,
            Expr::Unary {
                op: op.token_kind(),
                expr: Box::new(expr),
            },
        ));
    }
    // not(expr) — tight-binding form: not followed by ( without space
    if input.starts_with("not(") {
        let r = &input[3..];
        let (r, _) = parse_char(r, '(')?;
        let (r, _) = ws(r)?;
        let (r, expr) = expression(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        return Ok((
            r,
            Expr::Unary {
                op: TokenKind::Bang,
                expr: Box::new(expr),
            },
        ));
    }
    // so(expr) — tight-binding form: so followed by ( without space
    if input.starts_with("so(") {
        let r = &input[2..];
        let (r, _) = parse_char(r, '(')?;
        let (r, _) = ws(r)?;
        let (r, expr) = expression(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        return Ok((
            r,
            Expr::Unary {
                op: TokenKind::Question,
                expr: Box::new(expr),
            },
        ));
    }
    postfix_expr(input)
}

/// Postfix: method calls (.method), indexing ([]), ++, --
fn postfix_expr(input: &str) -> PResult<'_, Expr> {
    let (mut rest, mut expr) = primary(input)?;

    loop {
        // Method call: .method or .method(args) or .method: args
        // Also handles modifiers: .?method, .!method
        // Also handles: .^method (meta-method)
        // Also handles call-on: .(args)
        if rest.starts_with('.') && !rest.starts_with("..") {
            let r = &rest[1..];
            // Check for call-on syntax: .(args)
            if r.starts_with('(') {
                let (r, _) = parse_char(r, '(')?;
                let (r, _) = ws(r)?;
                let (r, args) = parse_call_arg_list(r)?;
                let (r, _) = ws(r)?;
                let (r, _) = parse_char(r, ')')?;
                expr = Expr::CallOn {
                    target: Box::new(expr),
                    args,
                };
                rest = r;
                continue;
            }
            // Check for modifier
            let (r, modifier) = if let Some(stripped) = r.strip_prefix('?') {
                (stripped, Some('?'))
            } else if let Some(stripped) = r.strip_prefix('^') {
                (stripped, Some('^'))
            } else {
                (r, None)
            };
            // Parse method name
            if let Ok((r, name)) =
                take_while1(r, |c: char| c.is_alphanumeric() || c == '_' || c == '-')
            {
                let name = name.to_string();
                // Check for args in parens
                if r.starts_with('(') {
                    let (r, _) = parse_char(r, '(')?;
                    let (r, _) = ws(r)?;
                    let (r, args) = parse_call_arg_list(r)?;
                    let (r, _) = ws(r)?;
                    let (r, _) = parse_char(r, ')')?;
                    expr = Expr::MethodCall {
                        target: Box::new(expr),
                        name,
                        args,
                        modifier,
                    };
                    rest = r;
                    continue;
                }
                // Check for colon-arg syntax: .method: arg, arg2
                let (r2, _) = ws(r)?;
                if r2.starts_with(':') && !r2.starts_with("::") {
                    let r3 = &r2[1..];
                    let (r3, _) = ws(r3)?;
                    // Parse the arguments after colon
                    if let Ok((r3, first_arg)) = expression(r3) {
                        let mut args = vec![first_arg];
                        let mut r_inner = r3;
                        loop {
                            let (r4, _) = ws(r_inner)?;
                            if !r4.starts_with(',') {
                                break;
                            }
                            let r4 = &r4[1..];
                            let (r4, _) = ws(r4)?;
                            if let Ok((r4, next)) = expression(r4) {
                                args.push(next);
                                r_inner = r4;
                            } else {
                                break;
                            }
                        }
                        expr = Expr::MethodCall {
                            target: Box::new(expr),
                            name,
                            args,
                            modifier,
                        };
                        rest = r_inner;
                        continue;
                    }
                }
                // No-arg method call
                expr = Expr::MethodCall {
                    target: Box::new(expr),
                    name,
                    args: Vec::new(),
                    modifier,
                };
                rest = r;
                continue;
            }
        }

        // CallOn: $var(args) — invoke a callable stored in a variable
        if rest.starts_with('(')
            && matches!(
                &expr,
                Expr::Var(_) | Expr::CodeVar(_) | Expr::MethodCall { .. }
            )
        {
            let (r, _) = parse_char(rest, '(')?;
            let (r, _) = ws(r)?;
            let (r, args) = parse_call_arg_list(r)?;
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, ')')?;
            expr = Expr::CallOn {
                target: Box::new(expr),
                args,
            };
            rest = r;
            continue;
        }

        // Array indexing: [expr]
        if rest.starts_with('[') {
            let r = &rest[1..];
            let (r, _) = ws(r)?;
            let (r, index) = expression(r)?;
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, ']')?;
            expr = Expr::Index {
                target: Box::new(expr),
                index: Box::new(index),
            };
            rest = r;
            continue;
        }

        // Hash indexing with angle brackets: %hash<key>, $hash<key>, @a[0]<key>, etc.
        // Only match when content is a simple word key (alphanumeric, no operators)
        if rest.starts_with('<')
            && !rest.starts_with("<=")
            && !rest.starts_with("<<")
            && !rest.starts_with("<=>")
        {
            let r = &rest[1..];
            if let Some(end) = r.find('>') {
                let key = &r[..end];
                if !key.is_empty()
                    && key
                        .chars()
                        .all(|c| c.is_alphanumeric() || c == '_' || c == '-')
                {
                    let r = &r[end + 1..];
                    expr = Expr::Index {
                        target: Box::new(expr),
                        index: Box::new(Expr::Literal(Value::Str(key.to_string()))),
                    };
                    rest = r;
                    continue;
                }
            }
        }

        // Hash indexing with braces: %hash{"key"}, %hash{$var}, @a[0]{"key"}, etc.
        if rest.starts_with('{')
            && matches!(&expr, Expr::HashVar(_) | Expr::Var(_) | Expr::Index { .. })
        {
            let r = &rest[1..];
            let (r, _) = ws(r)?;
            let (r, index) = expression(r)?;
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, '}')?;
            expr = Expr::Index {
                target: Box::new(expr),
                index: Box::new(index),
            };
            rest = r;
            continue;
        }

        // Postfix ++ and --
        if let Some((op, len)) = parse_postfix_update_op(rest) {
            rest = &rest[len..];
            expr = Expr::PostfixOp {
                op: op.token_kind(),
                expr: Box::new(expr),
            };
            continue;
        }

        break;
    }

    Ok((rest, expr))
}

/// Check if a byte is a valid identifier continuation character.
fn is_ident_char(b: Option<u8>) -> bool {
    match b {
        Some(c) => c.is_ascii_alphanumeric() || c == b'_' || c == b'-',
        None => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_binary_add() {
        let (rest, expr) = expression("1 + 2").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(
            expr,
            Expr::Binary {
                op: TokenKind::Plus,
                ..
            }
        ));
    }

    #[test]
    fn parse_comparison() {
        let (rest, expr) = expression("$x == $y").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(
            expr,
            Expr::Binary {
                op: TokenKind::EqEq,
                ..
            }
        ));
    }

    #[test]
    fn parse_method_call() {
        let (rest, expr) = expression("$x.defined").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::MethodCall { .. }));
    }

    #[test]
    fn parse_ternary() {
        let (rest, expr) = expression("$x ?? 1 !! 2").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::Ternary { .. }));
    }

    #[test]
    fn parse_junctive_op_all() {
        assert_eq!(parse_junctive_op("?|"), Some((JunctiveOp::Or, 2)));
        assert_eq!(parse_junctive_op("?&"), Some((JunctiveOp::And, 2)));
        assert_eq!(parse_junctive_op("?^"), Some((JunctiveOp::Xor, 2)));
        assert_eq!(parse_junctive_op("?"), None);
        assert_eq!(parse_junctive_op("??"), None);
    }

    #[test]
    fn parse_or_or_op_all() {
        assert_eq!(parse_or_or_op("||"), Some((LogicalOp::OrOr, 2)));
        assert_eq!(parse_or_or_op("//"), Some((LogicalOp::DefinedOr, 2)));
        assert_eq!(parse_or_or_op("///"), None);
        assert_eq!(parse_or_or_op("|"), None);
    }

    #[test]
    fn parse_and_and_op_all() {
        assert_eq!(parse_and_and_op("&&"), Some((LogicalOp::AndAnd, 2)));
        assert_eq!(parse_and_and_op("&"), None);
    }

    #[test]
    fn parse_word_logical_op_all() {
        assert_eq!(parse_word_logical_op("or "), Some((LogicalOp::Or, 2)));
        assert_eq!(parse_word_logical_op("and "), Some((LogicalOp::And, 3)));
        assert_eq!(parse_word_logical_op("or_foo"), None);
        assert_eq!(parse_word_logical_op("and_bar"), None);
        assert_eq!(parse_word_logical_op("oracle"), None);
    }

    #[test]
    fn parse_logical_operators_in_expr() {
        // Test word forms
        let (rest, expr) = expression("1 or 2").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(
            expr,
            Expr::Binary {
                op: TokenKind::OrWord,
                ..
            }
        ));

        let (rest, expr) = expression("1 and 2").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(
            expr,
            Expr::Binary {
                op: TokenKind::AndAnd,
                ..
            }
        ));

        // Test symbolic forms
        let (rest, expr) = expression("1 || 2").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(
            expr,
            Expr::Binary {
                op: TokenKind::OrOr,
                ..
            }
        ));

        let (rest, expr) = expression("1 && 2").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(
            expr,
            Expr::Binary {
                op: TokenKind::AndAnd,
                ..
            }
        ));

        let (rest, expr) = expression("1 // 2").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(
            expr,
            Expr::Binary {
                op: TokenKind::SlashSlash,
                ..
            }
        ));
    }

    #[test]
    fn parse_junctive_operators_in_expr() {
        let inputs = [("1 ?| 2", "?|"), ("1 ?& 2", "?&"), ("1 ?^ 2", "?^")];

        for (input, expected_op) in inputs {
            let (rest, expr) = expression(input).unwrap();
            assert_eq!(rest, "");
            if let Expr::Binary { op, .. } = expr {
                match op {
                    TokenKind::Ident(s) => assert_eq!(s, expected_op),
                    _ => panic!("Expected Ident token for {}", expected_op),
                }
            } else {
                panic!("Expected Binary expression");
            }
        }
    }
}
