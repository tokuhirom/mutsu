use std::sync::atomic::{AtomicUsize, Ordering};

use super::super::expr::{
    QuotedMethodName, expression, expression_no_sequence, parse_quoted_method_name,
};
use super::super::helpers::ws;
use super::super::parse_result::{
    PError, PResult, merge_expected_messages, parse_char, take_while1,
};
use super::super::primary::parse_call_arg_list;

use crate::ast::{AssignOp, Expr, Stmt};
use crate::symbol::Symbol;
use crate::token_kind::TokenKind;
use crate::value::Value;

use super::{ident, parse_statement_modifier, var_name};

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

static TMP_INDEX_COUNTER: AtomicUsize = AtomicUsize::new(0);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum CompoundAssignOp {
    Comma,
    DefinedOr,
    LogicalOr,
    LogicalAnd,
    Add,
    Sub,
    Concat,
    Mul,
    Div,
    Mod,
    Power,
    Repeat,
    ListRepeat,
    BitOr,
    BitAnd,
    BitXor,
    BitShiftLeft,
    BitShiftRight,
    Min,
    Max,
    KeywordOr,
    KeywordAnd,
    Orelse,
    Andthen,
    IntDiv,
    Lcm,
    Gcd,
    StrBitAnd,
    StrBitOr,
    StrBitXor,
    StrShiftLeft,
    StrShiftRight,
    BoolBitAnd,
    BoolBitOr,
    BoolBitXor,
    XorXor,
}

impl CompoundAssignOp {
    pub(super) fn symbol(self) -> &'static str {
        match self {
            CompoundAssignOp::Comma => ",=",
            CompoundAssignOp::DefinedOr => "//=",
            CompoundAssignOp::LogicalOr => "||=",
            CompoundAssignOp::LogicalAnd => "&&=",
            CompoundAssignOp::Add => "+=",
            CompoundAssignOp::Sub => "-=",
            CompoundAssignOp::Concat => "~=",
            CompoundAssignOp::Mul => "*=",
            CompoundAssignOp::Div => "/=",
            CompoundAssignOp::Mod => "%=",
            CompoundAssignOp::Power => "**=",
            CompoundAssignOp::Repeat => "x=",
            CompoundAssignOp::ListRepeat => "xx=",
            CompoundAssignOp::BitOr => "+|=",
            CompoundAssignOp::BitAnd => "+&=",
            CompoundAssignOp::BitXor => "+^=",
            CompoundAssignOp::BitShiftLeft => "+<=",
            CompoundAssignOp::BitShiftRight => "+>=",
            CompoundAssignOp::Min => "min=",
            CompoundAssignOp::Max => "max=",
            CompoundAssignOp::KeywordOr => "or=",
            CompoundAssignOp::KeywordAnd => "and=",
            CompoundAssignOp::Orelse => "orelse=",
            CompoundAssignOp::Andthen => "andthen=",
            CompoundAssignOp::IntDiv => "div=",
            CompoundAssignOp::Lcm => "lcm=",
            CompoundAssignOp::Gcd => "gcd=",
            CompoundAssignOp::StrBitAnd => "~&=",
            CompoundAssignOp::StrBitOr => "~|=",
            CompoundAssignOp::StrBitXor => "~^=",
            CompoundAssignOp::StrShiftLeft => "~<=",
            CompoundAssignOp::StrShiftRight => "~>=",
            CompoundAssignOp::BoolBitAnd => "?&=",
            CompoundAssignOp::BoolBitOr => "?|=",
            CompoundAssignOp::BoolBitXor => "?^=",
            CompoundAssignOp::XorXor => "^^=",
        }
    }

    /// Convert from the base operator name (without `=` suffix) to a CompoundAssignOp.
    pub(crate) fn from_op_name(name: &str) -> Option<Self> {
        match name {
            "+" => Some(CompoundAssignOp::Add),
            "-" => Some(CompoundAssignOp::Sub),
            "~" => Some(CompoundAssignOp::Concat),
            "*" => Some(CompoundAssignOp::Mul),
            "/" => Some(CompoundAssignOp::Div),
            "%" => Some(CompoundAssignOp::Mod),
            "**" => Some(CompoundAssignOp::Power),
            "//" => Some(CompoundAssignOp::DefinedOr),
            "||" => Some(CompoundAssignOp::LogicalOr),
            "&&" => Some(CompoundAssignOp::LogicalAnd),
            "," => Some(CompoundAssignOp::Comma),
            "x" => Some(CompoundAssignOp::Repeat),
            "xx" => Some(CompoundAssignOp::ListRepeat),
            "+|" => Some(CompoundAssignOp::BitOr),
            "+&" => Some(CompoundAssignOp::BitAnd),
            "+^" => Some(CompoundAssignOp::BitXor),
            "+<" => Some(CompoundAssignOp::BitShiftLeft),
            "+>" => Some(CompoundAssignOp::BitShiftRight),
            "min" => Some(CompoundAssignOp::Min),
            "max" => Some(CompoundAssignOp::Max),
            "or" => Some(CompoundAssignOp::KeywordOr),
            "and" => Some(CompoundAssignOp::KeywordAnd),
            "orelse" => Some(CompoundAssignOp::Orelse),
            "andthen" => Some(CompoundAssignOp::Andthen),
            "div" => Some(CompoundAssignOp::IntDiv),
            "lcm" => Some(CompoundAssignOp::Lcm),
            "gcd" => Some(CompoundAssignOp::Gcd),
            "~&" => Some(CompoundAssignOp::StrBitAnd),
            "~|" => Some(CompoundAssignOp::StrBitOr),
            "~^" => Some(CompoundAssignOp::StrBitXor),
            "~<" => Some(CompoundAssignOp::StrShiftLeft),
            "~>" => Some(CompoundAssignOp::StrShiftRight),
            "?&" => Some(CompoundAssignOp::BoolBitAnd),
            "?|" => Some(CompoundAssignOp::BoolBitOr),
            "?^" => Some(CompoundAssignOp::BoolBitXor),
            "^^" => Some(CompoundAssignOp::XorXor),
            _ => None,
        }
    }

    pub(crate) fn token_kind(self) -> TokenKind {
        match self {
            CompoundAssignOp::Comma => TokenKind::Comma,
            CompoundAssignOp::DefinedOr => TokenKind::SlashSlash,
            CompoundAssignOp::LogicalOr => TokenKind::OrOr,
            CompoundAssignOp::LogicalAnd => TokenKind::AndAnd,
            CompoundAssignOp::Add => TokenKind::Plus,
            CompoundAssignOp::Sub => TokenKind::Minus,
            CompoundAssignOp::Concat => TokenKind::Tilde,
            CompoundAssignOp::Mul => TokenKind::Star,
            CompoundAssignOp::Div => TokenKind::Slash,
            CompoundAssignOp::Mod => TokenKind::Percent,
            CompoundAssignOp::Power => TokenKind::StarStar,
            CompoundAssignOp::Repeat => TokenKind::Ident("x".to_string()),
            CompoundAssignOp::ListRepeat => TokenKind::Ident("xx".to_string()),
            CompoundAssignOp::BitOr => TokenKind::BitOr,
            CompoundAssignOp::BitAnd => TokenKind::BitAnd,
            CompoundAssignOp::BitXor => TokenKind::BitXor,
            CompoundAssignOp::BitShiftLeft => TokenKind::BitShiftLeft,
            CompoundAssignOp::BitShiftRight => TokenKind::BitShiftRight,
            CompoundAssignOp::Min => TokenKind::Ident("min".to_string()),
            CompoundAssignOp::Max => TokenKind::Ident("max".to_string()),
            CompoundAssignOp::KeywordOr => TokenKind::OrWord,
            CompoundAssignOp::KeywordAnd => TokenKind::AndAnd,
            CompoundAssignOp::Orelse => TokenKind::OrElse,
            CompoundAssignOp::Andthen => TokenKind::AndThen,
            CompoundAssignOp::IntDiv => TokenKind::Ident("div".to_string()),
            CompoundAssignOp::Lcm => TokenKind::Ident("lcm".to_string()),
            CompoundAssignOp::Gcd => TokenKind::Ident("gcd".to_string()),
            CompoundAssignOp::StrBitAnd => TokenKind::StrBitAnd,
            CompoundAssignOp::StrBitOr => TokenKind::StrBitOr,
            CompoundAssignOp::StrBitXor => TokenKind::StrBitXor,
            CompoundAssignOp::StrShiftLeft => TokenKind::StrShiftLeft,
            CompoundAssignOp::StrShiftRight => TokenKind::StrShiftRight,
            CompoundAssignOp::BoolBitAnd => TokenKind::BoolBitAnd,
            CompoundAssignOp::BoolBitOr => TokenKind::BoolBitOr,
            CompoundAssignOp::BoolBitXor => TokenKind::BoolBitXor,
            CompoundAssignOp::XorXor => TokenKind::XorXor,
        }
    }
}

fn autoviv_compound_lhs(lhs: Expr, op: CompoundAssignOp) -> Expr {
    if matches!(op, CompoundAssignOp::Mul | CompoundAssignOp::Power) {
        Expr::Ternary {
            cond: Box::new(Expr::Call {
                name: Symbol::intern("defined"),
                args: vec![lhs.clone()],
            }),
            then_expr: Box::new(lhs),
            else_expr: Box::new(Expr::Literal(Value::Int(1))),
        }
    } else {
        lhs
    }
}

pub(crate) fn compound_assigned_value_expr(lhs: Expr, op: CompoundAssignOp, rhs: Expr) -> Expr {
    if matches!(op, CompoundAssignOp::DefinedOr) {
        let tmp_name = format!(
            "__mutsu_compound_lhs_{}",
            TMP_INDEX_COUNTER.fetch_add(1, Ordering::Relaxed)
        );
        let tmp_var = Expr::Var(tmp_name.clone());
        Expr::Ternary {
            cond: Box::new(Expr::DoBlock {
                body: vec![
                    Stmt::VarDecl {
                        name: tmp_name.clone(),
                        expr: lhs,
                        type_constraint: None,
                        is_state: false,
                        is_our: false,
                        is_dynamic: false,
                        is_export: false,
                        export_tags: Vec::new(),
                        custom_traits: Vec::new(),
                        where_constraint: None,
                    },
                    Stmt::Expr(Expr::Call {
                        name: Symbol::intern("defined"),
                        args: vec![tmp_var.clone()],
                    }),
                ],
                label: None,
            }),
            then_expr: Box::new(tmp_var),
            else_expr: Box::new(rhs),
        }
    } else if matches!(op, CompoundAssignOp::Andthen) {
        // andthen=: assign RHS only when LHS is defined, else keep LHS
        let tmp_name = format!(
            "__mutsu_compound_lhs_{}",
            TMP_INDEX_COUNTER.fetch_add(1, Ordering::Relaxed)
        );
        let tmp_var = Expr::Var(tmp_name.clone());
        Expr::Ternary {
            cond: Box::new(Expr::DoBlock {
                body: vec![
                    Stmt::VarDecl {
                        name: tmp_name.clone(),
                        expr: lhs,
                        type_constraint: None,
                        is_state: false,
                        is_our: false,
                        is_dynamic: false,
                        is_export: false,
                        export_tags: Vec::new(),
                        custom_traits: Vec::new(),
                        where_constraint: None,
                    },
                    Stmt::Expr(Expr::Call {
                        name: Symbol::intern("defined"),
                        args: vec![tmp_var.clone()],
                    }),
                ],
                label: None,
            }),
            then_expr: Box::new(rhs),
            else_expr: Box::new(tmp_var),
        }
    } else {
        Expr::Binary {
            left: Box::new(autoviv_compound_lhs(lhs, op)),
            op: op.token_kind(),
            right: Box::new(rhs),
        }
    }
}

pub(super) const COMPOUND_ASSIGN_OPS: &[CompoundAssignOp] = &[
    CompoundAssignOp::Comma,
    CompoundAssignOp::DefinedOr,
    CompoundAssignOp::LogicalOr,
    CompoundAssignOp::LogicalAnd,
    CompoundAssignOp::XorXor, // ^^= before ^=
    CompoundAssignOp::Power,  // **= before *= to match longest first
    CompoundAssignOp::Add,
    CompoundAssignOp::Sub,
    // String bitwise ops before ~= to match longest first
    CompoundAssignOp::StrBitAnd,     // ~&=
    CompoundAssignOp::StrBitOr,      // ~|=
    CompoundAssignOp::StrBitXor,     // ~^=
    CompoundAssignOp::StrShiftLeft,  // ~<=
    CompoundAssignOp::StrShiftRight, // ~>=
    CompoundAssignOp::Concat,
    CompoundAssignOp::Mul,
    CompoundAssignOp::Div,
    CompoundAssignOp::Mod,
    CompoundAssignOp::ListRepeat, // xx= before x= to match longest first
    CompoundAssignOp::Repeat,
    CompoundAssignOp::BitOr,
    CompoundAssignOp::BitAnd,
    CompoundAssignOp::BitXor,
    CompoundAssignOp::BitShiftLeft,  // +<=
    CompoundAssignOp::BitShiftRight, // +>=
    // Boolean bitwise ops
    CompoundAssignOp::BoolBitAnd, // ?&=
    CompoundAssignOp::BoolBitOr,  // ?|=
    CompoundAssignOp::BoolBitXor, // ?^=
    CompoundAssignOp::Min,        // min=
    CompoundAssignOp::Max,        // max=
    CompoundAssignOp::Orelse,     // orelse= before or= to match longest first
    CompoundAssignOp::KeywordOr,  // or=
    CompoundAssignOp::Andthen,    // andthen= before and= to match longest first
    CompoundAssignOp::KeywordAnd, // and=
    CompoundAssignOp::IntDiv,     // div=
    CompoundAssignOp::Lcm,        // lcm=
    CompoundAssignOp::Gcd,        // gcd=
];

pub(crate) fn compound_assign_op_from_name(op: &str) -> Option<CompoundAssignOp> {
    COMPOUND_ASSIGN_OPS.iter().copied().find(|candidate| {
        candidate
            .symbol()
            .strip_suffix('=')
            .unwrap_or(candidate.symbol())
            == op
    })
}

pub(crate) fn parse_compound_assign_op(input: &str) -> Option<(&str, CompoundAssignOp)> {
    for op in COMPOUND_ASSIGN_OPS {
        if let Some(stripped) = input.strip_prefix(op.symbol()) {
            return Some((stripped, *op));
        }
    }
    // Z-prefixed compound assignments (Z+=, Z//=, ...) are handled by
    // parse_meta_compound_assign_op, not here.
    None
}

pub(crate) fn parse_custom_compound_assign_op(input: &str) -> Option<(&str, String)> {
    // Try word-like custom operators (alphabetic/underscore start)
    let mut chars = input.char_indices();
    let (_, first) = chars.next()?;
    if first.is_alphabetic() || first == '_' {
        let mut end = first.len_utf8();
        for (idx, ch) in chars {
            if ch.is_alphanumeric() || ch == '_' || ch == '-' {
                end = idx + ch.len_utf8();
            } else {
                break;
            }
        }
        let name = &input[..end];
        if !matches!(
            name,
            "if" | "unless"
                | "for"
                | "while"
                | "until"
                | "given"
                | "when"
                | "with"
                | "without"
                | "not"
        ) {
            let rest = &input[end..];
            if let Some(rest) = rest.strip_prefix('=') {
                return Some((rest, name.to_string()));
            }
        }
    }

    // Try user-declared symbol infix operators (e.g. ⋅= for infix:<⋅>)
    if let Some((symbol, len)) = super::simple::match_user_declared_infix_symbol_op(input) {
        let rest = &input[len..];
        if let Some(rest) = rest.strip_prefix('=') {
            return Some((rest, symbol));
        }
    }

    None
}

/// Parse set operator compound assignment: `(|)=`, `(&)=`, `(-)=`, `(^)=`, `(.)=`, `(+)=`
/// and their Unicode variants: `∪=`, `∩=`, etc.
/// Returns (rest_after_equals, TokenKind for the set operator).
pub(crate) fn parse_set_compound_assign_op(input: &str) -> Option<(&str, TokenKind)> {
    let (tok, len) = if input.starts_with("(|)") {
        (TokenKind::SetUnion, 3)
    } else if input.starts_with("(&)") {
        (TokenKind::SetIntersect, 3)
    } else if input.starts_with("(.)") {
        (TokenKind::SetMultiply, 3)
    } else if input.starts_with("(-)") {
        (TokenKind::SetDiff, 3)
    } else if input.starts_with("(^)") {
        (TokenKind::SetSymDiff, 3)
    } else if input.starts_with("(+)") {
        (TokenKind::SetAddition, 3)
    } else if input.starts_with('⊎') {
        (TokenKind::SetAddition, '⊎'.len_utf8())
    } else if input.starts_with('∪') {
        (TokenKind::SetUnion, '∪'.len_utf8())
    } else if input.starts_with('∩') {
        (TokenKind::SetIntersect, '∩'.len_utf8())
    } else if input.starts_with('⊍') {
        (TokenKind::SetMultiply, '⊍'.len_utf8())
    } else if input.starts_with('∖') {
        (TokenKind::SetDiff, '∖'.len_utf8())
    } else if input.starts_with('⊖') {
        (TokenKind::SetSymDiff, '⊖'.len_utf8())
    } else {
        return None;
    };
    let after_op = &input[len..];
    if after_op.starts_with('=') && !after_op.starts_with("==") {
        Some((&after_op[1..], tok))
    } else {
        None
    }
}

fn find_matching_bracket(input: &str) -> Option<usize> {
    if !input.starts_with('[') {
        return None;
    }
    let mut depth = 0usize;
    for (i, ch) in input.char_indices() {
        if ch == '[' {
            depth += 1;
        } else if ch == ']' {
            depth = depth.saturating_sub(1);
            if depth == 0 {
                return Some(i);
            }
        }
    }
    None
}

fn flatten_bracket_op(s: &str) -> String {
    if s.is_empty() {
        return String::new();
    }
    let first = s.as_bytes()[0];
    if first == b'['
        && let Some(end) = find_matching_bracket(s)
    {
        let inner = &s[1..end];
        return flatten_bracket_op(inner);
    }
    if (first == b'R' || first == b'X' || first == b'Z')
        && s.len() > 1
        && s.as_bytes()[1] == b'['
        && let Some(end) = find_matching_bracket(&s[1..])
    {
        let inner = &s[2..1 + end];
        let flattened_inner = flatten_bracket_op(inner);
        let rest = &s[1 + end + 1..];
        return format!("{}{}{}", first as char, flattened_inner, rest);
    }
    s.to_string()
}

fn parse_bracket_meta_assign_op(input: &str) -> Option<(&str, String, String)> {
    if !input.starts_with('[') {
        return None;
    }
    let end = find_matching_bracket(input)?;
    let inner = &input[1..end];
    let after_bracket = &input[end + 1..];
    if !after_bracket.starts_with('=') || after_bracket.starts_with("==") {
        return None;
    }
    let flattened = flatten_bracket_op(inner);
    let (meta, op) = if let Some(op) = flattened.strip_prefix('R') {
        ("R", op)
    } else if let Some(op) = flattened.strip_prefix('X') {
        ("X", op)
    } else if let Some(op) = flattened.strip_prefix('Z') {
        ("Z", op)
    } else {
        // Plain reduction compound assignment: [+]= is like +=
        // The reduction of a binary op on two values is just the op itself.
        let rest = &after_bracket[1..];
        return Some((rest, "reduce".to_string(), flattened));
    };
    let rest = &after_bracket[1..];
    Some((rest, meta.to_string(), op.to_string()))
}

pub(crate) fn parse_meta_compound_assign_op(input: &str) -> Option<(&str, String, String)> {
    let (meta, after_meta) = if let Some(rest) = input.strip_prefix('R') {
        ("R", rest)
    } else if let Some(rest) = input.strip_prefix('X') {
        ("X", rest)
    } else if let Some(rest) = input.strip_prefix('Z') {
        ("Z", rest)
    } else {
        return None;
    };
    if after_meta.starts_with('=') && !after_meta.starts_with("==") && !after_meta.starts_with("=>")
    {
        return Some((&after_meta[1..], meta.to_string(), "=".to_string()));
    }
    let (rest, op) = parse_compound_assign_op(after_meta)?;
    let op_name = op
        .symbol()
        .strip_suffix('=')
        .unwrap_or(op.symbol())
        .to_string();
    Some((rest, meta.to_string(), op_name))
}

pub(crate) fn parse_assign_expr_or_comma(input: &str) -> PResult<'_, Expr> {
    // Try to parse a chained assignment: $var op= ...
    if let Ok((rest, assign_expr)) = try_parse_assign_expr(input) {
        // After a chained assign, check for comma list at this level
        let (r, _) = ws(rest)?;
        if r.starts_with(',') && !r.starts_with(",,") {
            let (r, _) = parse_char(r, ',')?;
            let (r, _) = ws(r)?;
            if r.starts_with(';') || r.is_empty() || r.starts_with('}') {
                return Ok((r, Expr::ArrayLiteral(vec![assign_expr])));
            }
            let mut items = vec![assign_expr];
            let (mut r, second) = expression(r)?;
            items.push(second);
            loop {
                let (r2, _) = ws(r)?;
                if !r2.starts_with(',') {
                    return Ok((r2, Expr::ArrayLiteral(items)));
                }
                let (r2, _) = parse_char(r2, ',')?;
                let (r2, _) = ws(r2)?;
                if r2.starts_with(';') || r2.is_empty() || r2.starts_with('}') {
                    return Ok((r2, Expr::ArrayLiteral(items)));
                }
                let (r2, next) = expression(r2)?;
                items.push(next);
                r = r2;
            }
        }
        return Ok((rest, assign_expr));
    }
    parse_comma_or_expr(input)
}

fn method_lvalue_assign_expr(
    target: Expr,
    target_var_name: Option<String>,
    method_name: String,
    method_args: Vec<Expr>,
    value: Expr,
) -> Expr {
    let mut args = vec![
        target,
        Expr::Literal(Value::str(method_name)),
        Expr::ArrayLiteral(method_args),
        value,
    ];
    args.push(match target_var_name {
        Some(name) => Expr::Literal(Value::str(name)),
        None => Expr::Literal(Value::Nil),
    });
    Expr::Call {
        name: Symbol::intern("__mutsu_assign_method_lvalue"),
        args,
    }
}

fn named_sub_lvalue_assign_expr(name: String, call_args: Vec<Expr>, value: Expr) -> Expr {
    Expr::Call {
        name: Symbol::intern("__mutsu_assign_named_sub_lvalue"),
        args: vec![
            Expr::Literal(Value::str(name)),
            Expr::ArrayLiteral(call_args),
            value,
        ],
    }
}

fn callable_lvalue_assign_expr(target: Expr, call_args: Vec<Expr>, value: Expr) -> Expr {
    Expr::Call {
        name: Symbol::intern("__mutsu_assign_callable_lvalue"),
        args: vec![target, Expr::ArrayLiteral(call_args), value],
    }
}

fn subscript_adverb_lvalue_assign_expr(lhs: Expr, rhs: Expr) -> Option<Expr> {
    fn subscript_parts(expr: &Expr) -> Option<(Expr, Expr, String)> {
        let Expr::Call { name, args } = expr else {
            return None;
        };
        if name != "__mutsu_subscript_adverb" || args.len() < 3 {
            return None;
        }
        let Expr::Literal(Value::Str(mode)) = &args[2] else {
            return None;
        };
        Some((args[0].clone(), args[1].clone(), mode.to_string()))
    }

    match lhs {
        Expr::MethodCall {
            target,
            name,
            args,
            modifier: _,
            quoted: _,
        } if name == "value" && args.is_empty() => {
            if let Some((base_target, base_index, mode)) = subscript_parts(target.as_ref())
                && (mode == "p" || mode == "not-p")
            {
                return Some(Expr::IndexAssign {
                    target: Box::new(base_target),
                    index: Box::new(base_index),
                    value: Box::new(rhs),
                });
            }
            None
        }
        Expr::Index { target, index } => {
            let (base_target, base_index, mode) = subscript_parts(target.as_ref())?;
            if mode != "kv" && mode != "not-kv" {
                return None;
            }
            if !matches!(*index, Expr::Literal(Value::Int(1))) {
                return None;
            }
            Some(Expr::IndexAssign {
                target: Box::new(base_target),
                index: Box::new(base_index),
                value: Box::new(rhs),
            })
        }
        _ => None,
    }
}

fn list_lvalue_assign_expr(items: Vec<Expr>, rhs: Expr) -> Option<Expr> {
    let mut saw_whatever = false;
    let mut lvalues: Vec<Expr> = Vec::new();
    for item in items {
        if matches!(item, Expr::Whatever) {
            saw_whatever = true;
            continue;
        }
        lvalues.push(item);
    }

    if lvalues.len() != 1 {
        return None;
    }
    if !saw_whatever {
        return None;
    }

    match lvalues.into_iter().next()? {
        Expr::Var(name) => Some(Expr::AssignExpr {
            name,
            expr: Box::new(rhs),
        }),
        Expr::ArrayVar(name) => Some(Expr::AssignExpr {
            name: format!("@{}", name),
            expr: Box::new(rhs),
        }),
        Expr::HashVar(name) => Some(Expr::AssignExpr {
            name: format!("%{}", name),
            expr: Box::new(rhs),
        }),
        Expr::Index { target, index } => Some(Expr::IndexAssign {
            target,
            index,
            value: Box::new(rhs),
        }),
        _ => None,
    }
}

fn compound_index_assign_expr<F>(target: Expr, index: Expr, build_assigned_value: F) -> Expr
where
    F: FnOnce(Expr) -> Expr,
{
    let tmp_idx = format!(
        "__mutsu_idx_{}",
        TMP_INDEX_COUNTER.fetch_add(1, Ordering::Relaxed)
    );
    let tmp_idx_expr = Expr::Var(tmp_idx.clone());
    let lhs_expr = Expr::Index {
        target: Box::new(target.clone()),
        index: Box::new(tmp_idx_expr.clone()),
    };
    let assigned_value = build_assigned_value(lhs_expr);
    Expr::DoBlock {
        body: vec![
            Stmt::VarDecl {
                name: tmp_idx.clone(),
                expr: index,
                type_constraint: None,
                is_state: false,
                is_our: false,
                is_dynamic: false,
                is_export: false,
                export_tags: Vec::new(),
                custom_traits: Vec::new(),
                where_constraint: None,
            },
            Stmt::Expr(Expr::IndexAssign {
                target: Box::new(target),
                index: Box::new(tmp_idx_expr.clone()),
                value: Box::new(assigned_value),
            }),
        ],
        label: None,
    }
}

pub(crate) fn build_compound_assign_expr(
    lhs: Expr,
    op: CompoundAssignOp,
    rhs: Expr,
) -> Result<Expr, PError> {
    Ok(match lhs {
        Expr::AssignExpr { name, expr } => {
            // ($x += 2) *= 3 → first evaluate inner assign, then apply outer op
            // to the variable's value and assign back.
            // This becomes: { let _ = ($x = $x + 2); $x = $x * 3 }
            Expr::AssignExpr {
                name: name.clone(),
                expr: Box::new(Expr::Binary {
                    left: Box::new(Expr::AssignExpr { name, expr }),
                    op: op.token_kind(),
                    right: Box::new(rhs),
                }),
            }
        }
        Expr::Var(name) => Expr::AssignExpr {
            name: name.clone(),
            expr: Box::new(compound_assigned_value_expr(Expr::Var(name), op, rhs)),
        },
        Expr::ArrayVar(name) => Expr::AssignExpr {
            name: format!("@{}", name),
            expr: Box::new(compound_assigned_value_expr(Expr::ArrayVar(name), op, rhs)),
        },
        Expr::HashVar(name) => Expr::AssignExpr {
            name: format!("%{}", name),
            expr: Box::new(compound_assigned_value_expr(Expr::HashVar(name), op, rhs)),
        },
        Expr::Index { target, index } => {
            return Ok(compound_index_assign_expr(*target, *index, |lhs_expr| {
                compound_assigned_value_expr(lhs_expr, op, rhs)
            }));
        }
        Expr::MethodCall {
            target,
            name,
            args,
            modifier: _,
            quoted: _,
        } if name == "AT-POS" && args.len() == 1 => {
            let index = args.into_iter().next().unwrap_or(Expr::Literal(Value::Nil));
            return Ok(compound_index_assign_expr(*target, index, |lhs_expr| {
                compound_assigned_value_expr(lhs_expr, op, rhs)
            }));
        }
        Expr::MethodCall {
            target,
            name,
            args,
            modifier: _,
            quoted: _,
        } => {
            let target_var_name = match target.as_ref() {
                Expr::Var(name) => Some(name.clone()),
                Expr::ArrayVar(name) => Some(format!("@{}", name)),
                Expr::HashVar(name) => Some(format!("%{}", name)),
                _ => None,
            };
            let current_value = Expr::MethodCall {
                target: Box::new((*target).clone()),
                name,
                args: args.clone(),
                modifier: None,
                quoted: false,
            };
            let assigned_value = compound_assigned_value_expr(current_value, op, rhs);
            method_lvalue_assign_expr(
                *target,
                target_var_name,
                name.resolve(),
                args,
                assigned_value,
            )
        }
        Expr::BracketArray(items, tc) => Expr::Binary {
            left: Box::new(Expr::BracketArray(items, tc)),
            op: op.token_kind(),
            right: Box::new(rhs),
        },
        other => {
            // For short-circuit operators (or=, and=, ||=, &&=, //=, orelse=,
            // andthen=), preserve short-circuit semantics so that when the LHS
            // triggers short-circuit the RHS is never evaluated and no
            // assignment error is raised.
            if matches!(
                op,
                CompoundAssignOp::KeywordOr
                    | CompoundAssignOp::KeywordAnd
                    | CompoundAssignOp::LogicalOr
                    | CompoundAssignOp::LogicalAnd
                    | CompoundAssignOp::DefinedOr
                    | CompoundAssignOp::Orelse
                    | CompoundAssignOp::Andthen
            ) {
                Expr::Binary {
                    left: Box::new(other),
                    op: op.token_kind(),
                    right: Box::new(Expr::DoBlock {
                        body: vec![
                            Stmt::Expr(rhs),
                            Stmt::Expr(Expr::Call {
                                name: Symbol::intern("__mutsu_assignment_ro"),
                                args: Vec::new(),
                            }),
                        ],
                        label: None,
                    }),
                }
            } else {
                Expr::DoBlock {
                    body: vec![
                        Stmt::Expr(other),
                        Stmt::Expr(rhs),
                        Stmt::Expr(Expr::Call {
                            name: Symbol::intern("__mutsu_assignment_ro"),
                            args: Vec::new(),
                        }),
                    ],
                    label: None,
                }
            }
        }
    })
}

fn build_custom_compound_assign_expr(
    lhs: Expr,
    op_name: String,
    rhs: Expr,
) -> Result<Expr, PError> {
    Ok(match lhs {
        Expr::Var(name) => Expr::AssignExpr {
            name: name.clone(),
            expr: Box::new(Expr::InfixFunc {
                name: op_name,
                left: Box::new(Expr::Var(name)),
                right: vec![rhs],
                modifier: None,
            }),
        },
        Expr::ArrayVar(name) => Expr::AssignExpr {
            name: format!("@{}", name.clone()),
            expr: Box::new(Expr::InfixFunc {
                name: op_name,
                left: Box::new(Expr::ArrayVar(name)),
                right: vec![rhs],
                modifier: None,
            }),
        },
        Expr::HashVar(name) => Expr::AssignExpr {
            name: format!("%{}", name.clone()),
            expr: Box::new(Expr::InfixFunc {
                name: op_name,
                left: Box::new(Expr::HashVar(name)),
                right: vec![rhs],
                modifier: None,
            }),
        },
        Expr::Index { target, index } => {
            return Ok(compound_index_assign_expr(*target, *index, |lhs_expr| {
                Expr::InfixFunc {
                    name: op_name,
                    left: Box::new(lhs_expr),
                    right: vec![rhs],
                    modifier: None,
                }
            }));
        }
        _ => return Err(PError::expected("assignment expression")),
    })
}

fn build_meta_assign_expr(lhs: Expr, meta: String, op: String, rhs: Expr) -> Result<Expr, PError> {
    // For "reduce" meta (plain [op]=), reduce on two values is just the base op.
    if meta == "reduce"
        && let Some(compound_op) = CompoundAssignOp::from_op_name(&op)
    {
        return build_compound_assign_expr(lhs, compound_op, rhs);
    }
    Ok(match lhs {
        Expr::Var(name) => Expr::AssignExpr {
            name: name.clone(),
            expr: Box::new(Expr::MetaOp {
                meta,
                op,
                left: Box::new(Expr::Var(name)),
                right: Box::new(rhs),
            }),
        },
        Expr::ArrayVar(name) => Expr::AssignExpr {
            name: format!("@{}", name.clone()),
            expr: Box::new(Expr::MetaOp {
                meta,
                op,
                left: Box::new(Expr::ArrayVar(name)),
                right: Box::new(rhs),
            }),
        },
        Expr::HashVar(name) => Expr::AssignExpr {
            name: format!("%{}", name.clone()),
            expr: Box::new(Expr::MetaOp {
                meta,
                op,
                left: Box::new(Expr::HashVar(name)),
                right: Box::new(rhs),
            }),
        },
        Expr::Index { target, index } => {
            let lhs_expr = Expr::Index {
                target: target.clone(),
                index: index.clone(),
            };
            Expr::IndexAssign {
                target,
                index,
                value: Box::new(Expr::MetaOp {
                    meta,
                    op,
                    left: Box::new(lhs_expr),
                    right: Box::new(rhs),
                }),
            }
        }
        _ => return Err(PError::expected("assignment expression")),
    })
}

fn parenthesized_assign_expr(input: &str) -> PResult<'_, Expr> {
    let (rest, _) = parse_char(input, '(')?;
    let (rest, _) = ws(rest)?;
    if let Ok((rest_inner, inner_assign)) = try_parse_assign_expr(rest) {
        let (rest_inner, _) = ws(rest_inner)?;
        if let Ok((rest_after_paren, _)) = parse_char(rest_inner, ')') {
            return Ok((rest_after_paren, inner_assign));
        }
        // If the inner assignment consumed part but left comma-separated items before ')',
        // collect them into a list (e.g. `(@a[1,2] := "a","b")` where RHS is a list).
        if rest_inner.starts_with(',') && !rest_inner.starts_with(",,") {
            // Re-wrap: the inner assignment's RHS should include the remaining items
            if let Expr::IndexAssign {
                target,
                index,
                value,
            } = inner_assign
            {
                let mut items = vec![*value];
                let mut r = rest_inner;
                while r.starts_with(',') && !r.starts_with(",,") {
                    let (r2, _) = parse_char(r, ',')?;
                    let (r2, _) = ws(r2)?;
                    if r2.starts_with(')') {
                        r = r2;
                        break;
                    }
                    let (r2, item) = expression_no_sequence(r2)?;
                    items.push(item);
                    let (r2, _) = ws(r2)?;
                    r = r2;
                }
                let (r, _) = parse_char(r, ')')?;
                return Ok((
                    r,
                    Expr::IndexAssign {
                        target,
                        index,
                        value: Box::new(Expr::ArrayLiteral(items)),
                    },
                ));
            } else if let Expr::AssignExpr { name, expr } = inner_assign {
                let mut items = vec![*expr];
                let mut r = rest_inner;
                while r.starts_with(',') && !r.starts_with(",,") {
                    let (r2, _) = parse_char(r, ',')?;
                    let (r2, _) = ws(r2)?;
                    if r2.starts_with(')') {
                        r = r2;
                        break;
                    }
                    let (r2, item) = expression_no_sequence(r2)?;
                    items.push(item);
                    let (r2, _) = ws(r2)?;
                    r = r2;
                }
                let (r, _) = parse_char(r, ')')?;
                return Ok((
                    r,
                    Expr::AssignExpr {
                        name,
                        expr: Box::new(Expr::ArrayLiteral(items)),
                    },
                ));
            }
        }
    }
    let (rest, lhs) = expression_no_sequence(rest)?;
    let (rest, _) = ws(rest)?;
    if let Some(stripped) = rest.strip_prefix("⚛+=") {
        let name = match lhs {
            Expr::Var(name) => name,
            _ => return Err(PError::expected("atomic compound assignment expression")),
        };
        let (rest, _) = ws(stripped)?;
        let (rest, rhs) = match try_parse_assign_expr(rest) {
            Ok(r) => r,
            Err(_) => expression_no_sequence(rest)?,
        };
        let (rest, _) = ws(rest)?;
        let (rest, _) = parse_char(rest, ')')?;
        return Ok((
            rest,
            Expr::Call {
                name: Symbol::intern("__mutsu_atomic_add_var"),
                args: vec![Expr::Literal(Value::str(name)), rhs],
            },
        ));
    }
    if let Some((stripped, op)) = parse_compound_assign_op(rest) {
        let (rest, _) = ws(stripped)?;
        let (rest, rhs) = match try_parse_assign_expr(rest) {
            Ok(r) => r,
            Err(_) => expression_no_sequence(rest)?,
        };
        let (rest, _) = ws(rest)?;
        let (rest, _) = parse_char(rest, ')')?;
        return Ok((rest, build_compound_assign_expr(lhs, op, rhs)?));
    }
    if let Some((stripped, op_name)) = parse_custom_compound_assign_op(rest) {
        let (rest, _) = ws(stripped)?;
        let (rest, rhs) = match try_parse_assign_expr(rest) {
            Ok(r) => r,
            Err(_) => expression_no_sequence(rest)?,
        };
        let (rest, _) = ws(rest)?;
        let (rest, _) = parse_char(rest, ')')?;
        return Ok((rest, build_custom_compound_assign_expr(lhs, op_name, rhs)?));
    }
    if let Some((stripped, meta, op)) = parse_bracket_meta_assign_op(rest) {
        let (rest, _) = ws(stripped)?;
        let (rest, rhs) = match try_parse_assign_expr(rest) {
            Ok(r) => r,
            Err(_) => expression_no_sequence(rest)?,
        };
        let (rest, _) = ws(rest)?;
        let (rest, _) = parse_char(rest, ')')?;
        return Ok((rest, build_meta_assign_expr(lhs, meta, op, rhs)?));
    }
    if let Some(stripped) = rest.strip_prefix("::=").or_else(|| rest.strip_prefix(":=")) {
        let (rest, _) = ws(stripped)?;
        let (rest, rhs) = match try_parse_assign_expr(rest) {
            Ok(r) => r,
            Err(_) => {
                let (r, first) = expression_no_sequence(rest)?;
                let (r2, _) = ws(r)?;
                if r2.starts_with(',') && !r2.starts_with(",,") {
                    let mut items = vec![first];
                    let mut r = r2;
                    while r.starts_with(',') && !r.starts_with(",,") {
                        let (r2, _) = parse_char(r, ',')?;
                        let (r2, _) = ws(r2)?;
                        if r2.starts_with(')') {
                            r = r2;
                            break;
                        }
                        let (r2, item) = expression_no_sequence(r2)?;
                        items.push(item);
                        let (r2, _) = ws(r2)?;
                        r = r2;
                    }
                    (r, Expr::ArrayLiteral(items))
                } else {
                    (r, first)
                }
            }
        };
        let (rest, _) = ws(rest)?;
        let (rest, _) = parse_char(rest, ')')?;
        let expr = match lhs {
            Expr::Var(name) => Expr::AssignExpr {
                name,
                expr: Box::new(rhs),
            },
            Expr::ArrayVar(name) => Expr::AssignExpr {
                name: format!("@{}", name),
                expr: Box::new(rhs),
            },
            Expr::HashVar(name) => Expr::AssignExpr {
                name: format!("%{}", name),
                expr: Box::new(rhs),
            },
            Expr::Index { target, index } => Expr::IndexAssign {
                target,
                index,
                value: Box::new(rhs),
            },
            Expr::MultiDimIndex { target, dimensions } => Expr::MultiDimIndexAssign {
                target,
                dimensions,
                value: Box::new(rhs),
            },
            _ => return Err(PError::expected("assignment expression")),
        };
        return Ok((rest, expr));
    }
    if (!rest.starts_with('=') || rest.starts_with("==") || rest.starts_with("=>"))
        && !rest.starts_with("⚛=")
    {
        return Err(PError::expected("assignment expression"));
    }
    let is_atomic = rest.starts_with("⚛=");
    let rest = if is_atomic {
        &rest["⚛=".len()..]
    } else {
        &rest[1..]
    };
    let (rest, _) = ws(rest)?;
    // Parse RHS: try chained assignment first, then comma-separated list for multi-value
    // assignment (e.g. `(%h{|| @a} = 42,666)`).
    let (rest, rhs) = match try_parse_assign_expr(rest) {
        Ok(r) => r,
        Err(_) => {
            let (r, first) = expression_no_sequence(rest)?;
            let (r2, _) = ws(r)?;
            if r2.starts_with(',') && !r2.starts_with(",,") {
                // Comma-separated RHS: collect into an ArrayLiteral
                let mut items = vec![first];
                let mut r = r2;
                while r.starts_with(',') && !r.starts_with(",,") {
                    let (r2, _) = parse_char(r, ',')?;
                    let (r2, _) = ws(r2)?;
                    if r2.starts_with(')') {
                        r = r2;
                        break;
                    }
                    let (r2, item) = expression_no_sequence(r2)?;
                    items.push(item);
                    let (r2, _) = ws(r2)?;
                    r = r2;
                }
                (r, Expr::ArrayLiteral(items))
            } else {
                (r, first)
            }
        }
    };
    let (rest, _) = ws(rest)?;
    let (rest, _) = parse_char(rest, ')')?;
    if let Some(expr) = subscript_adverb_lvalue_assign_expr(lhs.clone(), rhs.clone()) {
        return Ok((rest, expr));
    }
    let expr = match lhs {
        Expr::Var(name) => Expr::AssignExpr {
            name,
            expr: Box::new(rhs),
        },
        Expr::ArrayVar(name) => Expr::AssignExpr {
            name: format!("@{}", name),
            expr: Box::new(rhs),
        },
        Expr::HashVar(name) => Expr::AssignExpr {
            name: format!("%{}", name),
            expr: Box::new(rhs),
        },
        Expr::Index { target, index } => Expr::IndexAssign {
            target,
            index,
            value: Box::new(rhs),
        },
        Expr::MultiDimIndex { target, dimensions } => Expr::MultiDimIndexAssign {
            target,
            dimensions,
            value: Box::new(rhs),
        },
        Expr::MethodCall {
            target,
            name,
            args,
            modifier: _,
            quoted: _,
        } => {
            if name == "AT-POS" && args.len() == 1 {
                Expr::IndexAssign {
                    target,
                    index: Box::new(args[0].clone()),
                    value: Box::new(rhs),
                }
            } else {
                let target_var_name = match target.as_ref() {
                    Expr::Var(name) => Some(name.clone()),
                    Expr::ArrayVar(name) => Some(format!("@{}", name)),
                    Expr::HashVar(name) => Some(format!("%{}", name)),
                    _ => None,
                };
                method_lvalue_assign_expr(*target, target_var_name, name.resolve(), args, rhs)
            }
        }
        Expr::Call { name, args } => named_sub_lvalue_assign_expr(name.resolve(), args, rhs),
        Expr::CallOn { target, args } => {
            if args.is_empty() {
                if let Expr::ArrayLiteral(items) = *target.clone() {
                    if let Some(expr) = list_lvalue_assign_expr(items, rhs.clone()) {
                        expr
                    } else {
                        callable_lvalue_assign_expr(*target, args, rhs)
                    }
                } else {
                    callable_lvalue_assign_expr(*target, args, rhs)
                }
            } else {
                callable_lvalue_assign_expr(*target, args, rhs)
            }
        }
        Expr::ArrayLiteral(items) => {
            if let Some(expr) = list_lvalue_assign_expr(items, rhs) {
                expr
            } else {
                return Err(PError::expected("assignment expression"));
            }
        }
        Expr::BareWord(name) => Expr::AssignExpr {
            name,
            expr: Box::new(rhs),
        },
        // Fallback for other lvalue expressions (e.g. HyperIndex %h{|| @a})
        other => callable_lvalue_assign_expr(other, Vec::new(), rhs),
    };
    if is_atomic {
        if let Expr::AssignExpr { name, expr } = expr {
            return Ok((
                rest,
                Expr::Call {
                    name: Symbol::intern("__mutsu_atomic_store_var"),
                    args: vec![Expr::Literal(Value::str(name)), *expr],
                },
            ));
        }
        return Err(PError::expected("atomic assignment expression"));
    }
    Ok((rest, expr))
}

fn looks_like_parenthesized_assignment(input: &str) -> bool {
    if !input.starts_with('(') {
        return false;
    }
    let mut chars = input.char_indices().peekable();
    let mut depth = 0usize;
    let mut in_single = false;
    let mut in_double = false;
    while let Some((idx, ch)) = chars.next() {
        if in_single {
            if ch == '\'' {
                in_single = false;
            }
            continue;
        }
        if in_double {
            if ch == '\\' {
                let _ = chars.next();
                continue;
            }
            if ch == '"' {
                in_double = false;
            }
            continue;
        }
        match ch {
            '\'' => in_single = true,
            '"' => in_double = true,
            '(' => depth += 1,
            ')' => {
                depth = depth.saturating_sub(1);
                if depth == 0 {
                    break;
                }
            }
            ':' if depth == 1 => {
                if input[idx + ch.len_utf8()..].starts_with('=') {
                    return true;
                }
            }
            '=' if depth == 1 => {
                let next = input[idx + ch.len_utf8()..].chars().next();
                if !matches!(next, Some('=') | Some('>')) {
                    return true;
                }
            }
            _ => {}
        }
    }
    false
}

/// Try to parse a single assignment expression: $var op= expr or $var = expr.
/// Returns the expression as Expr::AssignExpr.
pub(in crate::parser) fn try_parse_assign_expr(input: &str) -> PResult<'_, Expr> {
    if input.starts_with('(') {
        if !looks_like_parenthesized_assignment(input) {
            return Err(PError::expected("assignment expression"));
        }
        return parenthesized_assign_expr(input);
    }
    let sigil = input.as_bytes().first().copied().unwrap_or(0);
    if sigil == b'$' && input.as_bytes().get(1).is_some_and(|c| *c == b'=') {
        return Err(PError::expected("assignment expression"));
    }
    if sigil != b'$' && sigil != b'@' && sigil != b'%' {
        return Err(PError::expected("assignment expression"));
    }
    let (r, var) = var_name(input)?;

    // Handle subscripted lvalues: @a[1] = ..., %h{key} = ..., $a[0] = ...
    if r.starts_with('[') || r.starts_with('{') || r.starts_with('<') {
        let closing = match r.as_bytes()[0] {
            b'[' => ']',
            b'{' => '}',
            _ => '>',
        };
        let (r_idx, _) = parse_char(r, r.as_bytes()[0] as char)?;
        let (r_idx, _) = ws(r_idx)?;
        // Parse comma-separated index expressions inside brackets
        let (r_idx, first_expr) = expression(r_idx)?;
        let (mut r_idx, _) = ws(r_idx)?;
        let index_expr = if r_idx.starts_with(',') {
            let mut items = vec![first_expr];
            while r_idx.starts_with(',') {
                let (r2, _) = parse_char(r_idx, ',')?;
                let (r2, _) = ws(r2)?;
                if r2.starts_with(closing) {
                    r_idx = r2;
                    break;
                }
                let (r2, next) = expression(r2)?;
                items.push(next);
                let (r2, _) = ws(r2)?;
                r_idx = r2;
            }
            Expr::ArrayLiteral(items)
        } else {
            first_expr
        };
        let (r_idx, _) = parse_char(r_idx, closing)?;
        let (r_after, _) = ws(r_idx)?;
        // Check for binding assignment (:= or ::=)
        if let Some(stripped) = r_after
            .strip_prefix("::=")
            .or_else(|| r_after.strip_prefix(":="))
        {
            let (rest, _) = ws(stripped)?;
            let (rest, rhs) = match try_parse_assign_expr(rest) {
                Ok(r) => r,
                Err(_) => expression(rest)?,
            };
            let target = match sigil {
                b'@' => Expr::ArrayVar(var.to_string()),
                b'%' => Expr::HashVar(var.to_string()),
                _ => Expr::Var(var.to_string()),
            };
            return Ok((
                rest,
                Expr::IndexAssign {
                    target: Box::new(target),
                    index: Box::new(index_expr),
                    value: Box::new(rhs),
                },
            ));
        }
        // Check for simple assignment
        if (r_after.starts_with('=') && !r_after.starts_with("==") && !r_after.starts_with("=>"))
            || r_after.starts_with("⚛=")
        {
            let r3 = if let Some(stripped) = r_after.strip_prefix("⚛=") {
                stripped
            } else {
                &r_after[1..]
            };
            let (rest, _) = ws(r3)?;
            let (rest, rhs) = match try_parse_assign_expr(rest) {
                Ok(r) => r,
                Err(_) => expression(rest)?,
            };
            let target = match sigil {
                b'@' => Expr::ArrayVar(var.to_string()),
                b'%' => Expr::HashVar(var.to_string()),
                _ => Expr::Var(var.to_string()),
            };
            return Ok((
                rest,
                Expr::IndexAssign {
                    target: Box::new(target),
                    index: Box::new(index_expr),
                    value: Box::new(rhs),
                },
            ));
        }
        return Err(PError::expected("assignment expression"));
    }

    let (r2, _) = ws(r)?;
    let prefix = match sigil {
        b'@' => "@",
        b'%' => "%",
        _ => "",
    };
    // .= mutating method call: $var .= method(args) => $var = $var.method(args)
    if let Some(stripped) = r2.strip_prefix(".=") {
        let (r, _) = ws(stripped)?;
        let name = format!("{}{}", prefix, var);
        let method_target = match sigil {
            b'@' => Expr::ArrayVar(var.to_string()),
            b'%' => Expr::HashVar(var.to_string()),
            _ => Expr::Var(var.to_string()),
        };
        // Check for quoted method name: .="method"() or .='method'()
        if let Some((r_after_quote, qname)) = parse_quoted_method_name(r) {
            // Quoted method names require parenthesized arguments
            let (r_after_quote, _) = ws(r_after_quote)?;
            let (rest, args) = if r_after_quote.starts_with('(') {
                let (r2, _) = parse_char(r_after_quote, '(')?;
                let (r2, _) = ws(r2)?;
                let (r2, a) = parse_call_arg_list(r2)?;
                let (r2, _) = ws(r2)?;
                let (r2, _) = parse_char(r2, ')')?;
                (r2, a)
            } else {
                (r_after_quote, vec![])
            };
            let method_expr = match qname {
                QuotedMethodName::Static(mname) => Expr::MethodCall {
                    target: Box::new(method_target),
                    name: Symbol::intern(&mname),
                    args,
                    modifier: None,
                    quoted: true,
                },
                QuotedMethodName::Dynamic(name_expr) => Expr::DynamicMethodCall {
                    target: Box::new(method_target),
                    name_expr: Box::new(name_expr),
                    args,
                },
            };
            return Ok((
                rest,
                Expr::AssignExpr {
                    name,
                    expr: Box::new(method_expr),
                },
            ));
        }
        // Parse regular method name
        let (r, method_name) =
            take_while1(r, |c: char| c.is_alphanumeric() || c == '_' || c == '-')?;
        let (r, _) = ws(r)?;
        // Parse optional args in parens or colon-form
        let (rest, args) = if r.starts_with('(') {
            let (r, _) = parse_char(r, '(')?;
            let (r, _) = ws(r)?;
            if r.starts_with(')') {
                let (r, _) = parse_char(r, ')')?;
                (r, vec![])
            } else {
                let mut args = Vec::new();
                let (mut r, first) = expression_no_sequence(r)?;
                args.push(first);
                loop {
                    let (r2, _) = ws(r)?;
                    if !r2.starts_with(',') {
                        r = r2;
                        break;
                    }
                    let (r2, _) = ws(&r2[1..])?;
                    let (r2, next) = expression_no_sequence(r2)?;
                    args.push(next);
                    r = r2;
                }
                let (r, _) = ws(r)?;
                let (r, _) = parse_char(r, ')')?;
                (r, args)
            }
        } else if r.starts_with(':') && !r.starts_with("::") {
            // Colon-arg syntax: .=method: arg, arg2
            let r = &r[1..];
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
            (r, vec![])
        };
        return Ok((
            rest,
            Expr::AssignExpr {
                name,
                expr: Box::new(Expr::MethodCall {
                    target: Box::new(method_target),
                    name: Symbol::intern(method_name),
                    args,
                    modifier: None,
                    quoted: false,
                }),
            },
        ));
    }
    if let Some((stripped, op)) = parse_compound_assign_op(r2) {
        let (rest, _) = ws(stripped)?;
        // RHS: try chained assign, else single expression
        let (rest, rhs) = match try_parse_assign_expr(rest) {
            Ok(r) => r,
            Err(_) => expression(rest)?,
        };
        let name = format!("{}{}", prefix, var);
        return Ok((
            rest,
            Expr::AssignExpr {
                name,
                expr: Box::new(compound_assigned_value_expr(
                    Expr::Var(var.to_string()),
                    op,
                    rhs,
                )),
            },
        ));
    }
    if let Some((stripped, meta, op)) = parse_meta_compound_assign_op(r2) {
        let (rest, _) = ws(stripped)?;
        let (rest, rhs) = match try_parse_assign_expr(rest) {
            Ok(r) => r,
            Err(_) => expression_no_sequence(rest)?,
        };
        let name = format!("{}{}", prefix, var);
        // For scalar variables with Z-meta, fall back to plain compound assignment
        // since scalar Zip on single values is equivalent to the base operator.
        // This avoids Z+ returning an array when assigning to a scalar.
        if meta == "Z"
            && sigil == b'$'
            && let Some(compound_op) = CompoundAssignOp::from_op_name(&op)
        {
            return Ok((
                rest,
                Expr::AssignExpr {
                    name,
                    expr: Box::new(compound_assigned_value_expr(
                        Expr::Var(var.to_string()),
                        compound_op,
                        rhs,
                    )),
                },
            ));
        }
        let var_expr = match sigil {
            b'@' => Expr::ArrayVar(var.to_string()),
            b'%' => Expr::HashVar(var.to_string()),
            _ => Expr::Var(name.clone()),
        };
        return Ok((
            rest,
            Expr::AssignExpr {
                name,
                expr: Box::new(Expr::MetaOp {
                    meta,
                    op,
                    left: Box::new(var_expr),
                    right: Box::new(rhs),
                }),
            },
        ));
    }
    if let Some((stripped, meta, op)) = parse_bracket_meta_assign_op(r2) {
        let (rest, _) = ws(stripped)?;
        let (rest, rhs) = match try_parse_assign_expr(rest) {
            Ok(r) => r,
            Err(_) => expression_no_sequence(rest)?,
        };
        let name = format!("{}{}", prefix, var);
        let var_expr = match sigil {
            b'@' => Expr::ArrayVar(var.to_string()),
            b'%' => Expr::HashVar(var.to_string()),
            _ => Expr::Var(name.clone()),
        };
        return Ok((
            rest,
            Expr::AssignExpr {
                name,
                expr: Box::new(Expr::MetaOp {
                    meta,
                    op,
                    left: Box::new(var_expr),
                    right: Box::new(rhs),
                }),
            },
        ));
    }
    if let Some((stripped, op_name)) = parse_custom_compound_assign_op(r2) {
        let (rest, _) = ws(stripped)?;
        let (rest, rhs) = match try_parse_assign_expr(rest) {
            Ok(r) => r,
            Err(_) => expression(rest)?,
        };
        let name = format!("{}{}", prefix, var);
        return Ok((
            rest,
            Expr::AssignExpr {
                name: name.clone(),
                expr: Box::new(Expr::InfixFunc {
                    name: op_name,
                    left: Box::new(Expr::Var(name)),
                    right: vec![rhs],
                    modifier: None,
                }),
            },
        ));
    }
    if let Some(stripped) = r2.strip_prefix("::=").or_else(|| r2.strip_prefix(":=")) {
        let (rest, _) = ws(stripped)?;
        let (rest, rhs) = match try_parse_assign_expr(rest) {
            Ok(r) => r,
            Err(_) => expression(rest)?,
        };
        let name = format!("{}{}", prefix, var);
        return Ok((
            rest,
            Expr::AssignExpr {
                name,
                expr: Box::new(rhs),
            },
        ));
    }
    if let Some(stripped) = r2.strip_prefix("⚛+=") {
        let (rest, _) = ws(stripped)?;
        let (rest, rhs) = match try_parse_assign_expr(rest) {
            Ok(r) => r,
            Err(_) => expression(rest)?,
        };
        let name = format!("{}{}", prefix, var);
        return Ok((
            rest,
            Expr::Call {
                name: Symbol::intern("__mutsu_atomic_add_var"),
                args: vec![Expr::Literal(Value::str(name)), rhs],
            },
        ));
    }
    // Check simple chained assignment: $var = ...
    if (r2.starts_with('=') && !r2.starts_with("==") && !r2.starts_with("=>"))
        || r2.starts_with("⚛=")
    {
        let is_atomic = r2.starts_with("⚛=");
        let r3 = if is_atomic {
            &r2["⚛=".len()..]
        } else {
            &r2[1..]
        };
        let (rest, _) = ws(r3)?;
        let (rest, rhs) = match try_parse_assign_expr(rest) {
            Ok(r) => r,
            Err(_) => {
                if sigil == b'@' || sigil == b'%' {
                    expression(rest)?
                } else {
                    expression_no_sequence(rest)?
                }
            }
        };
        let name = format!("{}{}", prefix, var);
        if is_atomic {
            return Ok((
                rest,
                Expr::Call {
                    name: Symbol::intern("__mutsu_atomic_store_var"),
                    args: vec![Expr::Literal(Value::str(name)), rhs],
                },
            ));
        }
        return Ok((
            rest,
            Expr::AssignExpr {
                name,
                expr: Box::new(rhs),
            },
        ));
    }
    Err(PError::expected("assignment expression"))
}

/// Parse a comma expression (may produce a list).
pub(in crate::parser) fn parse_comma_or_expr(input: &str) -> PResult<'_, Expr> {
    let (rest, first) = expression(input)?;
    let (r, _) = ws(rest)?;
    if r.starts_with(',') && !r.starts_with(",,") {
        let (r, _) = parse_char(r, ',')?;
        let (r, _) = ws(r)?;
        if r.starts_with(';') || r.is_empty() || r.starts_with('}') {
            return Ok((r, Expr::ArrayLiteral(vec![first])));
        }
        let mut items = vec![first];
        let (mut r, second) = expression(r)?;
        items.push(second);
        loop {
            let (r2, _) = ws(r)?;
            if !r2.starts_with(',') {
                let items = merge_sequence_seeds(lift_meta_ops_in_list(items));
                return Ok((r2, finalize_list(items)));
            }
            let (r2, _) = parse_char(r2, ',')?;
            let (r2, _) = ws(r2)?;
            if r2.starts_with(';') || r2.is_empty() || r2.starts_with('}') {
                let items = merge_sequence_seeds(lift_meta_ops_in_list(items));
                return Ok((r2, finalize_list(items)));
            }
            let (r2, next) = expression(r2)?;
            items.push(next);
            r = r2;
        }
    }
    Ok((rest, first))
}

/// If the list has exactly one item, return it directly instead of wrapping
/// in an ArrayLiteral.
fn finalize_list(items: Vec<Expr>) -> Expr {
    if items.len() == 1 {
        items.into_iter().next().unwrap()
    } else {
        Expr::ArrayLiteral(items)
    }
}

/// In a comma-separated list, if the last item is a sequence expression
/// (Binary { ..., DotDotDot/DotDotDotCaret, ... }), merge all preceding
/// items into the sequence LHS.
/// E.g. `[0, 1, (*+* ... *)]` → `[ArrayLiteral([0, 1, *+*]) ... *]`
fn merge_sequence_seeds(items: Vec<Expr>) -> Vec<Expr> {
    if items.len() < 2 {
        return items;
    }
    let last = items.last().unwrap();
    if let Expr::Binary { left, op, right } = last
        && matches!(op, TokenKind::DotDotDot | TokenKind::DotDotDotCaret)
    {
        // Merge preceding items + sequence LHS into a new ArrayLiteral
        let mut seeds: Vec<Expr> = items[..items.len() - 1].to_vec();
        seeds.push(*left.clone());
        let merged = Expr::Binary {
            left: Box::new(Expr::ArrayLiteral(seeds)),
            op: op.clone(),
            right: right.clone(),
        };
        vec![merged]
    } else {
        items
    }
}

/// In a comma-separated list, if an item is a MetaOp (X+, Z-, etc.), merge
/// all preceding items into its left operand. This gives meta-ops effective
/// list-infix precedence: `1, 2 X+ 10` → `MetaOp(X, +, [1,2], 10)`.
pub(super) fn lift_meta_ops_in_list(items: Vec<Expr>) -> Vec<Expr> {
    // Find the first MetaOp in the list
    let meta_idx = items.iter().position(|e| matches!(e, Expr::MetaOp { .. }));
    if let Some(idx) = meta_idx
        && idx > 0
        && let Expr::MetaOp {
            meta,
            op,
            left,
            right,
        } = &items[idx]
    {
        // Merge preceding items + meta's original left into a single list
        let mut seeds: Vec<Expr> = items[..idx].to_vec();
        seeds.push(*left.clone());
        let new_left = Expr::ArrayLiteral(seeds);
        let new_meta = Expr::MetaOp {
            meta: meta.clone(),
            op: op.clone(),
            left: Box::new(new_left),
            right: right.clone(),
        };
        let mut result = vec![new_meta];
        result.extend(items[idx + 1..].to_vec());
        return result;
    }
    items
}

pub(super) fn assign_stmt(input: &str) -> PResult<'_, Stmt> {
    let sigil = input.as_bytes().first().copied().unwrap_or(0);
    let is_sigiled = sigil == b'$' || sigil == b'@' || sigil == b'%' || sigil == b'&';

    // Try bare identifier assignment for sigilless variables: a = expr
    if !is_sigiled {
        if let Ok((after_ident, bare_name)) = ident(input) {
            if bare_name == "qx" || bare_name == "qqx" {
                return Err(PError::expected("assignment"));
            }
            let (after_ws, _) = ws(after_ident)?;
            if (after_ws.starts_with('=') && !after_ws.starts_with("=="))
                || after_ws.starts_with("⚛=")
            {
                let is_atomic = after_ws.starts_with("⚛=");
                let rest = if is_atomic {
                    &after_ws["⚛=".len()..]
                } else {
                    &after_ws[1..]
                };
                let (rest, _) = ws(rest)?;
                let (rest, expr) = parse_assign_expr_or_comma(rest)?;
                if is_atomic {
                    let stmt = Stmt::Expr(Expr::Call {
                        name: Symbol::intern("__mutsu_atomic_store_var"),
                        args: vec![Expr::Literal(Value::str(bare_name)), expr],
                    });
                    return parse_statement_modifier(rest, stmt);
                }
                let stmt = Stmt::Assign {
                    name: bare_name,
                    expr,
                    op: AssignOp::Assign,
                };
                return parse_statement_modifier(rest, stmt);
            }
        }
        return Err(PError::expected("assignment"));
    }

    let prefix = match sigil {
        b'@' => "@",
        b'%' => "%",
        b'&' => "&",
        _ => "",
    };

    let (rest, var) = var_name(input)?;
    let name = format!("{}{}", prefix, var);
    let (rest, _) = ws(rest)?;
    let var_expr = if sigil == b'@' {
        Expr::ArrayVar(var.clone())
    } else if sigil == b'%' {
        Expr::HashVar(var.clone())
    } else {
        Expr::Var(name.clone())
    };

    // Mutating method compound assignment: $x.m += expr
    if let Some(after_dot) = rest.strip_prefix('.')
        && !after_dot.starts_with('=')
    {
        let (after_name, method_name) = take_while1(after_dot, |c: char| {
            c.is_alphanumeric() || c == '_' || c == '-'
        })?;
        let method_name = method_name.to_string();
        let (after_name, _) = ws(after_name)?;
        if let Some((after_op, op)) = parse_compound_assign_op(after_name) {
            let (after_op, _) = ws(after_op)?;
            let (rest, rhs) = parse_assign_expr_or_comma(after_op)?;
            let current_value = Expr::MethodCall {
                target: Box::new(var_expr.clone()),
                name: Symbol::intern(&method_name),
                args: Vec::new(),
                modifier: None,
                quoted: false,
            };
            let updated_value = Expr::Binary {
                left: Box::new(current_value),
                op: op.token_kind(),
                right: Box::new(rhs),
            };
            let assign_call = Expr::Call {
                name: Symbol::intern("__mutsu_assign_method_lvalue"),
                args: vec![
                    var_expr,
                    Expr::Literal(Value::str(method_name)),
                    Expr::ArrayLiteral(Vec::new()),
                    updated_value,
                    Expr::Literal(Value::str(name.clone())),
                ],
            };
            let stmt = Stmt::Expr(assign_call);
            return parse_statement_modifier(rest, stmt);
        }
    }

    // Meta-op assignment: @a [X+]= @b → @a = @a X+ @b
    // Meta-op assignment: @a [X+]= @b → @a = @a X+ @b
    // Also handles reduction assignment: $x [+]= 6 → $x += 6
    if let Some((after_eq, meta, op)) = parse_bracket_meta_assign_op(rest) {
        let (after_eq, _) = ws(after_eq)?;
        let (rest, rhs) = parse_assign_expr_or_comma(after_eq)?;
        // For "reduce" meta (plain [op]=), reduce on two values is just the base op.
        if meta == "reduce"
            && let Some(compound_op) = CompoundAssignOp::from_op_name(&op)
        {
            let expr = compound_assigned_value_expr(var_expr, compound_op, rhs);
            let stmt = Stmt::Assign {
                name,
                expr,
                op: AssignOp::Assign,
            };
            return parse_statement_modifier(rest, stmt);
        }
        let expr = Expr::MetaOp {
            meta,
            op,
            left: Box::new(var_expr),
            right: Box::new(rhs),
        };
        let stmt = Stmt::Assign {
            name,
            expr,
            op: AssignOp::Assign,
        };
        return parse_statement_modifier(rest, stmt);
    }

    // Meta-op assignment: @a X*= 10 → @a = @a X* 10
    if let Some((after_eq, meta, op)) = parse_meta_compound_assign_op(rest) {
        let (after_eq, _) = ws(after_eq)?;
        let (rest, rhs) = parse_assign_expr_or_comma(after_eq)?;
        // For scalar variables with Z-meta, fall back to plain compound assignment
        // since scalar Zip on single values is equivalent to the base operator.
        if meta == "Z"
            && sigil == b'$'
            && let Some(compound_op) = CompoundAssignOp::from_op_name(&op)
        {
            let expr = compound_assigned_value_expr(var_expr, compound_op, rhs);
            let stmt = Stmt::Assign {
                name,
                expr,
                op: AssignOp::Assign,
            };
            return parse_statement_modifier(rest, stmt);
        }
        let expr = Expr::MetaOp {
            meta,
            op,
            left: Box::new(var_expr),
            right: Box::new(rhs),
        };
        let stmt = Stmt::Assign {
            name,
            expr,
            op: AssignOp::Assign,
        };
        return parse_statement_modifier(rest, stmt);
    }

    // Set operator compound assignment: $s (|)= 5 → $s = $s (|) 5
    if let Some((stripped, set_tok)) = parse_set_compound_assign_op(rest) {
        let (rest, _) = ws(stripped)?;
        let (rest, rhs) = parse_assign_expr_or_comma(rest).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected right-hand expression after set compound assignment",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(rest.len())),
            exception: None,
        })?;
        let expr = Expr::Binary {
            left: Box::new(var_expr),
            op: set_tok,
            right: Box::new(rhs),
        };
        let stmt = Stmt::Assign {
            name,
            expr,
            op: AssignOp::Assign,
        };
        return parse_statement_modifier(rest, stmt);
    }

    if let Some((stripped, op)) = parse_compound_assign_op(rest) {
        let (rest, _) = ws(stripped)?;
        let (rest, rhs) = parse_assign_expr_or_comma(rest).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected right-hand expression after compound assignment",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(rest.len())),
            exception: None,
        })?;
        let expr = compound_assigned_value_expr(Expr::Var(name.clone()), op, rhs);
        let stmt = Stmt::Assign {
            name,
            expr,
            op: AssignOp::Assign,
        };
        return parse_statement_modifier(rest, stmt);
    }
    if let Some((stripped, op_name)) = parse_custom_compound_assign_op(rest) {
        let (rest, _) = ws(stripped)?;
        let (rest, rhs) = parse_assign_expr_or_comma(rest).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected right-hand expression after compound assignment",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(rest.len())),
            exception: None,
        })?;
        let stmt = Stmt::Assign {
            name: name.clone(),
            expr: Expr::InfixFunc {
                name: op_name,
                left: Box::new(var_expr),
                right: vec![rhs],
                modifier: None,
            },
            op: AssignOp::Assign,
        };
        return parse_statement_modifier(rest, stmt);
    }

    // Mutating method call: $x.=method or $x .= method(args)
    if let Some(stripped) = rest.strip_prefix(".=") {
        let (stripped, _) = ws(stripped)?;
        let var_expr = if sigil == b'@' {
            Expr::ArrayVar(var.clone())
        } else if sigil == b'%' {
            Expr::HashVar(var.clone())
        } else {
            Expr::Var(var.clone())
        };
        // Check for quoted method name: .="method"() or .='method'()
        if let Some((r_after_quote, qname)) = parse_quoted_method_name(stripped) {
            let (r_after_quote, _) = ws(r_after_quote)?;
            let (r_final, args) = if r_after_quote.starts_with('(') {
                let (r2, _) = parse_char(r_after_quote, '(')?;
                let (r2, _) = ws(r2)?;
                let (r2, a) = parse_call_arg_list(r2)?;
                let (r2, _) = ws(r2)?;
                let (r2, _) = parse_char(r2, ')')?;
                (r2, a)
            } else {
                (r_after_quote, vec![])
            };
            let method_expr = match qname {
                QuotedMethodName::Static(mname) => Expr::MethodCall {
                    target: Box::new(var_expr),
                    name: Symbol::intern(&mname),
                    args,
                    modifier: None,
                    quoted: true,
                },
                QuotedMethodName::Dynamic(name_expr) => Expr::DynamicMethodCall {
                    target: Box::new(var_expr),
                    name_expr: Box::new(name_expr),
                    args,
                },
            };
            let stmt = Stmt::Assign {
                name,
                expr: method_expr,
                op: AssignOp::Assign,
            };
            return parse_statement_modifier(r_final, stmt);
        }
        let (r, method_name) = take_while1(stripped, |c: char| {
            c.is_alphanumeric() || c == '_' || c == '-'
        })
        .map_err(|err| PError {
            messages: merge_expected_messages("expected method name after '.='", &err.messages),
            remaining_len: err.remaining_len.or(Some(stripped.len())),
            exception: None,
        })?;
        let method_name = method_name.to_string();
        let (r, args) = if r.starts_with('(') {
            let (r, _) = parse_char(r, '(')?;
            let (r, _) = ws(r)?;
            let (r, a) = parse_call_arg_list(r).map_err(|err| PError {
                messages: merge_expected_messages("expected method call arguments", &err.messages),
                remaining_len: err.remaining_len.or(Some(r.len())),
                exception: None,
            })?;
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, ')')?;
            (r, a)
        } else if r.starts_with(':') && !r.starts_with("::") {
            // Colon-arg syntax: .=method: arg, arg2
            let r = &r[1..];
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
            (r, Vec::new())
        };
        let expr = Expr::MethodCall {
            target: Box::new(var_expr),
            name: Symbol::intern(&method_name),
            args,
            modifier: None,
            quoted: false,
        };
        let stmt = Stmt::Assign {
            name,
            expr,
            op: AssignOp::Assign,
        };
        return parse_statement_modifier(r, stmt);
    }

    // Detect Perl 5 =~ braino: only when followed by space or m/ (not =~$var which is = ~$var)
    if rest.starts_with("=~") && !rest.starts_with("=~=") && !rest.starts_with("=:=") {
        let after = &rest[2..];
        if after.starts_with(' ')
            || after.starts_with('\t')
            || after.starts_with("m/")
            || after.starts_with("m ")
        {
            return Err(PError::fatal(
                "X::Obsolete: Unsupported use of =~ to do pattern matching; in Raku please use ~~"
                    .to_string(),
            ));
        }
    }
    // Simple assignment
    if let Some(stripped) = rest.strip_prefix("⚛+=") {
        let (rest, _) = ws(stripped)?;
        let (rest, rhs) = parse_assign_expr_or_comma(rest).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected right-hand expression after atomic compound assignment",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(rest.len())),
            exception: None,
        })?;
        let stmt = Stmt::Expr(Expr::Call {
            name: Symbol::intern("__mutsu_atomic_add_var"),
            args: vec![Expr::Literal(Value::str(name)), rhs],
        });
        return parse_statement_modifier(rest, stmt);
    }

    // $/ = "string" or $/ = 'string' is rejected as a P5-ism
    // ($/ = Nil, $/ = 42, etc. are allowed)
    if name == "/"
        && rest.starts_with('=')
        && !rest.starts_with("==")
        && !rest.starts_with("=>")
        && !rest.starts_with("=:")
    {
        let rhs_rest = rest[1..].trim_start();
        if rhs_rest.starts_with('"') || rhs_rest.starts_with('\'') {
            return Err(PError::fatal(
                "X::Syntax::Perl5Var: Unsupported use of $/ variable; in Raku please use the filehandle's .nl-in attribute".to_string(),
            ));
        }
    }

    if (rest.starts_with('=') && !rest.starts_with("==") && !rest.starts_with("=>"))
        || rest.starts_with("⚛=")
    {
        let is_atomic = rest.starts_with("⚛=");
        let rest = if is_atomic {
            &rest["⚛=".len()..]
        } else {
            &rest[1..]
        };
        let (rest, _) = ws(rest)?;
        let (rest, expr) = parse_assign_expr_or_comma(rest).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected right-hand expression after '='",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(rest.len())),
            exception: None,
        })?;
        if is_atomic {
            let stmt = Stmt::Expr(Expr::Call {
                name: Symbol::intern("__mutsu_atomic_store_var"),
                args: vec![Expr::Literal(Value::str(name)), expr],
            });
            return parse_statement_modifier(rest, stmt);
        }
        let stmt = Stmt::Assign {
            name,
            expr,
            op: AssignOp::Assign,
        };
        return parse_statement_modifier(rest, stmt);
    }
    // Binding (:= or ::=)
    if let Some(stripped) = rest.strip_prefix("::=").or_else(|| rest.strip_prefix(":=")) {
        let (rest, _) = ws(stripped)?;
        let (rest, expr) = parse_comma_or_expr(rest).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected right-hand expression after ':='",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(rest.len())),
            exception: None,
        })?;
        let stmt = Stmt::Assign {
            name,
            expr,
            op: AssignOp::Bind,
        };
        return parse_statement_modifier(rest, stmt);
    }

    Err(PError::expected("assignment"))
}
