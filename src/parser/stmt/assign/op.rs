use super::*;

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
            CompoundAssignOp::JuncAny => "|=",
            CompoundAssignOp::JuncAll => "&=",
            CompoundAssignOp::JuncOne => "^=",
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
            CompoundAssignOp::JuncAny => TokenKind::Pipe,
            CompoundAssignOp::JuncAll => TokenKind::Ampersand,
            CompoundAssignOp::JuncOne => TokenKind::Caret,
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

const COMPOUND_ASSIGN_OPS: &[CompoundAssignOp] = &[
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
    // Junction compound assignment: |=, &=, ^= -- must come after all prefixed variants
    CompoundAssignOp::JuncAny,    // |=
    CompoundAssignOp::JuncAll,    // &=
    CompoundAssignOp::JuncOne,    // ^=
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
    if let Some((symbol, len)) =
        crate::parser::stmt::simple::match_user_declared_infix_symbol_op(input)
    {
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
