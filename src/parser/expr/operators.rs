use crate::token_kind::TokenKind;

use super::super::helpers::is_ident_char;
use super::super::parse_result::{PError, merge_expected_messages};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ComparisonOp {
    StrictEq,
    StrictNe,
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
    ApproxEq,
    ContainerEq,
}

impl ComparisonOp {
    pub(super) fn token_kind(self) -> TokenKind {
        match self {
            ComparisonOp::StrictEq => TokenKind::EqEqEq,
            ComparisonOp::StrictNe => TokenKind::BangEqEqEq,
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
            ComparisonOp::ApproxEq => TokenKind::Ident("=~=".to_string()),
            ComparisonOp::ContainerEq => TokenKind::Ident("=:=".to_string()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ConcatOp {
    Concat,
    Repeat,
    ListRepeat,
    Compose,
}

impl ConcatOp {
    pub(super) fn token_kind(self) -> TokenKind {
        match self {
            ConcatOp::Concat => TokenKind::Tilde,
            ConcatOp::Repeat => TokenKind::Ident("x".to_string()),
            ConcatOp::ListRepeat => TokenKind::Ident("xx".to_string()),
            ConcatOp::Compose => TokenKind::Ident("o".to_string()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum AdditiveOp {
    Add,
    Sub,
    BitOr,
    BitXor,
}

impl AdditiveOp {
    pub(super) fn token_kind(self) -> TokenKind {
        match self {
            AdditiveOp::Add => TokenKind::Plus,
            AdditiveOp::Sub => TokenKind::Minus,
            AdditiveOp::BitOr => TokenKind::BitOr,
            AdditiveOp::BitXor => TokenKind::BitXor,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum MultiplicativeOp {
    Mul,
    Div,
    Mod,
    DivisibleBy,
    IntDiv,
    IntMod,
    Gcd,
    Lcm,
    BitAnd,
    BitShiftLeft,
    BitShiftRight,
}

impl MultiplicativeOp {
    pub(super) fn token_kind(self) -> TokenKind {
        match self {
            MultiplicativeOp::Mul => TokenKind::Star,
            MultiplicativeOp::Div => TokenKind::Slash,
            MultiplicativeOp::Mod => TokenKind::Percent,
            MultiplicativeOp::DivisibleBy => TokenKind::PercentPercent,
            MultiplicativeOp::IntDiv => TokenKind::Ident("div".to_string()),
            MultiplicativeOp::IntMod => TokenKind::Ident("mod".to_string()),
            MultiplicativeOp::Gcd => TokenKind::Ident("gcd".to_string()),
            MultiplicativeOp::Lcm => TokenKind::Ident("lcm".to_string()),
            MultiplicativeOp::BitAnd => TokenKind::BitAnd,
            MultiplicativeOp::BitShiftLeft => TokenKind::BitShiftLeft,
            MultiplicativeOp::BitShiftRight => TokenKind::BitShiftRight,
        }
    }
}

pub(super) fn parse_concat_op(r: &str) -> Option<(ConcatOp, usize)> {
    if r.starts_with('~') && !r.starts_with("~~") && !r.starts_with("~=") {
        Some((ConcatOp::Concat, 1))
    } else if r.starts_with("xx") && !is_ident_char(r.as_bytes().get(2).copied()) {
        Some((ConcatOp::ListRepeat, 2))
    } else if r.starts_with('x') && !is_ident_char(r.as_bytes().get(1).copied()) {
        Some((ConcatOp::Repeat, 1))
    } else if r.starts_with('o') && !is_ident_char(r.as_bytes().get(1).copied()) {
        Some((ConcatOp::Compose, 1))
    } else if r.starts_with('\u{2218}') {
        // ∘ (U+2218 RING OPERATOR) — function composition
        Some((ConcatOp::Compose, '\u{2218}'.len_utf8()))
    } else {
        None
    }
}

pub(super) fn parse_additive_op(r: &str) -> Option<(AdditiveOp, usize)> {
    // Unicode: − (U+2212 MINUS SIGN) subtraction
    if r.starts_with('\u{2212}') {
        return Some((AdditiveOp::Sub, '\u{2212}'.len_utf8()));
    }
    if r.starts_with("+|") {
        Some((AdditiveOp::BitOr, 2))
    } else if r.starts_with("+^") {
        Some((AdditiveOp::BitXor, 2))
    } else if r.starts_with('+') && !r.starts_with("++") && !r.starts_with("+=") {
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

pub(super) fn parse_multiplicative_op(r: &str) -> Option<(MultiplicativeOp, usize)> {
    // Unicode: × (U+00D7) multiplication
    if r.starts_with('\u{00D7}') {
        return Some((MultiplicativeOp::Mul, '\u{00D7}'.len_utf8()));
    }
    // Unicode: ÷ (U+00F7) division
    if r.starts_with('\u{00F7}') {
        return Some((MultiplicativeOp::Div, '\u{00F7}'.len_utf8()));
    }
    if r.starts_with("+&") {
        Some((MultiplicativeOp::BitAnd, 2))
    } else if r.starts_with("+<") {
        Some((MultiplicativeOp::BitShiftLeft, 2))
    } else if r.starts_with("+>") {
        Some((MultiplicativeOp::BitShiftRight, 2))
    } else if r.starts_with('*') && !r.starts_with("**") && !r.starts_with("*=") {
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
pub(super) enum PrefixUnaryOp {
    Not,
    Boolify,
    PreInc,
    PreDec,
    Negate,
    Positive,
    Stringify,
    IntBitNeg,  // +^ integer bitwise negation
    BoolBitNeg, // ?^ boolean bitwise negation
}

impl PrefixUnaryOp {
    pub(super) fn token_kind(self) -> TokenKind {
        match self {
            PrefixUnaryOp::Not => TokenKind::Bang,
            PrefixUnaryOp::Boolify => TokenKind::Question,
            PrefixUnaryOp::PreInc => TokenKind::PlusPlus,
            PrefixUnaryOp::PreDec => TokenKind::MinusMinus,
            PrefixUnaryOp::Negate => TokenKind::Minus,
            PrefixUnaryOp::Positive => TokenKind::Plus,
            PrefixUnaryOp::Stringify => TokenKind::Tilde,
            PrefixUnaryOp::IntBitNeg => TokenKind::IntBitNeg,
            PrefixUnaryOp::BoolBitNeg => TokenKind::BoolBitNeg,
        }
    }

    pub(super) fn consumes_ws(self) -> bool {
        matches!(
            self,
            PrefixUnaryOp::Not
                | PrefixUnaryOp::Boolify
                | PrefixUnaryOp::Negate
                | PrefixUnaryOp::Positive
                | PrefixUnaryOp::Stringify
        )
    }

    pub(super) fn parses_postfix_target(self) -> bool {
        matches!(self, PrefixUnaryOp::PreInc | PrefixUnaryOp::PreDec)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum PostfixUpdateOp {
    Inc,
    Dec,
}

impl PostfixUpdateOp {
    pub(super) fn token_kind(self) -> TokenKind {
        match self {
            PostfixUpdateOp::Inc => TokenKind::PlusPlus,
            PostfixUpdateOp::Dec => TokenKind::MinusMinus,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum LogicalOp {
    Or,         // or (word)
    And,        // and (word)
    OrOr,       // ||
    AndAnd,     // &&
    DefinedOr,  // //
    AndThen,    // andthen
    NotAndThen, // notandthen
    OrElse,     // orelse
    Min,        // min (infix)
    Max,        // max (infix)
}

impl LogicalOp {
    pub(super) fn token_kind(self) -> TokenKind {
        match self {
            LogicalOp::Or => TokenKind::OrWord,
            LogicalOp::And => TokenKind::AndAnd,
            LogicalOp::OrOr => TokenKind::OrOr,
            LogicalOp::AndAnd => TokenKind::AndAnd,
            LogicalOp::DefinedOr => TokenKind::SlashSlash,
            LogicalOp::AndThen => TokenKind::AndThen,
            LogicalOp::NotAndThen => TokenKind::NotAndThen,
            LogicalOp::OrElse => TokenKind::OrElse,
            LogicalOp::Min => TokenKind::Ident("min".to_string()),
            LogicalOp::Max => TokenKind::Ident("max".to_string()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum JunctiveOp {
    Or,  // ?|
    And, // ?&
    Xor, // ?^
}

impl JunctiveOp {
    pub(super) fn token_kind(self) -> TokenKind {
        match self {
            JunctiveOp::Or => TokenKind::Ident("?|".to_string()),
            JunctiveOp::And => TokenKind::Ident("?&".to_string()),
            JunctiveOp::Xor => TokenKind::Ident("?^".to_string()),
        }
    }
}

pub(super) fn parse_prefix_unary_op(input: &str) -> Option<(PrefixUnaryOp, usize)> {
    let next_non_ws = |s: &str| s.trim_start().chars().next();
    if input.starts_with('!')
        && !input.starts_with("!!")
        && !input.starts_with("!~~")
        && !input.starts_with("!%%")
    {
        Some((PrefixUnaryOp::Not, 1))
    } else if input.starts_with("?^") {
        Some((PrefixUnaryOp::BoolBitNeg, 2))
    } else if input.starts_with('?')
        && !input.starts_with("??")
        && !input.starts_with("?|")
        && !input.starts_with("?&")
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
        && let Some(c) = next_non_ws(&input[1..])
        && (c == '$'
            || c == '@'
            || c == '%'
            || c == '&'
            || c == '('
            || c == '"'
            || c == '\''
            || c == '*'
            || c == '.'  // topic method call: -.method
            || c == '\u{221E}' // ∞
            || c.is_ascii_digit()
            || c.is_ascii_alphabetic())
    {
        Some((PrefixUnaryOp::Negate, 1))
    } else if input.starts_with('\u{2212}') {
        // − (U+2212 MINUS SIGN) as prefix negation
        Some((PrefixUnaryOp::Negate, '\u{2212}'.len_utf8()))
    } else if input.starts_with("+^") {
        Some((PrefixUnaryOp::IntBitNeg, 2))
    } else if input.starts_with('+')
        && !input.starts_with("++")
        && let Some(c) = next_non_ws(&input[1..])
        && (c == '$'
            || c == '@'
            || c == '%'
            || c == '&'
            || c == '('
            || c == '"'
            || c == '\''
            || c == '*'
            || c.is_ascii_digit()
            || c.is_ascii_alphabetic())
    {
        Some((PrefixUnaryOp::Positive, 1))
    } else if input.starts_with('~')
        && !input.starts_with("~~")
        && let Some(c) = next_non_ws(&input[1..])
        && (c == '$'
            || c == '@'
            || c == '%'
            || c == '&'
            || c == '('
            || c == '"'
            || c == '\''
            || c == '*'
            || c.is_ascii_digit()
            || c.is_ascii_alphabetic())
    {
        Some((PrefixUnaryOp::Stringify, 1))
    } else {
        None
    }
}

pub(super) fn parse_postfix_update_op(input: &str) -> Option<(PostfixUpdateOp, usize)> {
    if input.starts_with("++") {
        Some((PostfixUpdateOp::Inc, 2))
    } else if input.starts_with("--") {
        Some((PostfixUpdateOp::Dec, 2))
    } else {
        None
    }
}

pub(super) fn parse_junctive_op(input: &str) -> Option<(JunctiveOp, usize)> {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum JunctionInfixOp {
    Any, // |
    All, // &
    One, // ^
}

impl JunctionInfixOp {
    pub(super) fn token_kind(self) -> TokenKind {
        match self {
            JunctionInfixOp::Any => TokenKind::Pipe,
            JunctionInfixOp::All => TokenKind::Ampersand,
            JunctionInfixOp::One => TokenKind::Caret,
        }
    }
}

pub(super) fn parse_junction_infix_op(input: &str) -> Option<(JunctionInfixOp, usize)> {
    // | but not || or |=
    if input.starts_with('|') && !input.starts_with("||") && !input.starts_with("|=") {
        Some((JunctionInfixOp::Any, 1))
    // & but not && or &=
    } else if input.starts_with('&') && !input.starts_with("&&") && !input.starts_with("&=") {
        // Make sure it's not a sigil (e.g. &func)
        if let Some(&c) = input.as_bytes().get(1)
            && (c.is_ascii_alphabetic()
                || c == b'_'
                || c == b'?'
                || c == b'!'
                || c == b'^'
                || c == b'*'
                || c == b'['
                || c == b':')
        {
            return None;
        }
        Some((JunctionInfixOp::All, 1))
    // ^ but not ^.. or ^^ or ^=
    } else if input.starts_with('^')
        && !input.starts_with("^.")
        && !input.starts_with("^^")
        && !input.starts_with("^=")
    {
        if let Some(&c) = input.as_bytes().get(1)
            && (c.is_ascii_alphanumeric() || c == b'$' || c == b'@' || c == b'(')
        {
            return None;
        }
        Some((JunctionInfixOp::One, 1))
    } else {
        None
    }
}

pub(super) fn parse_or_or_op(input: &str) -> Option<(LogicalOp, usize)> {
    if input.starts_with("||") && !input.starts_with("||=") {
        Some((LogicalOp::OrOr, 2))
    } else if input.starts_with("//") && !input.starts_with("///") && !input.starts_with("//=") {
        Some((LogicalOp::DefinedOr, 2))
    } else if input.starts_with("min") && !is_ident_char(input.as_bytes().get(3).copied()) {
        Some((LogicalOp::Min, 3))
    } else if input.starts_with("max") && !is_ident_char(input.as_bytes().get(3).copied()) {
        Some((LogicalOp::Max, 3))
    } else {
        None
    }
}

pub(super) fn parse_and_and_op(input: &str) -> Option<(LogicalOp, usize)> {
    if input.starts_with("&&") && !input.starts_with("&&=") {
        Some((LogicalOp::AndAnd, 2))
    } else {
        None
    }
}

pub(super) fn parse_word_logical_op(input: &str) -> Option<(LogicalOp, usize)> {
    if input.starts_with("orelse") && !is_ident_char(input.as_bytes().get(6).copied()) {
        Some((LogicalOp::OrElse, 6))
    } else if input.starts_with("or") && !is_ident_char(input.as_bytes().get(2).copied()) {
        Some((LogicalOp::Or, 2))
    } else if input.starts_with("andthen") && !is_ident_char(input.as_bytes().get(7).copied()) {
        Some((LogicalOp::AndThen, 7))
    } else if input.starts_with("notandthen") && !is_ident_char(input.as_bytes().get(10).copied()) {
        Some((LogicalOp::NotAndThen, 10))
    } else if input.starts_with("and") && !is_ident_char(input.as_bytes().get(3).copied()) {
        Some((LogicalOp::And, 3))
    } else {
        None
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ExprMode {
    Full,
    NoSequence,
}

pub(super) fn enrich_expected_error(
    err: PError,
    context: &str,
    remaining_len_fallback: usize,
) -> PError {
    PError {
        messages: merge_expected_messages(context, &err.messages),
        remaining_len: err.remaining_len.or(Some(remaining_len_fallback)),
    }
}
