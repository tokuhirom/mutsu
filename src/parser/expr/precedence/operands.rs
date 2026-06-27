use super::*;

/// Parse an operand at additive level or tighter (for loose prefix operators).
pub(crate) fn loose_prefix_operand(input: &str, level: i32) -> PResult<'_, Expr> {
    use crate::parser::stmt::simple::*;
    if level <= PREC_CONCAT {
        concat_expr(input)
    } else {
        additive_expr(input)
    }
}

/// Parse an operand at multiplicative level.
pub(crate) fn multiplicative_operand(input: &str) -> PResult<'_, Expr> {
    multiplicative_expr(input)
}

/// Parse an operand at power level.
pub(crate) fn power_operand(input: &str) -> PResult<'_, Expr> {
    power_expr(input)
}
