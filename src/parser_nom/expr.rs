use nom::IResult;

use crate::ast::Expr;

use super::primary::primary;

/// Parse an expression. Currently delegates directly to primary.
pub(super) fn expression(input: &str) -> IResult<&str, Expr> {
    primary(input)
}
