//! Recover the public composition expression from the parser's executable
//! WhateverCode closure. The executable operands have already been classified
//! into `WhateverCode::Argument` leaves; the source spelling keeps the `o`
//! infix and its original operand order.

use crate::scope::NameScope;
use super::RakuAstNode;
use super::convert::{convert_expr, unsupported};
use crate::ast::{Expr, Stmt};
use crate::value::RuntimeError;

// Cost: O(n), n = nodes in the two composition operands.
pub(super) fn convert(names: &NameScope, source: &Expr, executable: &Expr) -> Result<RakuAstNode, RuntimeError> {
    let Expr::Binary {
        left,
        op,
        right,
        form,
    } = source
    else {
        return Err(unsupported("composition source"));
    };
    let body = match executable {
        Expr::Lambda { body, .. } | Expr::AnonSubParams { body, .. } => body,
        _ => return Err(unsupported("composition execution shape")),
    };
    let [
        Stmt::Expr(Expr::Binary {
            left: executed_left,
            right: executed_right,
            ..
        }),
    ] = body.as_slice()
    else {
        return Err(unsupported("composition execution body"));
    };
    let restored = Expr::Binary {
        left: Box::new(if crate::parser::is_whatever(left) {
            Expr::WhateverArg
        } else {
            (**executed_left).clone()
        }),
        op: op.clone(),
        right: Box::new(if crate::parser::is_whatever(right) {
            Expr::WhateverArg
        } else {
            (**executed_right).clone()
        }),
        form: *form,
    };
    convert_expr(names, &restored)
}
