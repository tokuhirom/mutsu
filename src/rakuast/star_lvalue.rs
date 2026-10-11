//! Reconstruct the source list shape of an array/Whatever assignment.

use crate::ast::Expr;
use crate::value::ValueView;

/// Recover the written one-array/one-Whatever assignment from the parser's
/// helper that truncates or offsets an array's right-hand side.
// Cost: O(1).
pub(super) fn parts(name: &str, value: &Expr) -> Option<(Expr, Expr)> {
    let Expr::Call {
        name: helper, args, ..
    } = value
    else {
        return None;
    };
    if helper.as_str() != "__mutsu_star_lvalue_rhs" {
        return None;
    }
    let [Expr::Literal(target), rhs] = args.as_slice() else {
        return None;
    };
    let ValueView::Str(target) = target.view() else {
        return None;
    };
    let array = target.as_str().strip_prefix('@')?;
    if target.as_str() != name || array.is_empty() {
        return None;
    }
    let (position, source_rhs) = match rhs {
        Expr::MethodCall {
            target,
            name: method,
            args,
            ..
        } if method.as_str() == "skip" => {
            let [Expr::Literal(offset)] = args.as_slice() else {
                return None;
            };
            let ValueView::Int(offset) = offset.view() else {
                return None;
            };
            (usize::try_from(offset).ok()?, target.as_ref().clone())
        }
        other => (0, other.clone()),
    };
    let mut targets = vec![Expr::Whatever; position];
    targets.push(Expr::ArrayVar(array.to_string()));
    if position == 0 {
        targets.push(Expr::Whatever);
    }
    Some((
        Expr::Grouped(Box::new(Expr::ArrayLiteral(targets))),
        source_rhs,
    ))
}
