//! An imaginary literal (`2i`, `0.0i`, `3e2i`) as RakuAST keeps it: the
//! number as written, the operand of a `Postfix("i")`. The parser folds the
//! literal to its Complex value and remembers the number's spelling
//! (`Spelling::Imaginary`); without one the number is chosen from the value.

use super::view::{leaf_field, node_field, unsupported};
use super::{RakuAstClass, RakuAstNode};
use crate::value::{RuntimeError, Value};

/// `ApplyPostfix(<number>, Postfix("i"))` for an imaginary literal whose
/// number was written as `text`: an `IntLiteral`, a `RatLiteral` (`0.0`) or,
/// with an exponent, a `NumLiteral` (`3e2` is `300e0`), as rakudo keeps it.
// Cost: O(t), t = length of the text.
pub(super) fn spelled(text: &str) -> Result<RakuAstNode, RuntimeError> {
    let (class, value) = if text.contains(['e', 'E']) {
        let n: f64 = text
            .parse()
            .map_err(|_| unsupported("imaginary literal with an unreadable number"))?;
        (RakuAstClass::NumLiteral, Value::num(n))
    } else if text.contains('.') {
        let rat = crate::parser::decimal_literal_value(text)
            .ok_or_else(|| unsupported("imaginary literal without a decimal spelling"))?;
        (RakuAstClass::RatLiteral, rat)
    } else {
        let n: i64 = text
            .parse()
            .map_err(|_| unsupported("imaginary literal with an unreadable number"))?;
        (RakuAstClass::IntLiteral, Value::int(n))
    };
    Ok(apply_imaginary(RakuAstNode {
        origin: Default::default(),
        class,
        fields: vec![leaf_field(None, value)],
    }))
}

/// `ApplyPostfix(<number>, Postfix("i"))` for an imaginary literal of
/// imaginary part `im`. The parser keeps only the value, so the number's
/// spelling is chosen from it: an integral part is an `IntLiteral` (`2i`), any
/// other finite one the `RatLiteral` of its shortest decimal spelling (`3.5i`).
/// A `3.5e0i` therefore comes back as `3.5i`, which denotes the same Complex.
// Cost: O(d), d = digits of the imaginary part.
pub(super) fn from_value(im: f64) -> Result<RakuAstNode, RuntimeError> {
    if !im.is_finite() || im < 0.0 || (im == 0.0 && im.is_sign_negative()) {
        return Err(unsupported(
            "imaginary literal with a non-finite or negative part",
        ));
    }
    let number = if im.fract() == 0.0 && im < 9.007_199_254_740_992e15 {
        RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::IntLiteral,
            fields: vec![leaf_field(None, Value::int(im as i64))],
        }
    } else {
        let spelling = format!("{im}");
        let rat = crate::parser::decimal_literal_value(&spelling)
            .ok_or_else(|| unsupported("imaginary literal without a decimal spelling"))?;
        RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::RatLiteral,
            fields: vec![leaf_field(None, rat)],
        }
    };
    Ok(apply_imaginary(number))
}

/// `ApplyPostfix(number, Postfix("i"))`.
// Cost: O(1).
fn apply_imaginary(number: RakuAstNode) -> RakuAstNode {
    RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::ApplyPostfix,
        fields: vec![
            node_field(Some("operand"), number),
            node_field(
                Some("postfix"),
                RakuAstNode {
                    origin: Default::default(),
                    class: RakuAstClass::Postfix,
                    fields: vec![leaf_field(Some("operator"), Value::str_from("i"))],
                },
            ),
        ],
    }
}
