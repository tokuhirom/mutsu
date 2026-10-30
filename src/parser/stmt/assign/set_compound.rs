use super::*;
use crate::rakuast::compound::set::SET_COMPOUND_OPS;

/// Parse set operator compound assignment: `(|)=`, `(&)=`, `(-)=`, `(^)=`, `(.)=`, `(+)=`
/// and their Unicode variants: `\u{222A}=`, `\u{2229}=`, etc.
/// Returns (rest_after_equals, the operator as written).
pub(crate) fn parse_set_compound_assign_op(input: &str) -> Option<(&str, &str)> {
    let (spelling, _) = SET_COMPOUND_OPS
        .iter()
        .find(|(spelling, _)| input.starts_with(spelling))?;
    let after_op = &input[spelling.len()..];
    if after_op.starts_with('=') && !after_op.starts_with("==") {
        set_op_from_spelling(spelling)?;
        Some((&after_op[1..], &input[..spelling.len()]))
    } else {
        None
    }
}
