use crate::ast::Expr;

/// The invocant `has T $.x .= method(...)` calls `method` on, built from the
/// attribute's written type `T`. The parser and the RakuAST lowering share it,
/// so both read `has Inp() $.x .= new` / `has Int(Str) $.x .= new` as a call on
/// the coercion's *target* type (#15845), and `::?CLASS` / `::?ROLE` as the
/// compile-time package variables.
// Cost: O(n), n = length of the type name.
pub(crate) fn call_assign_invocant(type_name: String) -> Expr {
    let target_name = match type_name.split_once('(') {
        Some((target, rest)) if rest.ends_with(')') && !target.is_empty() => target.to_string(),
        _ => type_name,
    };
    match target_name.as_str() {
        "::?CLASS" => Expr::Var("?CLASS".to_string()),
        "::?ROLE" => Expr::Var("?ROLE".to_string()),
        _ => Expr::BareWord(target_name),
    }
}
