use super::*;

/// Convert the written declaration, never its staged initialization helpers.
pub(super) fn declaration_modifier_node(
    declaration: &Stmt,
    condition: &Expr,
    kind: crate::ast::decl_modifier::DeclarationModifier,
) -> Result<RakuAstNode, RuntimeError> {
    use crate::ast::decl_modifier::DeclarationModifier;
    let mut statement =
        convert_stmt(declaration)?.ok_or_else(|| unsupported("empty declaration modifier"))?;
    statement.fields.push(node_field(
        Some("condition-modifier"),
        RakuAstNode {
            origin: Default::default(),
            class: match kind {
                DeclarationModifier::If => RakuAstClass::StatementModifierIf,
                DeclarationModifier::Unless => RakuAstClass::StatementModifierUnless,
                DeclarationModifier::With => RakuAstClass::StatementModifierWith,
                DeclarationModifier::Without => RakuAstClass::StatementModifierWithout,
                DeclarationModifier::When => RakuAstClass::StatementModifierWhen,
            },
            fields: vec![node_field(None, convert_expr(condition)?)],
        },
    ));
    Ok(statement)
}
