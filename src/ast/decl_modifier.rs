//! Written conditional modifiers on declarations, before initializer hoisting.

#[derive(Debug, Clone, Copy, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum DeclarationModifier {
    If,
    Unless,
    With,
    Without,
}
