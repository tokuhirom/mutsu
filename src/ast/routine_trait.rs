//! Written routine traits, before the parser folds them into execution fields.

use super::{Expr, SourceForm, Stmt};

#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum RoutineTrait {
    Is {
        name: String,
        argument: Option<TraitArgument>,
    },
    Returns(String),
    Of(String),
    Unsupported(String),
}

#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum TraitArgument {
    Parentheses(Expr),
    Words(String),
    ExportTags(Vec<String>),
    Operator(String),
}

/// Source records are compiler-inert, like the other source forms. Ordinary
/// parses build none; the `.AST` parser and the RakuAST lowering retain them.
// Cost: O(n), n = statements in the body (moving the existing statements).
pub(crate) fn attach(body: &mut Vec<Stmt>, traits: Vec<RoutineTrait>) {
    if !traits.is_empty() {
        body.insert(
            0,
            Stmt::SourceForm(Box::new(SourceForm::RoutineTraits(traits))),
        );
    }
}

// Cost: O(n), n = statements in the body.
pub(crate) fn get(body: &[Stmt]) -> Option<&[RoutineTrait]> {
    body.iter().find_map(|stmt| match stmt {
        Stmt::SourceForm(form) => match form.as_ref() {
            SourceForm::RoutineTraits(traits) => Some(traits.as_slice()),
            _ => None,
        },
        _ => None,
    })
}
