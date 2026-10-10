//! Public fields owned by abstract model classes rather than concrete nodes.

use super::RakuAstClass;
use super::fields::Absent;

const STATEMENT_FIELDS: &[(&str, Absent)] = &[("labels", Absent::EmptyList)];

// Cost: O(1), the model class name is a finite registry key.
pub(super) fn local_names(class_name: &str) -> Option<Vec<&'static str>> {
    match class_name {
        "RakuAST::Statement" => Some(STATEMENT_FIELDS.iter().map(|(name, _)| *name).collect()),
        _ => None,
    }
}

// Cost: O(1), the concrete node's printed class name is a finite registry key.
pub(super) fn for_class(class: RakuAstClass) -> &'static [(&'static str, Absent)] {
    if class.printed_name().starts_with("RakuAST::Statement::") {
        STATEMENT_FIELDS
    } else {
        &[]
    }
}
