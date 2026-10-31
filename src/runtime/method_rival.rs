//! The multi candidates of a method dispatch that did not bind the call.

use super::decl_types::MethodDef;
use super::registry::MethodOverloads;

/// A multi candidate that did not bind the call: a share of its owner's
/// candidate row and its index, so collecting the ~265 rivals of a
/// `RakuAST::Deparse` dispatch bumps a refcount each instead of copying a
/// `MethodDef` (several `String`s) each.
#[derive(Clone)]
pub(crate) struct Rival {
    row: MethodOverloads,
    idx: usize,
}

impl Rival {
    /// The `idx`-th candidate of `row`.
    // Cost: O(1).
    pub(crate) fn of_row(row: &MethodOverloads, idx: usize) -> Self {
        Self {
            row: row.clone(),
            idx,
        }
    }

    /// A candidate that is not held in a shared row.
    // Cost: O(1).
    pub(crate) fn owned(def: MethodDef) -> Self {
        Self {
            row: MethodOverloads::from(vec![def]),
            idx: 0,
        }
    }
}

impl std::ops::Deref for Rival {
    type Target = MethodDef;
    fn deref(&self) -> &MethodDef {
        &self.row[self.idx]
    }
}
