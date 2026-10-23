//! Emitting a named element store (ADR-14632 §2.5).
//!
//! A target the compiler resolved to a local slot compiles to
//! `IndexAssignLocal { slot, .. }`, whose store operates on that slot when the
//! running code declares the variable. Any other target compiles to
//! `IndexAssignExprNamed`, which stores by name.

use super::*;

/// The flags an element store carries besides its target.
#[derive(Clone, Copy, Default)]
pub(super) struct IndexAssignFlags {
    pub(super) is_positional: bool,
    pub(super) index_first: bool,
    pub(super) concat_append: bool,
    pub(super) element_share: bool,
}

impl Compiler {
    /// Emit the element store into `name_idx`, held in `target_slot` when the
    /// compiler resolved the name to a local.
    // Cost: O(1).
    pub(super) fn emit_index_assign_named(
        &mut self,
        name_idx: u32,
        target_slot: Option<u32>,
        flags: IndexAssignFlags,
    ) {
        let IndexAssignFlags {
            is_positional,
            index_first,
            concat_append,
            element_share,
        } = flags;
        self.code.emit(match target_slot {
            Some(slot) => OpCode::IndexAssignLocal {
                slot,
                name_idx,
                is_positional,
                index_first,
                concat_append,
                element_share,
            },
            None => OpCode::IndexAssignExprNamed {
                name_idx,
                is_positional,
                index_first,
                concat_append,
                element_share,
            },
        });
    }
}
