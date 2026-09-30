//! A code object's own view of the readonly state of the variables it captures.
//!
//! `Interpreter::readonly_vars` is keyed by bare name and follows the *dynamic*
//! call stack: every routine marks its non-`is rw` scalar parameters on entry and
//! the journal (`enter_readonly_frame`) unmarks them on exit. That is exactly
//! right for a frame's OWN variables, but a variable a closure (or a `my sub`
//! code object) captures belongs to the frame that CREATED it, not to whoever
//! calls it later. A caller's readonly parameter `$string` therefore made the
//! assignment to the callee's own captured `my $string` fail (#10389):
//!
//! ```raku
//! sub mk() { my $string; my sub start() { $string = 1 }; &start }
//! sub cc($string) { my $s = mk(); $s() }   # "Cannot assign to a readonly variable"
//! ```
//!
//! The registry cannot say which binding a mark belongs to, so the code object
//! records it instead. Creation ([`Interpreter::capture_readonly_state`]) notes
//! which of the scalar variables the body writes were readonly in the creating
//! frame, and entry ([`Interpreter::reconcile_captured_readonly`]) puts the
//! registry back in that state for the duration of the call -- journaled, so the
//! caller's own marks return when the call does. Only *written* free variables
//! matter, because a write is the only thing the registry is consulted for.

use super::*;
use crate::opcode::CompiledCode;
use crate::value::CapturedReadonly;
use std::sync::{Arc, LazyLock};

/// Shared by every code object whose creating frame had nothing readonly to
/// record, so the common case allocates nothing.
static NO_READONLY_CAPTURES: LazyLock<CapturedReadonly> = LazyLock::new(|| Arc::from([]));

/// Is `sym` a variable whose readonly state is decided by the bare-name
/// registry and is not one of the names the call machinery manages itself?
///
/// That is a plain user scalar: `$x`, stored sigil-less as `x`. The topic `_`,
/// dynamics (`*x`), specials (`/`, `!`, `?x`, digits) and `__mutsu_*` metadata
/// all start with something other than a letter and are excluded; a sigiled
/// name (`@a`) is never a registry key, so probing one is a harmless no-op.
fn is_reconcilable(sym: Symbol) -> bool {
    sym.is_plain_user_lexical()
        || sym.with_str(|s| {
            s.starts_with(|c: char| c.is_ascii_uppercase())
                && s.chars()
                    .all(|c| c.is_alphanumeric() || c == '_' || c == '-')
        })
}

/// The free variables `code` (or a closure nested in it) writes.
fn written_free_vars(code: &CompiledCode) -> impl Iterator<Item = Symbol> + '_ {
    code.free_var_writes
        .iter()
        .chain(code.free_var_container_writes.iter())
        .copied()
        .filter(|sym| is_reconcilable(*sym))
}

impl Interpreter {
    /// Record, for a code object being created from `code` right now, which of
    /// the variables it writes are readonly in the creating frame. `None` when
    /// the code writes no free variable, so there is nothing to reconcile later.
    // Cost: O(w), w = scalar free variables `code` writes; O(1) when nothing is readonly.
    pub(crate) fn capture_readonly_state(&self, code: &CompiledCode) -> Option<CapturedReadonly> {
        if code.free_var_writes.is_empty() && code.free_var_container_writes.is_empty() {
            return None;
        }
        if self.no_readonly_vars() {
            return Some(Arc::clone(&NO_READONLY_CAPTURES));
        }
        let mut record: Vec<(Symbol, crate::ast::ReadonlyKind)> = Vec::new();
        for sym in written_free_vars(code) {
            if let Some(kind) = self.readonly_kind_sym(sym)
                && !record.iter().any(|(s, _)| *s == sym)
            {
                record.push((sym, kind));
            }
        }
        Some(if record.is_empty() {
            Arc::clone(&NO_READONLY_CAPTURES)
        } else {
            Arc::from(record)
        })
    }

    /// Put the readonly registry in the state a code object's captured
    /// variables had when it was created (see the module doc). Must run inside
    /// the call's readonly frame (after `push_call_frame`), so the changes are
    /// journaled and the caller's marks are restored on return.
    // Cost: O(w), w = scalar free variables `code` writes; O(1) when nothing is readonly.
    pub(crate) fn reconcile_captured_readonly(
        &mut self,
        record: Option<&CapturedReadonly>,
        code: &CompiledCode,
    ) {
        let Some(record) = record else {
            return;
        };
        if record.is_empty() && self.no_readonly_vars() {
            return;
        }
        for sym in written_free_vars(code) {
            let wanted = record.iter().find(|(s, _)| *s == sym).map(|(_, k)| *k);
            if self.readonly_kind_sym(sym) == wanted {
                continue;
            }
            match wanted {
                Some(kind) => self.mark_readonly_sym_with(sym, kind),
                None => self.unmark_readonly_sym(sym),
            }
        }
    }
}
