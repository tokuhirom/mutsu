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
//!
//! A class or role **method** has the same problem with no code object to
//! carry the record (#11054): `my $v; class FH { method m($t) { $v = $t } }`
//! called from `sub caller($v) { FH.m($v) }` saw the caller's readonly `$v`.
//! Its compiled body may not exist yet when the declaration registers, so the
//! declaring frame's marks are snapshotted whole
//! ([`Interpreter::capture_declaring_readonly_state`]) onto the `MethodDef`,
//! and method entry ([`Interpreter::reconcile_method_readonly`]) narrows the
//! snapshot to the variables the body writes.

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
        .chain(code.nested_sub_written_free.iter())
        .copied()
        .filter(|sym| is_reconcilable(*sym))
}

impl Interpreter {
    /// Record, for a code object being created from `code` right now, which of
    /// the variables it writes are readonly in the creating frame. `None` when
    /// the code writes no free variable, so there is nothing to reconcile later.
    // Cost: O(w), w = scalar free variables `code` writes; O(1) when nothing is readonly.
    pub(crate) fn capture_readonly_state(&self, code: &CompiledCode) -> Option<CapturedReadonly> {
        if code.free_var_writes.is_empty()
            && code.free_var_container_writes.is_empty()
            && code.nested_sub_written_free.is_empty()
        {
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

    /// Snapshot the readonly marks of the frame a method declaration registers
    /// in, for [`Self::reconcile_method_readonly`] to narrow at call time. The
    /// method's compiled body (and so the set of variables it writes) may not
    /// exist yet, so every reconcilable mark is kept.
    // Cost: O(r), r = names currently marked readonly; O(1) when none is.
    pub(crate) fn capture_declaring_readonly_state(&self) -> CapturedReadonly {
        if self.no_readonly_vars() {
            return Arc::clone(&NO_READONLY_CAPTURES);
        }
        let record: Vec<(Symbol, crate::ast::ReadonlyKind)> = self
            .readonly_vars
            .borrow()
            .iter()
            .filter(|(sym, _)| is_reconcilable(*sym))
            .collect();
        if record.is_empty() {
            Arc::clone(&NO_READONLY_CAPTURES)
        } else {
            Arc::from(record)
        }
    }

    /// Method-entry counterpart of [`Self::reconcile_captured_readonly`]: put
    /// the variables `code` writes back into the state the declaring frame's
    /// snapshot recorded. Must run inside the call's readonly frame.
    ///
    /// Only a [`crate::ast::ReadonlyKind::Alias`] mark (a parameter or loop
    /// alias, i.e. a binding of some *running* frame) is dropped when the
    /// snapshot lacks the name. An immutable-kind mark describes the binding
    /// itself (`my $x := 42`), and a top-level one may be made after the class
    /// registered -- dropping it would let the method assign an immutable.
    // Cost: O(w * s), w = scalar free variables `code` writes, s = snapshot size
    // (both tiny); O(1) when nothing is readonly.
    pub(crate) fn reconcile_method_readonly(
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
            let current = self.readonly_kind_sym(sym);
            if current == wanted {
                continue;
            }
            match (wanted, current) {
                (Some(kind), _) => self.mark_readonly_sym_with(sym, kind),
                (None, Some(crate::ast::ReadonlyKind::Alias)) => self.unmark_readonly_sym(sym),
                (None, _) => {}
            }
        }
    }
}
