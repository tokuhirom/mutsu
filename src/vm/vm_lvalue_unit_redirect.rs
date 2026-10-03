//! Route an lvalue-method writeback (`$x.attr = v`, `$x.substr-rw(..) = v`,
//! `$x[0].attr = v`, ...) to the routine's own variable when that variable is
//! a compunit lexical the routine captured (#11275).
//!
//! The lvalue builtins (`__mutsu_assign_method_lvalue` and its index/delete
//! siblings) write their target back *by name* into `env`
//! (`Env::insert_through`). Inside a named sub whose free variable is a
//! file-scope lexical (ADR-0024's `unit_lexicals` store), that name's env key
//! does not belong to the sub at all: the frame's env is rooted at the live
//! caller, so the key resolves to whatever the *caller* has under that name.
//! Reads and plain stores already take the compunit cell first
//! (`get_env_with_main_alias` / `unit_scope_lexical_write`); the lvalue
//! writeback did not, and replaced the caller's unrelated same-named lexical:
//!
//! ```raku
//! my $state = St.new;
//! sub set-it { $state.top = 5 }
//! sub make-p(&cb) { my $state = 'b'; my sub step { cb(); $state }; &step }
//! make-p({ set-it() })();   # was St.new(top => 5), must be 'b'
//! ```
//!
//! The fix binds the name, for the duration of the builtin call only, to the
//! compunit cell in the frame's own env tier, so the builtin's by-name read
//! and `insert_through` both reach the routine's real variable; the tier's
//! previous entry is put back afterwards. A name that is a local slot of the
//! running frame is the frame's own variable and is never redirected.

use super::*;

/// The frame-tier entry an lvalue writeback redirect displaced, restored by
/// [`Interpreter::end_lvalue_unit_redirect`].
pub(super) struct LvalueUnitRedirect {
    key: Symbol,
    prev: Option<Value>,
}

impl Interpreter {
    /// Bind `target` to its compunit cell in the running frame's env tier when
    /// `target` names a captured compunit lexical rather than a local of
    /// `code`. Returns what must be restored after the builtin call.
    // Cost: O(1) + one `unit_lexical_slot` probe (O(p), p = package-chain
    // candidates, at most 4).
    pub(super) fn begin_lvalue_unit_redirect(
        &mut self,
        code: &CompiledCode,
        target: &str,
    ) -> Option<LvalueUnitRedirect> {
        if self.unit_lexicals.is_empty() || self.find_local_slot(code, target).is_some() {
            return None;
        }
        let cell = self.unit_lexical_slot(target)?;
        if !matches!(cell.view(), ValueView::ContainerRef(_)) {
            return None;
        }
        let cell = cell.clone();
        let key = Symbol::intern(target);
        let prev = self.env().overlay_get_sym(key).cloned();
        self.env_mut().insert_sym(key, cell);
        Some(LvalueUnitRedirect { key, prev })
    }

    /// Undo [`Self::begin_lvalue_unit_redirect`]: put the frame tier's
    /// displaced entry back (or drop the temporary one without tombstoning, so
    /// an enclosing tier's binding shows through again).
    // Cost: O(1).
    pub(super) fn end_lvalue_unit_redirect(&mut self, redirect: Option<LvalueUnitRedirect>) {
        let Some(LvalueUnitRedirect { key, prev }) = redirect else {
            return;
        };
        match prev {
            Some(prev) => {
                self.env_mut().insert_sym(key, prev);
            }
            None => {
                self.env_mut().remove_overlay_sym(key);
            }
        }
    }
}
