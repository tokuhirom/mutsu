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
//! and method entry narrows the snapshot to the variables the body writes. A
//! top-level routine's code value (`my $s = &setter; $s($v)`, #11070) takes the
//! same declaration-time snapshot from its `FunctionDef`: its free variables
//! belong to no running routine frame, so the frame that mentions `&setter`
//! says nothing about them.
//!
//! Both kinds of record go through one reconcile
//! ([`Interpreter::reconcile_captured_readonly`]); a declaration-time snapshot
//! differs only in which absent marks it clears (see
//! [`crate::value::ReadonlySnapshot::at_declaration`]).

use super::*;
use crate::ast::ReadonlyKind;
use crate::opcode::CompiledCode;
use crate::value::{CapturedReadonly, ReadonlySnapshot};
use std::sync::{Arc, LazyLock};

/// Shared by every creation-time record whose frame had nothing readonly, so
/// the common case allocates nothing.
static NO_READONLY_CAPTURES: LazyLock<CapturedReadonly> =
    LazyLock::new(|| snapshot(Vec::new(), false));

/// The declaration-time counterpart of [`NO_READONLY_CAPTURES`].
static NO_READONLY_AT_DECLARATION: LazyLock<CapturedReadonly> =
    LazyLock::new(|| snapshot(Vec::new(), true));

fn snapshot(marks: Vec<(Symbol, ReadonlyKind)>, at_declaration: bool) -> CapturedReadonly {
    Arc::new(ReadonlySnapshot {
        marks: marks.into_boxed_slice(),
        at_declaration,
    })
}

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
        let mut record: Vec<(Symbol, ReadonlyKind)> = Vec::new();
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
            snapshot(record, false)
        })
    }

    /// Snapshot the readonly marks of the frame a declaration (a method, a
    /// routine) registers in. The compiled body -- and so the set of variables
    /// it writes -- may not exist yet, so every reconcilable mark is kept and
    /// the call narrows it.
    // Cost: O(r), r = names currently marked readonly; O(1) when none is.
    pub(crate) fn capture_declaring_readonly_state(&self) -> CapturedReadonly {
        if self.no_readonly_vars() {
            return Arc::clone(&NO_READONLY_AT_DECLARATION);
        }
        let record: Vec<(Symbol, ReadonlyKind)> = self
            .readonly_vars
            .borrow()
            .iter()
            .filter(|(sym, _)| is_reconcilable(*sym))
            .collect();
        if record.is_empty() {
            Arc::clone(&NO_READONLY_AT_DECLARATION)
        } else {
            snapshot(record, true)
        }
    }

    /// Put the variables `code` writes into the readonly state `record` holds
    /// (see the module doc). Must run inside the call's readonly frame (after
    /// `push_call_frame`), so the changes are journaled and the caller's marks
    /// are restored on return.
    ///
    /// A name the record lacks was writable where it was taken. A creation-time
    /// record clears any mark on it. A declaration-time snapshot only clears a
    /// [`ReadonlyKind::Alias`] mark (a parameter or loop alias of some running
    /// frame): an immutable-kind mark describes the binding itself
    /// (`my $x := 42`), which may be made after the declaration registered.
    // Cost: O(w * r), w = scalar free variables `code` writes, r = record size
    // (both tiny); O(1) when nothing is readonly.
    pub(crate) fn reconcile_captured_readonly(
        &mut self,
        record: Option<&CapturedReadonly>,
        code: &CompiledCode,
    ) {
        self.reconcile_captured_readonly_ex(record, code, false);
    }

    /// [`Self::reconcile_captured_readonly`] for an `is rw` routine: every
    /// free variable it READS is reconciled too, since its return value may be
    /// that variable's container, written by the caller after the call
    /// (`method level() is rw { $level }` then `$o.level = 5` from a frame
    /// with its own readonly `$level` parameter — Lumberjack's
    /// `for ... -> $level { $foo.log-level = $level }`).
    // Cost: O(v * r), v = scalar free variables `code` names, r = record size.
    pub(crate) fn reconcile_captured_readonly_ex(
        &mut self,
        record: Option<&CapturedReadonly>,
        code: &CompiledCode,
        include_reads: bool,
    ) {
        let Some(record) = record else {
            return;
        };
        if record.marks.is_empty() && self.no_readonly_vars() {
            return;
        }
        let reads = include_reads
            .then(|| {
                code.free_var_syms
                    .iter()
                    .copied()
                    .filter(|s| is_reconcilable(*s))
            })
            .into_iter()
            .flatten();
        let syms: Vec<Symbol> = written_free_vars(code).chain(reads).collect();
        for sym in syms {
            let wanted = record
                .marks
                .iter()
                .find(|(s, _)| *s == sym)
                .map(|(_, k)| *k);
            let current = self.readonly_kind_sym(sym);
            if current == wanted {
                continue;
            }
            match (wanted, current) {
                (Some(kind), _) => self.mark_readonly_sym_with(sym, kind),
                (None, Some(ReadonlyKind::Alias)) => self.unmark_readonly_sym(sym),
                (None, Some(_)) if !record.at_declaration => self.unmark_readonly_sym(sym),
                (None, _) => {}
            }
        }
    }
}
