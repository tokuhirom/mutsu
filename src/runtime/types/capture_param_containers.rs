//! A `|c` capture parameter keeps the caller's containers (#11295).
//!
//! Raku binds every argument of a Capture raw: `sub f(|c) { g(|c) }` hands `g`
//! the very containers `f` was called with, so an `is rw` / `\x` parameter of
//! `g` writes the caller's variable (`sub g($x is rw) { $x++ }; f($a)` bumps
//! `$a`). mutsu's `|c` binder used to strip every argument down to its value,
//! so the forwarded slip carried nothing writable and `g` died with "expects a
//! writable container".
//!
//! The capture now stores, for each positional argument that names a plain
//! scalar caller variable, a shared `ContainerRef` cell (the caller's live
//! cell when it already holds one, else a fresh one), and records a
//! per-element writeback in `rw_bindings` — the `*@v is raw` slurpy's
//! encoding — so the cell's final value reaches the caller's variable at
//! return, exactly as for a directly bound `is rw` parameter. A fresh cell is
//! also installed in the callee env under the caller's name, unless that name
//! is one of the callee's own parameters (`sub f($y, |c)` called as
//! `f(1, $y)`). A later forward (`g(|c)`) slips the cells, which the binder
//! already accepts as writable lvalues. The call capture `nqp::usecapture`
//! records reuses this routine (`runtime::call_capture`).

use super::*;
use crate::runtime::types::signature::{encode_slurpy_rw_param, indexed_varref_from_value};

impl Interpreter {
    /// The value a `|c` capture stores for positional argument `raw_arg`
    /// (already known not to be a named Pair). `source` is the caller's
    /// variable name the argument was read from — the `WrapVarRef` tag's name,
    /// else the call's `arg_sources` entry. When it names a writable plain
    /// scalar, the argument becomes a shared cell (installed under that name
    /// unless it is one of `callee_params`) and a writeback for element
    /// `elem_idx` of the capture bound as `capture_key` is pushed onto
    /// `rw_bindings`; any other argument is stored as its value.
    // Cost: O(1) (one env probe + one cell allocation).
    pub(in crate::runtime) fn capture_positional_container(
        &mut self,
        raw_arg: &Value,
        arg_source: Option<&String>,
        capture_key: Option<&str>,
        elem_idx: usize,
        rw_bindings: &mut Vec<(String, String)>,
        callee_params: &[ParamDef],
    ) -> Value {
        let value = unwrap_varref_value(raw_arg.clone());
        // A cell the caller already handed over (a forwarded capture's element,
        // a `deepmap` leaf) is kept as is: it already is the caller's container.
        if value.is_container_ref() {
            return value;
        }
        let Some(capture_key) = capture_key else {
            return value;
        };
        let varref = indexed_varref_from_value(raw_arg);
        // An element-indexed source (`f(@a[0])`) is an array slot, not an env
        // entry a cell can replace; it keeps the plain value.
        if matches!(varref, Some((_, _, Some(_)))) {
            return value;
        }
        let Some(source) = varref
            .map(|(name, _, _)| name)
            .or_else(|| arg_source.cloned())
        else {
            return value;
        };
        // Only a plain `$` scalar variable (`$a` is recorded as `a`): `@`/`%`
        // sources are reference types whose mutations are already shared, and
        // `self`, twigil and dynamic spellings are not caller lexicals a cell
        // may replace.
        // A sigilless source is named by its term key (`\c`, ADR-12441).
        let plain_scalar = source != "self"
            && crate::runtime::term_names::term_spelling(&source)
                .unwrap_or(&source)
                .as_bytes()
                .first()
                .is_some_and(|b| b.is_ascii_alphabetic() || *b == b'_');
        if !plain_scalar {
            return value;
        }
        // A type object read through a name that is not a local (`f(Int)`,
        // `f(C1)`) is a value, not a variable; nor is a readonly / sigilless
        // value binding (`my \t = Person`) or a Proxy the caller holds.
        if matches!(value.view(), ValueView::Package(_))
            && raw_arg.varref_slot().is_none_or(|slot| slot == u32::MAX)
        {
            return value;
        }
        if self.name_is_readonly_binding(&source) {
            return value;
        }
        // The caller's own slot may hold the cell while its env does not
        // (a light-path frame keeps an `is rw` parameter's shared cell in
        // its slot only, and the callee's env no longer sees the caller's
        // names): that cell is the caller's container (#13921).
        if let Some(cell) = self.caller_slot_cell(&source, raw_arg.varref_slot()) {
            return cell;
        }
        let cell = match self.env.get(&source).cloned() {
            Some(existing) if matches!(existing.view(), ValueView::ContainerRef(_)) => existing,
            Some(existing) if matches!(existing.view(), ValueView::Proxy { .. }) => return value,
            _ => {
                let cell = Value::container_ref(crate::gc::Gc::new(
                    crate::value::ContainerCell::new(value),
                ));
                // Installed under the caller's name, so a write the body makes
                // to that name (an `is rw` callee's writeback, a `callsame`
                // through a wrapper) and a write through the capture land in
                // the same cell. Not when the name is one of the callee's own
                // parameters (`sub f($y, |c)` called as `f(1, $y)`): the
                // parameter stays the callee's, and the cell is written back
                // at return.
                if !callee_params.iter().any(|pd| pd.name == source) {
                    self.env.insert(source.clone(), cell.clone());
                }
                cell
            }
        };
        crate::value::name_container_cell(&cell, &source);
        rw_bindings.push((encode_slurpy_rw_param(capture_key, elem_idx, None), source));
        cell
    }

    /// The shared cell the calling frame's slot of its local `source` holds:
    /// the frame that issued the call being bound is the innermost suspended
    /// one. `slot` is the slot the call site's `WrapVarRef` resolved; a
    /// method call's arguments carry none, and then the caller's only local
    /// of that name is the one (a name it declares twice is left alone).
    // Cost: O(1) with `slot`; O(l) without, l = the caller's locals.
    fn caller_slot_cell(&self, source: &str, slot: Option<u32>) -> Option<Value> {
        let frame = self.call_frames.last()?;
        if frame.caller_code == 0 {
            return None;
        }
        // SAFETY: `caller_code` is the address of the `CompiledCode` the
        // caller was running when it pushed this frame; that caller is
        // suspended until the frame pops, so its code is alive while the
        // frame is on `call_frames`.
        let caller = unsafe { &*(frame.caller_code as *const crate::opcode::CompiledCode) };
        let slot = match slot.filter(|slot| *slot != u32::MAX) {
            Some(slot) => {
                let slot = slot as usize;
                caller.locals.get(slot).filter(|name| *name == source)?;
                slot
            }
            None => {
                let mut named = caller
                    .locals
                    .iter()
                    .enumerate()
                    .filter(|(_, name)| *name == source)
                    .map(|(i, _)| i);
                let first = named.next()?;
                if named.next().is_some() {
                    return None;
                }
                first
            }
        };
        let base = frame.saved_locals_base.as_ref()?.base();
        let region = self.locals.all_slots().get(base..self.locals.base())?;
        region.get(slot).filter(|v| v.is_container_ref()).cloned()
    }
}
