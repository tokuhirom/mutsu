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
//! scalar caller variable, the same shared `ContainerRef` cell an `is rw`
//! parameter binds (reusing the caller's live cell when it already holds one),
//! and records a per-element writeback in `rw_bindings` — the `*@v is raw`
//! slurpy's encoding — so the cell's final value reaches the caller's variable
//! at return, exactly as for a directly bound `is rw` parameter. A later
//! forward (`g(|c)`) slips the cells, which the binder already accepts as
//! writable lvalues.

use super::*;
use crate::runtime::types::signature::{encode_slurpy_rw_param, indexed_varref_from_value};

impl Interpreter {
    /// The value a `|c` capture stores for positional argument `raw_arg`
    /// (already known not to be a named Pair). `source` is the caller's
    /// variable name the argument was read from — the `WrapVarRef` tag's name,
    /// else the call's `arg_sources` entry. When it names a writable plain
    /// scalar, the argument becomes a shared cell installed under that name and
    /// a writeback for element `elem_idx` of the capture bound as `capture_key`
    /// is pushed onto `rw_bindings`; any other argument is stored as its value.
    // Cost: O(1) per argument (one env probe + one cell allocation).
    pub(super) fn capture_positional_container(
        &mut self,
        raw_arg: &Value,
        arg_source: Option<&String>,
        capture_key: Option<&str>,
        elem_idx: usize,
        rw_bindings: &mut Vec<(String, String)>,
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
        let plain_scalar = source != "self"
            && source
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
        let existing = self.env.get(&source).cloned();
        let cell = match existing {
            Some(cell) if matches!(cell.view(), ValueView::ContainerRef(_)) => cell,
            Some(proxy) if matches!(proxy.view(), ValueView::Proxy { .. }) => return value,
            _ => {
                let cell = Value::container_ref(crate::gc::Gc::new(
                    crate::value::ContainerCell::new(value),
                ));
                self.env.insert(source.clone(), cell.clone());
                cell
            }
        };
        crate::value::name_container_cell(&cell, &source);
        rw_bindings.push((encode_slurpy_rw_param(capture_key, elem_idx, None), source));
        cell
    }
}
