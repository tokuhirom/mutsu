//! The VM half of BEGIN-time record and replay (ADR-18012 S1; the plan and the
//! proof live in [`crate::runtime::begin_replay`]).

use super::*;
use crate::runtime::begin_replay::{self, scan, watch::Watch};

/// A `BEGIN` body being recorded: what its unit lexicals encoded to before it
/// ran, and the watch over its run.
pub(super) struct BeginRecording {
    before: Vec<(usize, Option<Vec<u8>>)>,
    watch: Watch,
}

impl Interpreter {
    /// Start recording the `BEGIN` body `start..end` of `code`, or `None` when
    /// it cannot be proven replay-safe (then it just runs, as always).
    // Cost: O(n + v), n = opcodes in the body, v = size of the unit lexicals it
    // could change (encoded once before the body).
    pub(super) fn begin_recording(
        &self,
        code: &CompiledCode,
        start: usize,
        end: usize,
    ) -> Option<BeginRecording> {
        if !scan::body_is_recordable(code, start, end) {
            return None;
        }
        let declared = scan::body_declared_slots(code, start, end);
        let mut before = Vec::new();
        for slot in 0..self.locals.len() {
            if declared.contains(&slot) {
                continue;
            }
            let value = self.locals[slot].deref_container();
            match begin_replay::encoding_of(&value) {
                Some(bytes) => before.push((slot, Some(bytes))),
                // A type object cannot be changed by anything the scan admits;
                // any other value the log cannot hold might be.
                None if Self::begin_opaque_ok(&value) => before.push((slot, None)),
                None => {
                    begin_replay::trace("a unit lexical holds a value the log cannot hold");
                    return None;
                }
            }
        }
        Some(BeginRecording {
            before,
            watch: Watch::start(code),
        })
    }

    /// A unit lexical value the log cannot hold but the body cannot change
    /// either: a type object. (A routine is not one: `nqp::bindattr` can edit
    /// a `Code` object's attributes.)
    // Cost: O(1).
    fn begin_opaque_ok(value: &Value) -> bool {
        matches!(value.view(), ValueView::Package(_))
    }

    /// The `BEGIN` body `site_id` ran to `value`: record it if it stayed
    /// replay-safe.
    // Cost: O(v), v = size of the unit lexicals compared and the value.
    pub(super) fn finish_begin_recording(
        &self,
        recording: BeginRecording,
        site_id: u64,
        value: &Value,
    ) {
        if !recording.watch.finish() {
            begin_replay::trace(&format!("BEGIN {site_id} ran other compiled code"));
            return;
        }
        let mut changed = Vec::new();
        let mut unchanged = Vec::new();
        for (slot, before) in recording.before {
            let Some(current) = self.locals.get(slot).map(Value::deref_container) else {
                return;
            };
            let after = begin_replay::encoding_of(&current);
            match (&before, &after) {
                (Some(b), Some(a)) if b == a => unchanged.push(current),
                // Still a type object: the body cannot have changed it.
                (None, None) if Self::begin_opaque_ok(&current) => {}
                (_, Some(_)) => changed.push((slot, current)),
                _ => return,
            }
        }
        // The value and the changed lexicals must decode to what they are, and
        // share no container with each other or with a lexical left as it was
        // (a decode would build the shared one twice).
        let mut all: Vec<&Value> = vec![value];
        all.extend(changed.iter().map(|(_, v)| v));
        all.extend(unchanged.iter());
        if let Err(why) = begin_replay::faithful::check(&all) {
            begin_replay::trace(&format!("BEGIN {site_id} is not recorded: {why}"));
            return;
        }
        begin_replay::record_site(site_id, value, &changed);
    }

    /// Bind the unit lexicals a replayed `BEGIN` body changed, into the cells
    /// they live in (a routine that closed over one keeps seeing it).
    // Cost: O(k), k = recorded lexicals.
    pub(super) fn bind_replayed_locals(
        &mut self,
        code: &CompiledCode,
        locals: Vec<(usize, Value)>,
    ) {
        for (slot, value) in locals {
            if slot >= self.locals.len() {
                continue;
            }
            if let ValueView::ContainerRef(cell) = self.locals[slot].view() {
                Value::store_through_cell(&cell, &value);
            } else {
                self.locals[slot] = value;
            }
            self.flush_local_to_env(code, slot);
        }
    }
}
