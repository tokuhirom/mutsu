//! The VM half of BEGIN-time record and replay (ADR-18012 S1; the plan and the
//! proof live in [`crate::runtime::begin_replay`]).

use super::*;
use crate::runtime::begin_replay::{self, scan, watch::Watch};

/// A `BEGIN` body being recorded: the unit lexicals it reaches, what they
/// encoded to before it ran, and the watch over its run.
pub(super) struct BeginRecording {
    inputs: Vec<(usize, Vec<u8>)>,
    /// The body's own slots (sorted).
    declared: Vec<usize>,
    watch: Watch,
}

impl Interpreter {
    /// Start recording the `BEGIN` body `start..end` of `code`, or `None` when
    /// it cannot be proven replay-safe (then it just runs, as always).
    // Cost: O(n log n + v), n = opcodes in the body, v = size of the unit
    // lexicals it reaches (encoded once before the body).
    pub(super) fn begin_recording(
        &self,
        code: &CompiledCode,
        start: usize,
        end: usize,
    ) -> Option<BeginRecording> {
        if !scan::body_is_recordable(code, start, end) {
            return None;
        }
        let slots = scan::body_slots(code, start, end);
        let mut inputs = Vec::new();
        for slot in slots.unit {
            let value = self.locals.get(slot)?.deref_container();
            let Some(bytes) = begin_replay::encoding_of(&value) else {
                begin_replay::trace(
                    "a unit lexical the body reaches holds a value the log cannot hold",
                );
                return None;
            };
            inputs.push((slot, bytes));
        }
        Some(BeginRecording {
            inputs,
            declared: slots.declared,
            watch: Watch::start(code),
        })
    }

    /// The `BEGIN` body `site_id` ran to `value`: record it if it stayed
    /// replay-safe.
    // Cost: O(v + l), v = size of the reached lexicals and the value, l = the
    // containers reachable from the frame's other lexicals.
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
        for (slot, before) in &recording.inputs {
            let Some(current) = self.locals.get(*slot).map(Value::deref_container) else {
                return;
            };
            match begin_replay::encoding_of(&current) {
                Some(after) if after == *before => unchanged.push(current),
                Some(_) => changed.push((*slot, current)),
                None => {
                    begin_replay::trace(&format!(
                        "BEGIN {site_id}: a changed lexical is not recordable"
                    ));
                    return;
                }
            }
        }
        // The value and the changed lexicals must decode to what they are, and
        // share no container with each other or with any other lexical of the
        // frame that outlives the body (a decode would build the shared one
        // twice).
        let mut all: Vec<&Value> = vec![value];
        all.extend(changed.iter().map(|(_, v)| v));
        all.extend(unchanged.iter());
        let others: Vec<Value> = (0..self.locals.len())
            .filter(|slot| {
                !recording.inputs.iter().any(|(s, _)| s == slot)
                    && recording.declared.binary_search(slot).is_err()
            })
            .map(|slot| self.locals[slot].deref_container())
            .collect();
        let others: Vec<&Value> = others.iter().collect();
        if let Err(why) = begin_replay::faithful::check(&all, &others) {
            begin_replay::trace(&format!("BEGIN {site_id} is not recorded: {why}"));
            return;
        }
        begin_replay::record_site(site_id, value, &changed, recording.inputs);
    }

    /// Whether the unit lexicals a recorded body reaches hold what they held
    /// when it was recorded: only then is the recorded result its result now.
    // Cost: O(v), v = size of those lexicals (encoded).
    pub(super) fn begin_inputs_match(&self, inputs: &[(usize, Vec<u8>)]) -> bool {
        inputs.iter().all(|(slot, before)| {
            self.locals
                .get(*slot)
                .and_then(|v| begin_replay::encoding_of(&v.deref_container()))
                .is_some_and(|now| now == *before)
        })
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
