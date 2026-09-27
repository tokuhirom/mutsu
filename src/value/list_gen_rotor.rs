//! The `.rotor` stepper: one implementation shared by the eager
//! `Interpreter::dispatch_rotor` and the lazy [`crate::value::ListGen::Rotor`]
//! (#9158).

use crate::value::Value;
use std::sync::Arc;

/// How many elements one `.rotor` spec takes.
#[derive(Debug, Clone)]
pub(crate) enum RotorCount {
    /// A fixed count.
    Fixed(usize),
    /// `*` / `Inf` / `**`: everything that is left.
    Rest,
    /// `a..b` / `a..*`: the counts `a, a+1, ...`, cycling after `len` of them
    /// (`len` `None`: unbounded).
    Range { start: i64, len: Option<usize> },
}

/// One `.rotor` spec: take `count` elements, then move on by `count + gap`.
#[derive(Debug, Clone)]
pub(crate) struct RotorSpec {
    pub(crate) count: RotorCount,
    pub(crate) gap: i64,
}

/// Why a [`RotorState::step`] could not continue.
#[derive(Debug, Clone, Copy)]
pub(crate) struct RotorUnderflow {
    /// The (negative) index the gap would have moved the cursor to.
    pub(crate) new_pos: i64,
}

/// The cursor of one `.rotor` over a list.
#[derive(Debug, Clone)]
pub(crate) struct RotorState {
    specs: Arc<Vec<RotorSpec>>,
    partial: bool,
    pos: usize,
    spec_idx: usize,
    /// Position inside the current `Range` spec's counts.
    range_sub_idx: usize,
    /// Cursor position at the start of the current spec cycle. A full pass
    /// through every spec that advances the cursor by zero (a lone
    /// `rotor(0)`, `rotor(0, 0)`, or `rotor(2 => -2)`, where count + gap is 0
    /// for every spec) would never terminate, so it stops instead. A mixed
    /// cycle that does advance somewhere (`rotor(0, 1, *)`) is fine.
    cycle_start_pos: usize,
    emitted_any: bool,
    done: bool,
}

impl RotorState {
    // Cost: O(1).
    pub(crate) fn new(specs: Vec<RotorSpec>, partial: bool) -> Self {
        let done = specs.is_empty();
        RotorState {
            specs: Arc::new(specs),
            partial,
            pos: 0,
            spec_idx: 0,
            range_sub_idx: 0,
            cycle_start_pos: 0,
            emitted_any: false,
            done,
        }
    }

    /// Whether some spec can move the cursor backwards past the start of the
    /// list (a negative gap larger than its count), which raises
    /// `X::OutOfRange` mid-iteration. Only a rotor that cannot is run lazily:
    /// a pure iterator has no way to throw.
    // Cost: O(s), s = specs.
    pub(crate) fn can_underflow(specs: &[RotorSpec]) -> bool {
        specs.iter().any(|spec| {
            let min_count = match spec.count {
                RotorCount::Fixed(n) => n as i64,
                RotorCount::Rest => 0,
                RotorCount::Range { start, .. } => start.max(0),
            };
            min_count.saturating_add(spec.gap) < 0
        })
    }

    /// The next sub-list of `items`, `Ok(None)` at the end.
    // Cost: O(c) for a sub-list of c elements, plus O(s) for specs that
    // produce nothing, s = specs.
    pub(crate) fn step(&mut self, items: &[Value]) -> Result<Option<Value>, RotorUnderflow> {
        while !self.done {
            if self.pos >= items.len() {
                self.done = true;
                break;
            }
            // Detect a non-advancing full cycle at each cycle boundary.
            if self.spec_idx > 0 && self.spec_idx.is_multiple_of(self.specs.len()) {
                if self.pos == self.cycle_start_pos {
                    self.done = true;
                    break;
                }
                self.cycle_start_pos = self.pos;
            }
            let spec = &self.specs[self.spec_idx % self.specs.len()];
            let count = match &spec.count {
                RotorCount::Fixed(n) => *n,
                RotorCount::Rest => items.len() - self.pos,
                RotorCount::Range { start, len } => {
                    let sub = match len {
                        Some(n) if *n > 0 => self.range_sub_idx % n,
                        _ => self.range_sub_idx,
                    };
                    match start.checked_add(sub as i64) {
                        Some(c) if c >= 0 => c as usize,
                        Some(_) => 0,
                        None => items.len() - self.pos,
                    }
                }
            };
            let gap = spec.gap;
            // Take `count` items starting at pos.
            let end = std::cmp::min(self.pos.saturating_add(count), items.len());
            let chunk_len = end - self.pos;
            let mut emitted = None;
            if chunk_len == count || (self.partial && (chunk_len > 0 || count == 0)) {
                // When gap is negative and chunk is partial (not first chunk),
                // only emit if the chunk has enough elements to contain at least
                // one new element not already covered by the previous chunk's
                // overlap.
                let skip_partial =
                    chunk_len < count && gap < 0 && self.emitted_any && (chunk_len as i64) < -gap;
                if !skip_partial {
                    emitted = Some(Value::array(items[self.pos..end].to_vec()));
                    self.emitted_any = true;
                }
            }
            if chunk_len < count {
                self.done = true;
                return Ok(emitted);
            }
            // Advance position: count + gap (gap can be negative for overlap).
            let new_pos = (self.pos as i64).saturating_add((count as i64).saturating_add(gap));
            if new_pos < 0 {
                self.done = true;
                return Err(RotorUnderflow { new_pos });
            }
            self.pos = new_pos as usize;
            match &spec.count {
                RotorCount::Range { len, .. } => {
                    self.range_sub_idx += 1;
                    if len.is_some_and(|n| self.range_sub_idx >= n) {
                        self.range_sub_idx = 0;
                        self.spec_idx += 1;
                    }
                }
                _ => self.spec_idx += 1,
            }
            if emitted.is_some() {
                return Ok(emitted);
            }
        }
        Ok(None)
    }
}
