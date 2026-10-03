//! The taps of a `Channel.Supply`, as competing consumers of the channel queue.
//!
//! rakudo's `Channel.Supply` is an on-demand `supply { }` whose body, per tap,
//! drains whatever is already queued and then polls one value per send
//! notification. Each tap is therefore one more consumer of the queue,
//! competing with `receive`/`poll` and with every other tap: a value leaves the
//! queue for exactly one of them, and a value sent before any tap existed stays
//! queued until one takes it.
//!
//! mutsu models a tap as its on-demand emitter attached here. The runtime side
//! (`runtime::native_methods::channel_supply`) pumps queued values out to the
//! attached emitters and completes them once the channel is closed and drained.

use super::*;

/// How a closed and drained channel ended, for completing its taps.
pub(crate) enum ChannelEnd {
    /// `close`: the taps are done.
    Done,
    /// `fail`: the taps quit with this reason.
    Quit(Value),
}

impl SharedChannel {
    /// Attach a tap's emitter as a consumer of this channel; returns the id
    /// that detaches it.
    // Cost: O(1).
    pub(crate) fn attach_tap(&self, emitter: Value) -> u64 {
        let (lock, _) = &*self.inner;
        let mut state = lock.lock().unwrap();
        let id = state.next_tap_id;
        state.next_tap_id += 1;
        state.taps.push(ChannelTap {
            id,
            emitter,
            ready: false,
        });
        id
    }

    /// The `.tap` call that attached this tap has finished registering it.
    // Cost: O(t), t = attached taps.
    pub(crate) fn mark_tap_ready(&self, id: u64) {
        let (lock, _) = &*self.inner;
        let mut state = lock.lock().unwrap();
        if let Some(tap) = state.taps.iter_mut().find(|t| t.id == id) {
            tap.ready = true;
        }
    }

    /// Detach a tap (its `Tap` was closed). Values still queued stay for the
    /// remaining consumers.
    // Cost: O(t), t = attached taps.
    pub(crate) fn detach_tap(&self, id: u64) {
        let (lock, _) = &*self.inner;
        lock.lock().unwrap().taps.retain(|t| t.id != id);
    }

    /// Every attached tap, as `(id, emitter, marked ready)`.
    // Cost: O(t), t = attached taps.
    pub(crate) fn tap_emitters(&self) -> Vec<(u64, Value, bool)> {
        let (lock, _) = &*self.inner;
        let state = lock.lock().unwrap();
        state
            .taps
            .iter()
            .map(|t| (t.id, t.emitter.clone(), t.ready))
            .collect()
    }

    /// Take the next queued value for one of the `ready` taps, chosen round
    /// robin, as `(emitter, value)`. `None` when the queue is empty or none of
    /// the `ready` taps is still attached.
    // Cost: O(t * r), t = attached taps, r = `ready.len()`.
    pub(crate) fn take_for_tap(&self, ready: &[u64]) -> Option<(Value, Value)> {
        let (lock, _) = &*self.inner;
        let mut state = lock.lock().unwrap();
        if state.queue.is_empty() {
            return None;
        }
        let candidates: Vec<usize> = state
            .taps
            .iter()
            .enumerate()
            .filter(|(_, t)| ready.contains(&t.id))
            .map(|(i, _)| i)
            .collect();
        if candidates.is_empty() {
            return None;
        }
        let pick = candidates[state.tap_turn % candidates.len()];
        state.tap_turn = state.tap_turn.wrapping_add(1);
        let emitter = state.taps[pick].emitter.clone();
        let value = state.queue.pop_front()?;
        Self::finish_if_drained(&mut state);
        Some((emitter, value))
    }

    /// How the channel ended, once it is closed *and* drained; `None` while it
    /// can still produce values.
    // Cost: O(1).
    pub(crate) fn end_state(&self) -> Option<ChannelEnd> {
        let (lock, _) = &*self.inner;
        let state = lock.lock().unwrap();
        Self::end_of(&state)
    }

    /// Once the channel is closed and drained, detach the `ready` taps and
    /// return their emitters with how the channel ended, so each is completed
    /// exactly once. A tap that is not ready yet stays attached and is
    /// completed by the pump that runs once it is.
    // Cost: O(t * r), t = attached taps, r = `ready.len()`.
    pub(crate) fn take_ended_taps(&self, ready: &[u64]) -> Option<(Vec<Value>, ChannelEnd)> {
        let (lock, _) = &*self.inner;
        let mut state = lock.lock().unwrap();
        let end = Self::end_of(&state)?;
        let mut ended = Vec::new();
        state.taps.retain(|t| {
            if ready.contains(&t.id) {
                ended.push(t.emitter.clone());
                false
            } else {
                true
            }
        });
        Some((ended, end))
    }

    fn end_of(state: &ChannelState) -> Option<ChannelEnd> {
        if !state.drained_closed {
            return None;
        }
        Some(match &state.failure {
            Some(reason) => ChannelEnd::Quit(reason.clone()),
            None => ChannelEnd::Done,
        })
    }
}
