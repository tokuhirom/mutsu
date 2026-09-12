//! The cross-thread publish point for a mutable native instance method.
//!
//! Every mutable native method is a read-modify-write over a **private** copy of
//! the receiver's attribute map: the handler is given the map by value, mutates
//! it freely, and the dispatcher
//! ([`crate::runtime::Interpreter::call_native_instance_method_mut_in_place`])
//! commits the delta into the shared cell when the handler returns. That private
//! copy is what makes most handlers simple and correct — a handler that fails
//! part-way publishes nothing, so a `?` cannot leave half a transition behind.
//!
//! It is also a visibility window. Nothing the handler writes is visible to any
//! other thread until it returns, so a handler that *wakes another thread
//! mid-flight* — keeps a promise, emits to a supply, sends on a channel, spawns
//! — has published none of its writes at the moment the woken thread starts
//! reading the receiver. `Proc::Async.start` is the case that first bit
//! (tokuhirom/mutsu#7923): it keeps `.ready` as soon as the child is spawned, and
//! a thread blocked on `await $p.ready` read `started` straight back off the
//! instance and saw the pre-spawn map, so `.kill` threw
//! `X::Proc::Async::MustBeStarted`.
//!
//! [`AttrPublisher`] is the publish point that makes the window closable, and
//! the convention that goes with it is one rule:
//!
//! > **A mutable native handler must publish before any operation that can wake
//! > another thread which reads the receiver.**
//!
//! See `docs/adr/0095-native-mut-publish-before-wake.md`.
//!
//! # Why this is not just `cell.insert(key, value)`
//!
//! Writing straight through the cell (what `Proc::Async.start` did by hand
//! before this type existed) publishes the value but leaves the dispatcher's
//! before-image describing the map as it was read. At return the delta commit
//! then still sees that key as *changed* and stores it again — overwriting
//! whatever another thread wrote to it in between, which is exactly the lost
//! update `InstanceAttrs::commit_attrs_delta` exists to prevent. Publishing
//! through this type **rebases** the before-image onto what was published, so a
//! key the handler published and never touched again is no longer part of its
//! delta, and the other thread's later write survives.

use crate::value::{AttrBits, AttrMap, InstanceAttrs};

/// The publish point handed to every mutable native instance handler.
///
/// Holds the receiver's live attribute cell (when the caller has one) together
/// with the before-image the final delta commit diffs against, so that
/// publishing mid-flight and committing at return compose correctly.
pub(in crate::runtime) struct AttrPublisher<'a> {
    /// The receiver's shared attribute cell. `None` when the receiver was not
    /// dispatched from a live instance (the map is then the only copy there is,
    /// and publishing is a no-op).
    cell: Option<&'a InstanceAttrs>,
    /// The boxed-word image the next commit diffs against: the map as it was
    /// read, rebased onto every intervening [`AttrPublisher::publish`].
    before: AttrBits,
}

impl<'a> AttrPublisher<'a> {
    /// Open a publish point over `cell`, against the before-image `before` of the
    /// working map the handler is about to be given.
    pub(in crate::runtime) fn new(cell: Option<&'a InstanceAttrs>, before: AttrBits) -> Self {
        Self { cell, before }
    }

    /// A publish point with no cell behind it, for the handful of sites that run
    /// a mutable handler purely for its return value and discard the map (the
    /// immutable `IO::CatHandle` and `Supply.tap` entries). Publishing through it
    /// is a no-op, which is the truth: there is no shared receiver to publish to.
    pub(in crate::runtime) fn detached() -> Self {
        Self {
            cell: None,
            before: AttrBits::default(),
        }
    }

    /// Publish everything the handler has written to `working` so far, making it
    /// visible to every other thread holding this instance.
    ///
    /// Call this immediately **before** waking another thread that can read the
    /// receiver — keeping a promise, emitting to a supply, sending on a channel,
    /// spawning — never after: the woken thread may already be reading by the
    /// time the wake primitive returns.
    ///
    /// Idempotent and cheap when nothing changed: an unchanged key is not in the
    /// delta, and an empty delta takes no write lock at all.
    pub(in crate::runtime) fn publish(&mut self, working: &AttrMap) {
        let Some(cell) = self.cell else {
            return;
        };
        cell.commit_attrs_delta(&self.before, working);
        // Rebase: what was just published is the new baseline, so these keys are
        // no longer part of this handler's delta and a write another thread lands
        // on them from here on is not clobbered at return.
        self.before = working.bits_image();
    }

    /// Commit the handler's final map. Consumes the publish point, so the
    /// dispatcher's commit is the last word on the before-image.
    pub(in crate::runtime) fn commit(self, updated: &AttrMap) {
        if let Some(cell) = self.cell {
            cell.commit_attrs_delta(&self.before, updated);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::symbol::Symbol;
    use crate::value::Value;

    fn cell() -> InstanceAttrs {
        InstanceAttrs::new(Symbol::intern("T"), AttrMap::new(), 1, false)
    }

    /// The visibility window (tokuhirom/mutsu#7943 consequence 1): what a handler
    /// writes into its private working map is invisible to every other holder of
    /// the instance until it returns — unless it publishes.
    #[test]
    fn publish_makes_a_mid_flight_write_visible_before_the_handler_returns() {
        let cell = cell();
        let mut working = cell.to_map();
        let mut publish = AttrPublisher::new(Some(&cell), working.bits_image());

        // The handler writes, as a handler does: into its own copy.
        working.insert("started", Value::TRUE);
        assert_eq!(
            cell.as_map().get("started"),
            None,
            "the private working map must not be visible on its own"
        );

        // ...and publishes before the wake.
        publish.publish(&working);
        assert_eq!(cell.as_map().get("started"), Some(&Value::TRUE));
    }

    /// Publishing **rebases** the before-image, so a key the handler published and
    /// never touched again is no longer part of its delta. Writing straight
    /// through the cell instead (the hand-rolled form this type replaced) leaves
    /// the before-image stale, and the commit at return then stores the published
    /// value a second time — over whatever another thread wrote in between.
    #[test]
    fn a_published_key_is_not_re_committed_over_a_later_writer() {
        let cell = cell();
        let working = cell.to_map();
        let before = working.bits_image();
        let mut publish = AttrPublisher::new(Some(&cell), before);

        let mut working = working;
        working.insert("pid", Value::int(41));
        publish.publish(&working);

        // Another thread corrects the key we published and are done with.
        cell.insert("pid", Value::int(42));

        // The handler returns; its final map still carries the published value.
        publish.commit(&working);
        assert_eq!(
            cell.as_map().get("pid"),
            Some(&Value::int(42)),
            "a key the handler published and did not touch again must not be \
             re-committed over a later write"
        );
    }

    /// A key written after the publish is still the handler's, and still lands.
    #[test]
    fn a_write_after_the_publish_still_commits_at_return() {
        let cell = cell();
        let mut working = cell.to_map();
        let mut publish = AttrPublisher::new(Some(&cell), working.bits_image());

        working.insert("started", Value::TRUE);
        publish.publish(&working);
        working.insert("pid", Value::int(7));
        publish.commit(&working);

        assert_eq!(cell.as_map().get("started"), Some(&Value::TRUE));
        assert_eq!(cell.as_map().get("pid"), Some(&Value::int(7)));
    }

    /// A removal after the publish is a removal, not a no-op: the rebased image
    /// still records that the key was there.
    #[test]
    fn a_removal_after_the_publish_reaches_the_cell() {
        let cell = cell();
        let mut working = cell.to_map();
        let mut publish = AttrPublisher::new(Some(&cell), working.bits_image());

        working.insert("emitted", Value::int(1));
        publish.publish(&working);
        assert_eq!(cell.as_map().get("emitted"), Some(&Value::int(1)));

        working.remove("emitted");
        publish.commit(&working);
        assert_eq!(cell.as_map().get("emitted"), None);
    }

    /// A detached publish point has no receiver to publish to, and says so by
    /// doing nothing rather than by panicking.
    #[test]
    fn a_detached_publish_point_is_a_no_op() {
        let mut working = AttrMap::new();
        working.insert("a", Value::int(1));
        let mut publish = AttrPublisher::detached();
        publish.publish(&working);
        publish.commit(&working);
    }
}
