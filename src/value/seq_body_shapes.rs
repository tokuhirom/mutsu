//! Miri-checked probes for `SeqBody`'s reification `SyncUnsafeCell` +
//! generation-graveyard pattern (docs/adr/0034 phase 5), modeled directly on
//! [`crate::value::native_cache_shapes`], which exists for the same reason:
//! the borrow checker offers no protection at a `SyncUnsafeCell` write site
//! (`SeqBody::pull_and_store` / `SeqBody::pull_io_lines_prefix`), so pin the
//! shapes under Miri's Stacked Borrows model instead of reasoning about them.
//!
//! `SeqBody` reuses the exact graveyard technique `NativeBacking` uses for
//! the native `array[T]` decode cache (docs/adr/0030 §2.2): a generation is
//! never overwritten in place, only appended, so a `&Vec<Value>` obtained
//! from an earlier generation stays valid across a later push through the
//! same shared `&self`. A plain `Seq` (backed by `SeqSource::Iterator`/
//! `IoLines`) reifies **at most once** (seed -> reified), so
//! [`first_reference_survives_a_later_reify`] below exercises that single
//! push. The `IO::Handle.lines`/`.words` streaming-subscript path
//! (`SeqBody::pull_io_lines_prefix`) is the one place a body's graveyard
//! grows past one real (non-seed) generation, so
//! [`retired_generations_are_never_overwritten_in_place`] drives that path
//! directly to get two genuinely distinct, non-empty generations alive at
//! once — the stronger shape docs/adr/0034 phase 5 (b) asks for.

#[cfg(test)]
mod tests {
    use crate::value::{SeqBody, SeqSource, Value};
    use std::sync::Arc;

    /// A deferred body over a placeholder `Iterator` source — the `Value`
    /// itself is never inspected by the fake `pull` closures below, only the
    /// `SeqBody` state machine around it.
    fn deferred_iterator_body() -> Arc<SeqBody> {
        SeqBody::deferred(SeqSource::Iterator(Value::int(0)))
    }

    /// A deferred body over an `IoLines` source, for the streaming-prefix
    /// path (`SeqBody::pull_io_lines_prefix`). The `handle` `Value` is never
    /// read by the fake `pull_n` closures below (they ignore their `&Value`
    /// argument entirely), so any placeholder works.
    fn deferred_io_lines_body() -> Arc<SeqBody> {
        SeqBody::deferred(SeqSource::IoLines {
            handle: Value::int(0),
            words: false,
            kv: false,
        })
    }

    /// The core shape (docs/adr/0034 phase 5 (a)+(b)): a `&Vec<Value>`
    /// obtained via `Deref` from a not-yet-reified body (reads the empty
    /// seed), then a `reify()` call that pushes the real generation, then
    /// **using the first reference again**. This is the same aliasing shape
    /// `native_cache_shapes::first_reference_survives_a_later_resync` pins
    /// for `NativeBacking` — a write through `&self` (via the
    /// `SyncUnsafeCell`) must not invalidate a reference an earlier shared
    /// borrow of the very same `&self` is still holding.
    #[test]
    fn first_reference_survives_a_later_reify() {
        let body = deferred_iterator_body();
        let first = body.live_generation();
        assert_eq!(first.len(), 0, "nothing pulled yet, so the seed is empty");
        let reified = body
            .reify(|_source| Ok(vec![Value::int(10), Value::int(20), Value::int(30)]))
            .unwrap();
        assert_eq!(reified.len(), 3, "reify() pulled the real elements");
        // The line a naive overwrite-in-place implementation would make UB:
        // reading `first` again, after the push `reify()` just made.
        assert_eq!(
            first.len(),
            0,
            "the pre-reify reference still reads the empty seed"
        );
    }

    /// Generation stability under real, non-empty content (docs/adr/0034
    /// phase 5 (b)): drive `pull_io_lines_prefix` twice (the one path that
    /// grows a `SeqBody`'s graveyard past a single real generation) to get
    /// two distinct, non-empty generations alive at once, then prove the
    /// retired one still reads its own original content while a fresh
    /// borrow reads the new, longer generation. Mirrors
    /// `native_cache_shapes::retired_generations_are_never_overwritten_in_place`.
    #[test]
    fn retired_generations_are_never_overwritten_in_place() {
        let body = deferred_io_lines_body();
        body.pull_io_lines_prefix(1, |_handle, _words, n| {
            Ok(((0..n as i64).map(Value::int).collect(), false))
        })
        .unwrap();
        let gen1 = body.live_generation();
        assert_eq!(gen1.len(), 1);
        let gen1_ptr = gen1 as *const Vec<Value>;

        body.pull_io_lines_prefix(3, |_handle, _words, n| {
            Ok(((100..100 + n as i64).map(Value::int).collect(), false))
        })
        .unwrap();
        let gen2 = body.live_generation();
        assert_eq!(
            gen2.len(),
            3,
            "the fresh borrow sees the newly-pulled elements"
        );
        assert_eq!(gen2[0], Value::int(0), "the prefix carried over from gen1");
        assert_eq!(gen2[1], Value::int(100), "the newly-pulled elements follow");

        // SAFETY: `gen1_ptr` was derived from `body.live_generation()` and
        // `body` is still alive and not otherwise mutably borrowed here;
        // dereferencing it is
        // exactly the "read the retired generation" this test pins — the
        // graveyard (module docs on `SeqBody`) never overwrites an existing
        // slot, only appends, so this read stays sound after the second push.
        let gen1_contents = unsafe { &*gen1_ptr };
        assert_eq!(
            gen1_contents.len(),
            1,
            "the retired generation still reads its own original content, not gen2's"
        );
        assert_eq!(gen1_contents[0], Value::int(0));
    }

    /// `Sync` posture (docs/adr/0034 phase 5 (c)): `SeqBody` must stay
    /// `Sync`, and `Arc<SeqBody>` (how every `Value::Seq` handle actually
    /// shares one body) must stay `Send + Sync` — the whole reason
    /// `SyncUnsafeCell` exists instead of a plain `UnsafeCell` field (see
    /// `sync_cell.rs`, and `native_cache_shapes`'s equivalent assertion). A
    /// compile failure here means the cell's bound regressed.
    #[test]
    fn seq_body_and_its_arc_stay_send_and_sync() {
        fn assert_send<T: Send>() {}
        fn assert_sync<T: Sync>() {}
        assert_sync::<SeqBody>();
        assert_send::<Arc<SeqBody>>();
        assert_sync::<Arc<SeqBody>>();
    }

    /// Write visibility across threads (docs/adr/0034 phase 5 (d)): two
    /// `Arc<SeqBody>` clones share one `Mutex<SeqState>` +
    /// `SyncUnsafeCell<Vec<Box<Vec<Value>>>>` core, so a `reify()` on one
    /// clone (from a spawned thread, forcing the `Send + Sync` bound above to
    /// actually matter) must be visible through the other. The second
    /// `reify()` call's `pull` closure panics if invoked at all, which
    /// proves the second clone served the already-reified generation instead
    /// of pulling again.
    #[test]
    fn two_arc_clones_observe_one_reify_call() {
        let body = deferred_iterator_body();
        let clone = Arc::clone(&body);
        let handle = std::thread::spawn(move || {
            clone
                .reify(|_source| Ok(vec![Value::int(1), Value::int(2)]))
                .cloned()
        });
        let pulled = handle.join().unwrap().unwrap();
        assert_eq!(pulled, vec![Value::int(1), Value::int(2)]);

        let seen = body
            .reify(|_source| panic!("already reified; must not pull a second time"))
            .unwrap();
        assert_eq!(*seen, vec![Value::int(1), Value::int(2)]);
    }
}
