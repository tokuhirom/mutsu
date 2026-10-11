# The frame-writes dedup regression is pinned deterministically

`t/vm/frames/light-call-frame-writes-dedup.t` guarded the O(n^2) growth of
`Env::frame_writes` (#8489) with a wall-clock ratio (`via < direct * 1.5`),
which still failed intermittently under contended suites (#12655).

The invariant is now pinned in `src/env.rs` unit tests: repeated writes to one
name never grow the log, and a write to an already-logged name does not copy a
log shared with a saved caller env (checked by the log's address staying put).
Disabling the dedup in `note_frame_write` fails both. The `.t` file keeps the
end-to-end shape (both paths run `.make()` to completion) with no timing.
