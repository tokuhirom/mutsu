# Deduplicate the light-call frame-writes log, fixing an O(n^2) blowup behind nested singleton accessors

`Env::frame_writes` (ADR-0004 J4d) logs the by-name writes a light-called
frame makes, so a light call's return can merge just those names back into
the caller instead of rescanning the whole scope. The log is documented as a
*set* of names to consider ("order does not matter" --
`Env::retain_frame_writes`'s own comment), but `Env::note_frame_write` used
to append to it unconditionally, with no dedup.

A method entered as a nested light call -- the common lazy
class-level-singleton idiom, `submethod instance { $instance = T.bless
unless $instance; $instance.make() }` -- arms this log for its own frame's
whole lifetime, because calling `.make()` from inside `instance` is one level
of native-call nesting deeper than calling it directly. Every block mutsu
calls via `call_sub_value` (`.map`/`.grep`/`.classify`/...) inside `.make()`
is its own nested light call, and each one's return-time writeback merge
re-inserts its surviving keys into that SAME armed log. A method that
processes data through several such pipeline stages, each rewriting the same
handful of class-scoped `my` variables, therefore appended one entry per
call to an ever-growing `Arc<Vec<Symbol>>`. The moment that `Arc` was shared
with a saved caller env (`push_call_frame`, once per nested call), the next
append had to `Arc::make_mut`-deep-copy the whole already-huge log -- O(n)
extra work per call, O(n^2) over the frame's lifetime.

Found via [#8489](https://github.com/tokuhirom/mutsu/issues/8489): fixing
`Data::Generators`'s `ResourceAccess.instance` (a `blocked_load` ecosystem
record) to *load* surfaced a second bug underneath -- the class's own test
suite still timed out because `ResourceAccess.instance` (which parses an
85,000-line CSV on first access) never finished. Measured on the real
distribution: calling `.bless.make()` directly took ~21-24s; calling the
exact same work through `.instance` did not finish in 60s+. `strace`/`gdb`
on a reduced repro caught the smoking gun directly: a `heap_trim`/`madvise`
call freeing a `Vec<Symbol>` over 122,000 entries long, cycling the same 2-3
symbol IDs -- the write log for one long-lived `.make()` frame, grown by
every nested block call's writeback merge and periodically
`Arc::make_mut`-cloned in full.

The fix makes `note_frame_write` a read-only membership check (no `Arc`
touched, so no copy) before falling through to the `make_mut`-guarded push,
so a key already in the log costs one scan of the (now bounded) log instead
of a clone of the whole thing. The log's size is now bounded by the frame's
*distinct* write targets rather than its total write count, which is what
the "order does not matter" / "duplicate a key" `retain_frame_writes` /
`finish_positional_light_env` consumers already treated it as.

Measured on the real repro: the via-singleton path went from not finishing
in 90s+ to ~24.3s -- matching the direct-call path's ~24.5s almost exactly,
for identical work on identical input. `env::tests` (28 tests) and the new
`t/vm/frames/light-call-frame-writes-dedup.t` regression pin both pass.
