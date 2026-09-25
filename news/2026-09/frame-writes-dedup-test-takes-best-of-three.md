# The frame-writes dedup timing test takes the best of three runs

`t/vm/frames/light-call-frame-writes-dedup.t` pins the O(n^2) growth of
`Env::frame_writes` (#8489) by comparing two wall-clock timings: `.make()`
called directly versus called through a lazy-singleton accessor, with
`via < direct * 1.5` as the check. Each path was sampled exactly once, and
each sample is only ~0.15-0.2s long, so under `prove -j4` load a few tens of
milliseconds of scheduler noise pushed the ratio past 1.5 (1.54 and 1.56 in
two unrelated PRs' `make test` runs on a 4-core container, while standalone
reruns measured 1.01-1.17) (#9299).

The test now runs the two paths interleaved three times and compares the
minimum of each. Noise only ever adds time, so the minimum discards it; the
regression it guards against is a deterministic extra cost (about 2x at this
size before the fix) that every sample pays, so it survives the minimum
unchanged. Interleaving also stops the direct path from always being the
cold first run. With four copies of the test plus a CPU burner running at
once, every run measured between 0.97x and 1.28x.
