# Exact line, routine and callsite counts

The profiler now records exact per-line transition counts, routine entries, and callsite calls in
per-thread tables. JIT-compiled chunks emit the same line-entry hooks while profiling is armed —
`helpers::profile_line` at chunk entry, at every jump target, and at every sequential line transition
— so their counts remain complete rather than silently partial. Report rendering will consume these
counters in the later profiler output slice.

**Both halves of that are now measured** (2026-09-18, [#8713](https://github.com/tokuhirom/mutsu/issues/8713)).
On a `while` loop whose body runs exactly 5,000 times, each body line reports `hits=5000` and the
condition line `hits=5001` — the trip count, exactly, which is ADR-0106 §8 gate 3 — and running the
same fixture with the JIT off and with `MUTSU_JIT=on MUTSU_JIT_THRESHOLD=1` produces identical line,
routine and callsite tables, which is gate 4. Neither could be asserted when the counters landed:
`flush_at_exit` folded its snapshot into a static nothing read, so nothing outside the crate could see
a count, and the only test hand-built a `CompiledCode` and called `record_line` directly. It now prints
what it counted — scaffolding the report slice will replace, but enough for `tests/profile_counts.rs`
to pin the gates end to end, against the real interpreter and the real generated code.

An earlier correction appended to this entry claimed the JIT hooks did *not* provide complete
coverage. That was wrong, and is withdrawn: it came from grepping the JIT emitter for the `vm_poll`
function name instead of the helper's, finding only the backedge hooks, and inferring the rest.
