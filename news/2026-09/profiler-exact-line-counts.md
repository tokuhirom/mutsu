# Exact line, routine and callsite counts

The profiler now records exact per-line transition counts, routine entries, and callsite calls in
per-thread tables. Report rendering will consume these counters in the later profiler output slice.

**Correction (2026-09-18):** this entry first claimed that JIT-compiled chunks emit the same
line-entry hooks while profiling is armed, "so their counts remain complete rather than silently
partial". Measurement says otherwise. The JIT emits its line hook on *backedges*, and no Raku loop
form puts a backward jump inside a compiled range — `while`, `for`, `loop`, C-style `loop` and
`repeat` all compile to compound opcodes whose body is a separate compiled range with no backedge of
its own. What a native body actually contributes is the once-per-entry poll in `try_enter_range`
(one line hit per iteration, at the range's first instruction), and a compiled whole-function body
contributes nothing at all. So the counts are exact with the JIT off and coarse with it on. Tracked,
with the measurement and the fix, as [#8713](https://github.com/tokuhirom/mutsu/issues/8713); ADR-0106
§8.2 records what it means for gate 4.
