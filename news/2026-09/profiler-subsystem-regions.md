# A profiled line now names the interpreter subsystem that consumed it

ADR-0106 Slice 4 ([#8704](https://github.com/tokuhirom/mutsu/issues/8704)) closes the gap between the
Raku-level and Rust-level views of a slow program. Until now a mutsu profile could say "line 412 is
41% of the run", which for an interpreter is half an answer: the time is nearly always spent in
*mutsu's* code on that line's behalf, and which part matters. Call resolution, method dispatch, the
regex walk, the parser, a native builtin, an `nqp::` op and GC are very different diagnoses — the
first is a mutsu bug report, the last few are the user's own code being slow — and without the split
every profile ended in the same next step: open callgrind and re-derive it by hand.

A sample now carries a `Region` tag, and the report prints three new things: a whole-run split
(`profile: region regex ns=… samples=…`), a per-`(file, line)` split
(`profile: self-line-region fixture.raku:4 regex ns=…`), and a `top_region=` field in the header.

## The mechanism, and the one that does not work

The obvious design — a thread-local "current region" the sampler reads when it takes a sample — is
the one the ADR sketched, and it cannot work. mutsu's sampler is poll-based: a tick is noticed at the
next VM poll, and the polls are in the bytecode dispatch loops. The regions worth naming are
precisely the long native stretches that *do not* poll, so by the time a poll notices the tick the
region has already returned and the "current region" reads `interp` every single time. A tag read at
the sample point would report that mutsu spends ~100% of its time interpreting bytecode.

So the region claims the tick instead. Leaving a region costs one relaxed load and a compare: if the
epoch moved while this region was running, the region latches itself, and the next poll's sample —
whose elapsed weight covers exactly the stretch that region occupied — is tagged with it. Nothing is
measured on the region path: no clock read, no hash lookup, no allocation, no atomic store. That
matters beyond cost, because a region that paid for its own instrumentation would inflate precisely
the number it exists to report.

The first claim wins, and that is correct rather than arbitrary. A claim only happens when the tick
was *already* pending at that exit, so the claiming region was genuinely running at the tick instant
and any region that runs afterwards demonstrably was not. The rule reads correctly in both shapes it
has to handle: nested (`method-dispatch { call-resolve }`, where the inner guard drops first and the
enclosing one finds the latch taken) and sequential (a resolution walk, then a parse, with no poll
between them).

There is no `unknown` tag. A sample nothing claimed was taken while the thread was running bytecode,
and `interp` says exactly that — it is an answer, not a residue bucket.

## GC time is named instead of being a hole

Slice 2 already subtracted GC collects and stop-the-world parks from the sampled weight, so that time
does not silently land on whichever line reached the safepoint. That left a hole in the profile: the
wall clock and the sampled total disagreed and nothing said why. The subtraction already costs two
clock reads, so those reads now also *measure* the interval, and the report carries a separate
`excluded-region` table. It is deliberately a distinct row type, because it is measured rather than
sampled and it is time the line table above does **not** contain.

## How it is tested without shipping a flaky test

The region's nanoseconds are sampled, so nothing asserts one (ADR-0106 D5). Under
`MUTSU_PROFILE_TICK=every-poll` a tick is pending in every gap between polls, so which region claims
each sample becomes a function of the executed bytecode alone. `tests/profile_regions.rs` pins that
property first and then leans on it: a fixture matching one regex per trip through a seven-iteration
loop reports exactly seven `regex` samples, attached to the line holding the match; a fixture with no
regex in it reports no `regex` row at all rather than a zero; two `nqp::` ops per trip produce
fourteen `nqp` samples; every tag printed is from the closed set, and the tags partition the samples
the header counted, so a sample cannot go untagged.

## Where the tags are set

Eight chokepoints, each of which the VM already ran through: `resolve_function_with_types` (where the
`MUTSU_VM_STATS` full-resolve counter already sits), the VM and interpreter method-dispatch entries,
the native builtin cascades on both of those paths, `dispatch_nqp_op`, ADR-0099's regex walk
(`regex_walk_ends_in_pkg`), `parser::parse_program`, and the GC collect and stop-the-world park.
Five of them are re-entrant; the claim-at-exit rule handles that without a depth counter, because an
inner guard drops first and an outer claim finds the latch already taken.
