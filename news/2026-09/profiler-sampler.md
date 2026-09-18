# Something now records which Raku line is running when the timer fires

ADR-0106 Slice 2 ([#8702](https://github.com/tokuhirom/mutsu/issues/8702)) adds the profiler's time
half. A detached timer thread bumps one global epoch at `MUTSU_PROFILE_RATE` Hz (1000 by default);
every VM poll compares that epoch against a thread-local `last_seen` and, when the two differ, reads
one clock, walks the `RoutineFrame` stack, and copies fixed-size records into a per-thread buffer
reserved at arm time. Folding those into `(file, line)` and routine tables happens when a buffer
fills or at exit, never on the sample path. Together with the exact counters from Slice 3, a profile
now has NYTProf's two columns: *how many times* (exact) and *how long* (sampled).

There is deliberately no registry of mutator threads to keep in step with `clone_for_thread` and the
worker pool — the epoch reaches every thread that polls, which is exactly the set of threads running
Raku code. A thread blocked in a native call does not poll and therefore contributes nothing; the
report header says so (`blocked_threads_absent=1`) so nobody reads a missing thread as an idle one.

## Three things the design had to get right, and did not at first

**An interval belongs to the region that delayed the poll, not to the poll site.** A poll-based
sampler never sees where a tick fired; it finds out at the next poll, by which point the region that
was running has finished and the thread is standing at the start of the next one. Crediting the line
the poll landed on shifts a profile one region late — systematically, and on Raku code, where a line
is a handful of opcodes, "one region late" means a hot line's cost is reported against the line below
it. The sampler credits the *previous* poll's line instead. In the interpreter the polls are per
opcode and in JIT-compiled code the emitted hooks are per line transition, so in both the previous
poll bounds exactly the region that delayed this one. This is also what makes the elapsed-time
weighting do its ADR-0106 §7 job against safepoint bias: a long native region has its whole duration
charged to the line that entered it.

**A JIT-compiled body has to sample per line, not once per native entry.** The first working version
sampled only from `vm_poll::poll_code`, which native code reaches at `try_enter_range` and on
backedges. Since no Raku loop form places a backward jump inside a compiled range
(ADR-0106 §8.2), that meant one sample per native body entry, carrying the range's first ip — so a
hot loop reported all of its time against its first line and the other body lines were *absent*, not
merely under-weighted. This is the time-half of exactly what
[#8713](https://github.com/tokuhirom/mutsu/issues/8713) fixed for the counts, and it has the same
fix: the sampler now rides the per-line hook the JIT already emits while armed, so JIT-on and JIT-off
runs name the same lines, routines and caller edges (ADR-0106 §8 gate 4).

**Time spent not running Raku is subtracted.** A GC collect, a stop-the-world park, and every
blocking `sleep`/join/read (all of which already funnel through `gc::block_quiescent`) are wrapped in
`profile::exclude_non_raku`, so their duration is discounted from the next sample's weight instead of
landing on whichever line happened to reach the poll. A fixture that spins briefly and then sleeps
for a second now reports about 20ms of sampled time against a second of wall clock. Naming *which*
subsystem that time went to instead is [#8704](https://github.com/tokuhirom/mutsu/issues/8704).

## Two neighbouring defects the threaded case exposed

Both were latent in the Slice 3 counters and became visible as soon as a `start` block was profiled.

- **A live thread's data was never collected.** Tables were folded from `Drop`, and a worker-pool
  thread is still alive when the process reports, so everything it counted was lost: a routine called
  once on the mainline and once on a worker reported `entries=1`. Per-thread tables are now
  *registered* as well, so the report drains threads that are still running. The counters' hot path
  is unchanged — the "still on the same line" poll reads one `Cell` and takes no lock.
- **`def_file: None` split one routine into two rows.** It means "the same file as the caller", and
  the frames pushed on a worker do not always carry the declaring file, so `spun` appeared twice with
  different times. Both halves now resolve it (`frame.def_file.or(frame.file)`).

## What is asserted, and what deliberately is not

ADR-0106 D5 permits a test to assert counts and structure and nothing else, so `tests/profile_samples.rs`
asserts which locations a profile names, which routines and caller edges it links, and which threads
it covers — never a duration or a sample count. `MUTSU_PROFILE_TICK=every-poll` is the lever that
makes that possible: it replaces the timer with "every poll is a tick", so the set of samples a run
takes is a function of the bytecode rather than of the clock. Its *times* are not a ground truth —
the mode's own per-poll overhead is several times the work it measures — and no test reads them. The
single exception is the blocked-time check, which is a one-sided bound that cannot flake: a `sleep`
is excluded by construction, so load can only widen the gap it opens.

## Also filed

[#8719](https://github.com/tokuhirom/mutsu/issues/8719): one source file has two identities at
runtime — a chunk carries the canonicalized path while a `RoutineFrame` carries `$?FILE` as the user
spelled it — so `line` rows and `callsite` rows could name the same file differently and could not be
joined. The report reconciles them in `src/profile/paths.rs` for now; settling it at the source means
deciding what `$?FILE` and backtraces report, which several `t/` tests pin.
