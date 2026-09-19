# ADR-0106: The Raku-level profiler — sampled time over the static ip→line table, exact counts at the chokepoints that already exist

- **Status**: Accepted (Slices 0-5 and opt-in allocation attribution shipped; Slice 6 optional and unstarted — §9)
- **Date**: 2026-09-18
- **Context**: mutsu can measure itself in Rust (callgrind, `MUTSU_ALLOC_STATS`, `MUTSU_VM_STATS`,
  the bench CI) and cannot measure a Raku program at all. Every perf investigation therefore pays a
  manual translation step — from "which Rust symbol is hot" to "which Raku line pays for it" — and a
  mutsu *user* cannot take even the first step. [#8289](https://github.com/tokuhirom/mutsu/issues/8289)
  (the vendored `JSON::Fast` at ~63x/~126x rakudo) is the case that made the gap concrete.
- **Relates to**: [ADR-0006](0006-baseline-interpreter-optimizations.md) (the measurement protocol
  this profiler must not pretend to replace), [ADR-0004](0004-jit-strategy.md) and
  [ADR-0003](0003-default-on-gc-trigger.md) (the safepoint poll network this rides),
  [ADR-0065](0065-language-server-targets-ai-agents.md) (output is for a tool or an agent first, a
  human browser second), [ADR-0085](0085-ecosystem-testsuite-parity-measurement.md) (the ecosystem
  campaign that will consume this), [ADR-0096](0096-batteries-adoption-policy.md) §D3 (a measured gap
  justifies optimizing the *real* module's path — which requires knowing which of its lines is slow),
  [ADR-0099](0099-regex-engine-performance-strategy.md) (a perf strategy ADR whose §2 measurements had
  to be hand-assembled for want of this tool).

## 1. The gap

What mutsu can measure today, and what each thing answers:

| tool | question it answers |
|---|---|
| `callgrind` + `callgrind_annotate --tree=caller` | which **Rust** function costs instructions, and who called it |
| `MUTSU_ALLOC_STATS` + `alloc_scope!` | how many allocations a **Rust region** performs |
| `MUTSU_VM_STATS` | opcode histogram, fallback/carrier dispatch counts, full-resolve counts — per **opcode/name**, not per source location |
| bench CI (`bench-history.tsv`) | whether a **whole benchmark script** got faster since the last main commit |
| `--dump-bytecode`, `MUTSU_TRACE` | what was compiled / what executed, with no cost attached |

Nothing in that table names a line of Raku. Two constituencies are blocked by that:

1. **mutsu's own performance work on real modules.** The #8289 investigation is the worked example.
   A Rust profile of `to-json` says the time is in call resolution and string hashing — true, and
   ~3x too coarse to act on, because it does not say *which of `JSON::Fast`'s own routines and lines*
   run often enough for that to matter. The actual root cause (`nqp::elems`/`nqp::atpos_i` copying the
   whole element vector per call, making `str-escape` quadratic) was found by hand-writing `now`-delta
   micro-benchmarks around suspected calls — i.e. by manually performing, over several hours, exactly
   the attribution a line profiler performs in one run. ADR-0099 §2 is the same story at larger scale:
   its subrule-resolution finding needed a bespoke measurement harness per hypothesis.
2. **Users profiling their own Raku code.** For them callgrind is not a coarse tool, it is *no* tool:
   a profile of `mutsu` tells them about mutsu's internals, which is not information they can act on.
   Perl programmers have had `Devel::NYTProf` since 2008 and reach for it reflexively; the question
   that prompted this ADR is exactly that reflex.

There is a third, quieter reason. ADR-0096 forbids substituting a native implementation for a slow
battery and requires optimizing the real module's path instead. That policy is only executable if the
real module's slow *line* can be found. Without this tool, the pressure to break the policy grows with
every vendored distribution.

## 2. Prior art

### 2.1 Perl — `Devel::NYTProf`, the thing being asked for

Mechanism: a `runops` replacement that fires at every **statement** boundary (each `nextstate`/COP op
carries file+line), plus subroutine entry/exit hooks. Each event reads a high-resolution clock, so
both statement time and sub time are **deterministic** (exact counts, measured times), with
**caller attribution** ("this sub cost 3.1s, of which 2.9s when called from `Foo::bar` line 42") and
correct handling of string `eval`s. The profile is written to a compact binary file and rendered
offline (`nytprofhtml`) into per-source-file pages annotated line by line.

Two properties explain why it displaced everything else in Perl: the report is *the source code with
numbers on it* (no mental mapping needed), and the sub/caller table tells you where to cut, not just
where the time is. The cost is the usual deterministic-profiler tax: commonly 2-6x slowdown, and a
distortion that falls hardest on call-heavy code.

### 2.2 Rakudo / MoarVM — routine-level, and no statement-level anything

`raku --profile[=name]` (instrumented, the default `--profile-kind`) is the built-in. Its JSON output,
inspected directly on this box (rakudo 2026.07, `raku --profile=tmp/prof.json`), is:

- a **frame table**: `{ id → { name, file, line } }` — one entry per routine/block, where `line` is the
  routine's **declaration** line;
- a **call graph**: nodes of `{ id, entries, inclusive_time, exclusive_time, first_entry_time,
  callees, allocations, mono/poly/mega }` (µs; the `mono`/`poly`/`mega` counters are dispatch-inline-cache
  state, and allocations are counted per type).

So it is a call-graph profiler with allocation accounting and spesh/JIT visibility — genuinely good,
and **routine-granular**. Around it:

- `--profile-compile` profiles the compilation phase; `--profile-kind=heap` writes heap snapshots for
  `App::MoarVM::HeapAnalyzer` (`moar-ha`); `--profile-filename` picks the file; the extension picks
  HTML/JSON/SQL.
- **moarperf** (`MoarVM::Profile` in the ecosystem) is the web front-end for that JSON.
- **`Telemetry`** (core module): `snap`/`T`/`snapper` — periodic sampling of *resource counters*
  (wallclock, CPU, GC runs, spesh, thread state), not of code locations. It answers "what was the VM
  doing", never "which line".
- Ecosystem: `Grammar::Profiler::Simple` (per-rule counts/time for grammars), `Grammar::Tracer`,
  `MoarVM::Remote` / `App::MoarVM::Debug` (remote debug protocol, stepping and stack inspection).

**There is no Devel::NYTProf-class statement-level profiler for Raku.** That is worth stating plainly,
because it has two consequences for this decision: there is no upstream format to be compatible with
for the line half (§6g), and a line profiler is not mutsu catching up — it is mutsu ahead.

### 2.3 Python — the clearest split between counting and sampling

- `cProfile`/`profile` + `pstats`: deterministic, **function**-level, built in.
- **`line_profiler` / `kernprof`**: deterministic **line**-level, opt-in per function (`@profile`).
  The direct NYTProf analogue, and instructive: it is opt-in precisely because per-line deterministic
  timing is too expensive to leave on for a whole program.
- `pyinstrument` (sampling, call-stack, low overhead), **`py-spy`** (out-of-process sampling — attaches
  to a running process and reads its memory), `yappi` (per-thread, wall and CPU).
- **`Scalene`**: samples, attributes **per line**, and separates *Python time from native time* and
  memory growth per line. It is the tool whose output most resembles what mutsu needs (§4 D4).
- `memray` for allocations.
- PEP 669 `sys.monitoring` (3.12) gave the VM a set of **arm-able, near-zero-when-off event hooks**
  (including `LINE`), replacing the old blanket `sys.settrace`. The direction of travel is:
  *the VM provides cheap armed hooks; tools stop paying for what they do not use.*

### 2.4 Ruby — same split, arrived at from the other end

- `ruby-prof`: deterministic, C extension, call graphs and several measure modes.
- **`stackprof`**: sampling (`:cpu`/`:wall`/`:object`), and its `:line` mode attributes samples to
  file:line — a sampled line profiler, the exact shape proposed here.
- `vernier`: newer sampling profiler with a timeline view and GVL awareness.
- `rbspy`: out-of-process sampling, like py-spy.
- `TracePoint` (line/call/return events) and `Coverage` (line/branch/method counts) are the VM-provided
  hooks tools build on; the old stdlib `profile.rb` was dropped.

### 2.5 What the field agrees on

1. **Counts are instrumented; time is sampled.** Every ecosystem that started with deterministic
   timing (Perl, Python, Ruby) grew a sampling profiler next to it, and the sampling one is what people
   run first. Exact counts stay valuable and stay cheap.
2. **Line granularity is worth having and nobody pays deterministic timing for it by default.**
   `line_profiler` is opt-in-per-function; `stackprof :line` is sampled.
3. **The most useful modern tool is the one that splits the user's time from the runtime's**
   (Scalene). For a young interpreter whose runtime cost *is* the story, that is not a nice-to-have;
   it is the feature.

## 3. What mutsu already has (why this is much cheaper than it looks)

- **A static ip→line table on every chunk.** `CompiledCode::op_lines: Vec<u32>`, parallel to `ops`,
  with `line_at(ip)`. It is already compiled, already paid for, and is exactly NYTProf's COP data. The
  per-statement `SetSourceLine` opcode was deliberately removed in favour of it (refreshing the line on
  every op measured +7.8% instructions on fib).
- **A poll network, already placed, already armed by one cached load.** `gc_safepoint(SafepointKind)`
  has ten kinds — `Backedge`, `Call`, `Return`, `Await`, `ReactPoll`, `LazyForce`, `NestedRun`,
  `ThreadJoin`, `Manual`, construction — sited across all thirteen bytecode dispatch loops and the
  async machinery, gated by `gc::armed()` (a cached bool; `MUTSU_GC=off` pays one load).
- **The JIT polls it too.** `vm_jit_compile.rs` emits a call to `vm_jit_helpers::safepoint` on native
  backedges (ADR-0004 §2.4), and `vm_jit.rs` polls on entry. A profiler riding this network gets
  JIT-compiled code covered on day one rather than as a special case.
- **A Raku-level call stack.** `RoutineFrame { package, name, line, file, def_file, is_method,
  is_block, invocation_id }` is pushed on every dispatch path (ADR-0037 slice 1), which is the
  shadow stack a sampler needs — no new bookkeeping.
- **`FunctionData::source_file` / `source_file_sym()`** — per-routine file identity with a memoized
  `Symbol`.
- **Two precedents for arm-by-env-var instrumentation that costs nothing when off**: `MUTSU_VM_STATS`
  (histograms behind one relaxed atomic load, summary at exit) and `MUTSU_ALLOC_STATS` (compiled out
  entirely behind a cargo feature).

Two gaps, both small and both on the critical path:

- **`CompiledCode` has no file identity.** `op_lines` is line-only; the current file lives in the env
  (`?FILE`, `wk::file()`) and on `FunctionData`. A line profiler needs `(file, line)`, so the chunk must
  carry a compunit/file id (§5 Slice 0).
- **`cur_source_line` is not the executing line.** It is refreshed only where a line can be *observed*
  — 89 `sync_source_line` call sites, all at call/reentry/raise boundaries. Between those it is stale.
  It is a correct *call-site* line and a wrong *sample* line (§6b).

## 4. Decision

**D1 — Sampled time, instrumented counts.** Time attribution is statistical: a timer arms a tick, the
next poll takes one sample. Counts (per-line hits, per-routine entries, per-callsite calls) are exact,
taken at chokepoints that already exist. mutsu does **not** time every statement by default.

**D2 — A sample resolves `(chunk, ip)` through the static table, at report time.** The sample path
records the identity of the chunk and the instruction pointer; `op_lines`/`line_at` maps it to a line
when the report is built, not while the mutator is running. Never `cur_source_line` (§6b).

**D3 — A sample carries the whole Raku-level stack**, not just its top: the `RoutineFrame` stack,
top frame refined to `(chunk, ip)`. This is what buys self time, inclusive time and **caller
attribution** — NYTProf's most useful column, and the one thing a flat line table cannot give.

**D4 — Native time is attributed to the Raku line on top of the stack, and tagged with the
interpreter phase it was spent in.** A per-thread region tag (`call-resolve`, `method-dispatch`,
`regex`, `gc`, `parse`, `io`, `nqp`, `native-builtin:<name>`) is set at the few chokepoints that
already exist for stats, so a report line reads

> `JSON-Fast/lib/JSON/Fast.rakumod:412   38.1% self   (of which 71% call-resolve, 12% gc)`

This is the Scalene property from §2.3, and it is the feature that closes the loop with the Rust-level
tooling: the profile names the Raku line **and** the interpreter subsystem, so a callgrind session is
entered with a hypothesis instead of a hunch.

**D5 — Counts are exact; only counts are ever asserted in a test.** Per-line hit counts come from the
line-transition edge (`op_lines[ip] != last_line`) under the armed gate; per-routine entry counts from
the frame push. Both are deterministic and independent of load and optimization level — the same
property that makes `MUTSU_VM_STATS` usable in the debug build. **Tests assert counts and structure,
never a duration or a sample count**: a test that asserts "this line got ≥N samples" is a flaky test by
construction, and under this repo's definition of risk (flaky tests, reduced compatibility, band-aids)
shipping one would be a worse outcome than shipping no profiler.

**D6 — The profiled program is the program.** JIT and GC stay on under profiling; nothing is recompiled
in a "profiling mode" bytecode shape. This is a hard requirement, not a preference: a profiler that
changes JIT eligibility measures a program the user will never run. It is affordable precisely because
of D1 — sampling does not need per-op instrumentation, and the poll network already covers native code.
`--profile-jit=off` exists as an explicit A/B knob, and the report header records which it was.

**D7 — Output is a documented JSON document plus a built-in text summary.** JSON first because the
primary consumers are tooling and agents (ADR-0065's stance) and because the ecosystem-parity campaign
will want to diff profiles across runs. The text summary must be good enough to make HTML optional:
top-N self lines, top-N inclusive routines, per-caller breakdown for the top routines, region split.
HTML rendering is out of scope for the first implementation.

**D8 — Surface**: `--profile[=FILE]` (rakudo-compatible spelling; default `mutsu-prof.json`),
`--profile-kind=line|routine|both` (default `both`), `--profile-rate=<Hz>` (default 1000),
`--profile-report=<json|text|both>`, and `MUTSU_PROFILE=1` / `MUTSU_PROFILE_RATE` for env-armed runs
(matching how every other mutsu instrument is armed). Rakudo spellings that mean something different
in mutsu (`--profile-kind=heap`) are **not** claimed until they do that thing; an unknown kind is a CLI
option error per [ADR-0017](0017-cli-option-errors-follow-rakudo.md).

## 5. Mechanism, by slice

Each slice is independently landable and independently useful; the order is chosen so the first one
that lands already answers a question nobody can answer today.

### Slice 0 — file identity on the chunk

Add `source_file: Option<Symbol>` (or a compunit id into a side table of paths) to `CompiledCode`,
set by the compiler from the unit being compiled, propagated to closure chunks. `line_at(ip)` gains a
sibling `location_at(ip) -> Option<(Symbol, u32)>`. No runtime cost: a field read at report time.
Independent value: backtraces and error metadata get a chunk-local file without an env probe.

### Slice 1 — generalize the poll network

`gc::armed()` becomes `vm_poll::armed()` = `gc_armed | profiler_armed` (**one** cached bool, the union
computed once at arm time — not two loads OR'd per poll, which would cost a load per opcode on a path
that runs on every backedge), and `gc_safepoint(kind)` becomes the GC consumer of a
`vm_poll(kind, site)` entry point.

The JIT's `helpers::safepoint` shim needs the current ip, and this is the one place in the slice where
"free when disarmed" is not automatic: the backedge ip is a compile-time immediate, but emitting it
unconditionally puts an extra argument setup on every native backedge — i.e. in the hottest code mutsu
has — whether or not anyone is profiling. **The JIT must emit the ip-passing form only when the
profiler is armed.** That is sound because arming is a process-lifetime decision read at startup, while
JIT compilation happens later, at the hotness threshold: by the time a chunk is compiled the arming
state is already fixed, so the two shim shapes can be selected at codegen time and the disarmed build
pays exactly nothing.

The same rule governs everything the poll gains: the profiler's own work (including the tick check of
Slice 2) sits **inside** the armed branch, never before it.

Gate: §8 gates 1 and 1c.

### Slice 2 — the sampler

- A **timer thread** bumps one global `AtomicU64` epoch at the configured rate; each poll compares it
  against a thread-local `last_seen` and samples when they differ. There is deliberately **no registry
  of mutator threads** to maintain — mutsu has none to reuse (`src/gc/stw.rs` keeps only a per-thread
  `REGISTERED_MUTATOR` flag), and a registry would have to be kept in step with `clone_for_thread`
  (`src/runtime/runtime_thread.rs:198`) and the worker pool. The epoch reaches every thread that polls,
  which is every thread running Raku code. A thread blocked in a native call (sleep, IO, `await`, a
  GC `stw_aware_wait` park) does not poll and contributes nothing — correct, and a documented property
  of the report rather than a silent hole.
- The epoch load happens **inside** the armed branch (Slice 1), so a disarmed run does not pay an
  atomic load per opcode.
- A poll that observes a new epoch: read the clock once, walk the `RoutineFrame` stack plus the
  `(chunk, ip)` the **previous** poll stood at (§5.1 — the active ip is the start of the *next* region,
  not the one that ran), and append a **sample record** to a per-thread, pre-allocated ring buffer. The sample
  path performs **no allocation** and takes no lock — it writes fixed-size frames (chunk id, ip,
  routine id, region tag) into reserved space, which keeps the profiler out of its own measurement
  (an allocating sampler would pollute both the GC's candidate buffer and the alloc-stats counters).
- **The buffers are thread-local and allocated at arm time**, never fields on `Interpreter`.
  `Interpreter` is cloned per thread (`clone_for_thread`), so a buffer living on it would be memory
  every thread pays in every run, profiled or not — which is precisely the disarmed cost §8 gate 1b
  exists to keep at zero.
- Weighting: each sample carries the elapsed time since the previous sample **on that thread**, so a
  long region that delays the poll contributes its real duration rather than a fixed tick's worth.
- Aggregation happens at run end (or when a buffer fills) off the hot path: samples fold into a
  `(file, line) → {self, inclusive}` table and a caller-keyed routine table.

#### 5.1 Three corrections the implementation forced (2026-09-18, [#8702](https://github.com/tokuhirom/mutsu/issues/8702))

**A sample credits the line of the *previous* poll, not the line the poll is standing on.** A
poll-based sampler never sees where the tick fired; it finds out at the next poll, by which point the
region that was running has finished and the thread is at the start of the next one. The probability
of noticing at a given poll site is proportional to the time spent in the region *preceding* it, so
crediting the site itself shifts a profile one region late — systematically, not on average. On Raku
code, where a line is a handful of opcodes, "one region late" means a hot line's cost is reported
against the line below it, which is the same class of plausible-looking wrong answer §6b rejects
`cur_source_line` for. In the interpreter the polls are per opcode and in JIT-compiled code the
emitted hooks are per line transition, so in both the previous poll bounds exactly the region that
delayed this one. It is also what makes the elapsed weighting do its §7 job: a long native region has
its whole duration charged to the line that entered it rather than to whatever ran next.

**The sampler has to ride the JIT's per-line hook, not only the poll.** Sampling from `poll_code`
alone gives a native body one sample per entry, carrying the range's first ip — because no Raku loop
form places a backward jump inside a compiled range (§8.2). A hot loop then reports all of its time
against its first line and the other body lines are *absent*, not merely under-weighted. This is the
time half of exactly what [#8713](https://github.com/tokuhirom/mutsu/issues/8713) fixed for the
counts, and it has the same fix: the sampler rides `helpers::profile_line`, which the JIT already
emits at every line transition while armed.

**Per-thread tables must be registered, not folded on `Drop` alone.** A worker-pool thread is still
alive when the process reports, so a fold-on-drop loses everything it collected. This was latent in
Slice 3's counters too and surfaced the moment a `start` block was profiled: a routine called once on
the mainline and once on a worker reported `entries=1`. Both halves now register a handle the report
can drain from another thread; the counters' hot path is unchanged, because the "still on the same
line" poll reads one `Cell` and takes no lock.

One thing the slice deliberately did **not** do, **since settled at the source**. A chunk's
`source_file` was the canonicalized path while a `RoutineFrame`'s was `$?FILE` as the user spelled
it, so a `line` row and a `callsite` row could name one file two ways and could not be joined — the
mainline case of the `EVAL` hazard this ADR's §7 risk table already ruled on once. The report
reconciled them at report time (`src/profile/paths.rs`).
[#8719](https://github.com/tokuhirom/mutsu/issues/8719) removed the divergence instead: the mainline
now publishes the **as-invoked** path to both the unit stamp and the env, the way a `use`d module and
an `EVAL` already did, and `$?FILE` keeps its own absolutified spelling — which is rakudo's own
split, and derivable from the identity without a syscall. `src/profile/paths.rs` is gone with it, and
the report's tables no longer need a second fold. `t/tooling/profiler-unit-file-identity.t` pins the result.

### Slice 3 — exact counts

In the dispatch loops, under the armed gate only, compare `code.op_lines[ip]` with the last recorded
line and bump a per-`(chunk,line)` counter on a change. This is one indexed load and a compare on a
path that is already gated; when disarmed it is not reached at all. Per-routine entry counts and
per-callsite call counts come from the frame push, which already runs on every dispatch path.

Result: the report has NYTProf's two columns — *how many times* (exact) and *how long* (sampled).

### Slice 4 — region tags

A per-thread `region: u8` set/restored at the chokepoints that already exist for `MUTSU_VM_STATS`
accounting (`resolve_function_with_types`, the method-dispatch entry, the regex walk entry, GC
safepoint work, the parser entry, native builtin dispatch). Set under the armed gate; a plain
thread-local store, no atomics. This is what produces D4's "of which 71% call-resolve".

### Slice 5 — report, schema, docs, tests

- JSON schema documented in `docs/profiler.md`: header (mutsu version, argv, JIT/GC state, rate,
  wall time, sample count, **an explicit `"time_is_sampled": true`**), per-file per-line rows
  (`hits` exact, `self_us`/`incl_us` sampled, region split), routine rows with caller breakdown.
- Text renderer for the same document.
- Tests (`t/` — category per `docs/t-directory-layout.md`): run a fixture script with a known shape,
  assert **counts and structure** — the hot line appears, its `hits` is exactly the loop trip count,
  the caller table links the right routines, the JSON parses and carries the header. No timing
  assertion anywhere.

### Slice 6 — optional, only if a case appears

Compile/parse-phase profiling (rakudo's `--profile-compile`; mutsu's parse cost is real —
[#8095](https://github.com/tokuhirom/mutsu/issues/8095)); a MoarVM-shaped export of the routine half so
moarperf can open it; HTML; a Raku-level API (`use Telemetry`-alike) over the same data.

### What the report has to say to be worth building

For #8289, a five-minute triage instead of an afternoon would have read approximately (illustrative
shape, not measured data):

```
mutsu-prof: 6.2s wall, 6,143 samples @1000Hz, jit=on gc=on
TOP SELF LINES
  1  modules/JSON-Fast/lib/JSON/Fast.rakumod:412   41.3%  hits 1,904,000   [nqp 78% | call-resolve 14%]
  2  modules/JSON-Fast/lib/JSON/Fast.rakumod:389   18.7%  hits   238,000   [call-resolve 61% | gc 9%]
TOP ROUTINES (inclusive)
  str-escape          88.1%   entries 1,204   <- to-json:117 (99%)
```

Line 412's `hits` being an order of magnitude larger than the loop it sits in is the quadratic scan;
the `[nqp 78%]` tag names the subsystem to open callgrind on. Both facts are on screen without a
hypothesis having been formed first — that is the whole value proposition.

## 6. Rejected alternatives

**(a) Reintroduce a per-statement opcode (a profiling-mode `SetSourceLine`).** The data is already in
`op_lines`, so this buys nothing but a second bytecode shape — and a different shape means different
JIT eligibility and different dispatch counts, i.e. profiling a program the user does not run
(violates D6). It was removed for a measured +7.8% on fib; re-adding it even conditionally re-opens a
settled question.

**(b) Attribute samples from `cur_source_line`.** Rejected, and worth recording *why* because it is
the obvious shortcut: `cur_source_line` is refreshed only at the 89 `sync_source_line` observation
points, so between calls it holds the line of the last call site. A sampler reading it would produce
a plausible-looking profile that systematically credits time to call sites instead of to the code
doing the work. For a measurement tool, "wrong but plausible" is the worst available failure mode.

**(c) Deterministic per-statement timing (a true NYTProf) as the default.** Two clock reads per
statement, on a VM whose *call path* is the known bottleneck ([#7573](https://github.com/tokuhirom/mutsu/issues/7573)),
distorts exactly the programs most in need of profiling — and the distortion is not uniform, so
comparing two profiled runs is unsound. Deferred, not banned: if a case appears for exact statement
timing on a small region, it can be a later `--profile-kind=exact` over the same infrastructure.

**(d) `SIGPROF` signal-based sampling.** The classic way to escape safepoint bias, and rejected for
the first implementation: an async signal handler inside this VM would have to be safe against the GC,
the allocator and every borrow state the interrupted code holds, and the poll network already covers
JIT-compiled code (which is the usual reason to reach for signals). Revisit only if measurement shows
bias from long native regions the poll network cannot interrupt — the cheaper fix there is an explicit
poll inside those few loops (regex scan, sort, large-list map), which mutsu owns.

**(e) Out-of-process sampling (py-spy / rbspy style).** Genuinely valuable for production (attach to a
running process, no cooperation needed) and far too expensive now: it needs a versioned, stable
in-memory layout for the frame stack that mutsu cannot promise while ADR-0077/0078/0092 are still
moving the stacks around. Revisit when mutsu has long-running production users.

**(f) Force `MUTSU_JIT=off` while profiling.** Simplifies the sampler and measures a different
program (see D6). Kept as an explicit flag for A/B work, never as the default.

**(g) Adopt MoarVM's profile JSON as mutsu's native format.** Attractive for the free moarperf UI, and
wrong as the *native* format: the schema has nowhere to put per-line data (its `line` is a routine's
declaration line, §2.2) and carries MoarVM-specific fields (`spesh_time`, `mono`/`poly`/`mega`,
type-keyed allocations) mutsu would have to fabricate or leave empty. An **exporter** for the
routine-level half is a legitimate Slice 6 nice-to-have; the line data stays in mutsu's own document.

**(h) Ship it as a Raku-level module instead of VM support.** A `Telemetry`-alike, or wrapping routines
from Raku, cannot see the line currently executing, cannot see native time, and would pay Raku-level
call costs per event. A module surface over the VM's data is a fine later addition; it is not the
mechanism.

## 7. Consequences

**Gained**

- Perf work stops guessing. ADR-0096's "optimize the real module, never substitute for it" becomes
  executable by someone who has not already memorized the module; ADR-0085's parity campaign gets a
  triage tool for the slow-but-passing distributions it keeps finding.
- A user-facing capability rakudo does not have (§2.2), on a compatibility-driven project that usually
  measures itself by how much of rakudo it has caught up with.
- The Rust-level and Raku-level views finally join up through D4's region tags, instead of being two
  separate investigations connected by intuition.

**Paid**

- The poll network gains a second consumer. Its off-cost must stay at one cached load, which is a
  standing gate (§8), not a one-time check.
- **A permanent cost for everyone, in exchange for a tool most runs never use.** The profiler is
  compiled into the shipped binary, and Slice 0's chunk identity is memory every program pays. The
  design keeps that to one word per chunk and nothing else (§8 gates 1/1b/1c hold it there), which is
  the trade this ADR is making explicitly rather than discovering later: a profiler nobody can reach
  without a rebuild is not a profiler users have.
- A new output format to keep stable enough for tooling to consume, and a `docs/profiler.md` to keep
  honest.
- A discipline risk worth naming: **sampled numbers must never be quoted as authoritative in
  documents.** PERFORMANCE.md / PLAN.md / `news/` numbers continue to come from the bench CI
  (`bench-history.tsv`), per the existing rule. The report header carries `"time_is_sampled": true`
  so a pasted profile cannot be mistaken for a measurement.

**Residual risks and what answers each**

| risk | answer |
|---|---|
| safepoint bias (a long native region delays the poll) | elapsed-time weighting (Slice 2) plus explicit polls in the few long loops mutsu owns; §6d if measurement still shows bias |
| threaded programs under-sampled | per-thread tick + per-thread buffers from the start, not a retrofit |
| `EVAL`/precomp chunks with no file | Slice 0 gives every chunk an identity, including `EVAL` units. **Shipped as mutsu's existing `EVAL_<N>` unit name, not the `EVAL#<n>` this table first proposed** — `builtin_eval` already synthesizes and scopes that name, and it is what `Code.file` and a backtrace report, so a second identity would have made `location_at` disagree with the frame beside it |
| profiler perturbs GC/alloc statistics | no allocation on the sample path; buffers pre-reserved at arm time |
| a flaky timing test sneaks in | D5 — the test suite may assert counts and structure only |

## 8. Gates

The first three are all forms of one question — **what does a user who never profiles pay?** — because
the profiler ships compiled in (it is not behind a cargo feature like `alloc-stats`: `--profile` has to
work on the binary people actually install). The answer must be "nothing measurable", in CPU *and* in
memory, and it is not enough to assert it: each of these is measured per slice.

1. **Disarmed CPU**: ≤0.5% instructions on `bench-fib`, `bench-tak`, `bench-mandelbrot`
   (`perf stat -e instructions:u`, ADR-0006 protocol) with the profiler compiled in. Fails → the poll
   generalization is wrong, not the profiler.
1b. **Disarmed memory**: peak RSS within noise on a battery-heavy run — `bench-json-fast` and a
   META6-shaped `mzef` metadata read, which load enough modules to multiply any per-chunk or
   per-thread cost. The only permanent allocation this ADR licenses is Slice 0's per-chunk
   `Option<Symbol>`: **8 bytes against a `CompiledCode` measured at 2,168 bytes** of field headers
   alone (2026-09-18, debug build, `size_of` — 51 `Vec`s, 10 `FxHashSet`s, a `FxHashMap`, 4
   `OnceLock`s, 16 `bool`s, before any element storage), i.e. **0.37%** of a chunk's fixed cost and
   less of its real one. Everything else — ring buffers, counter tables, the timer thread — is
   allocated at arm time and must not exist in a disarmed run. A regression here means state leaked
   onto `Interpreter` (cloned per thread) or into `CompiledCode` (one per chunk, thousands per run).
1c. **Disarmed JIT backedge**: `bench-mandelbrot` and `bench-fib` with JIT **on**, isolating the Slice 1
   shim change, within the same 0.5%. This is the gate that catches the one non-automatic cost in the
   design — an ip argument emitted on every native backedge regardless of arming (§5 Slice 1). If it
   fails, the codegen is not specializing on arming state.
2. **Armed cost**: ≤1.3x wall on `bench-json-fast` at 1000 Hz for `--profile-kind=line`. (For scale:
   NYTProf is commonly 2-6x; sampling should be far cheaper, and if it is not, the sample path is
   allocating or locking.)
3. **Attribution correctness**: on a fixture whose hot line is known by construction, the top self line
   is that line, and its exact `hits` equals the trip count — asserted in `t/`.
4. **JIT parity**: the same fixture profiled with `--profile-jit=off` and on produces the same top line
   and the same `hits` (only the times differ). This is the check that the JIT backedge poll works.
   For the *sampled* half there is no "same number" to compare — a native body and an interpreted one
   take neither the same polls nor the same time per line — so what is asserted there is that the two
   runs name the same **set** of lines, routines and caller edges. That is what gate 4 is actually
   about: whether a profile silently loses resolution the moment a loop gets hot, which is exactly
   when a profiler is opened.

### 8.1 Measured results

**Gates 1 and 1c, after Slice 1 (2026-09-18): pass.** Measured with **callgrind** rather than
`perf stat` — the container this ran in has no `perf`, and callgrind's `Ir` count is deterministic and
load-independent, so it answers the same question with less noise than the gate asks for. Baseline is
the release binary of `d559d288` (main immediately before Slice 1 landed); `MUTSU_GC=off` throughout.
`nqp-backedge` is the fixture of §8.2 scaled to 4,000,000 native backedges — the workload the Slice 1
shim specialization exists for, and the one that would show its absence.

| workload | JIT | baseline Ir | after Slice 1 | delta |
|---|---|---:|---:|---:|
| `bench-fib` | off | 2,703,889,033 | 2,705,162,835 | +0.047% |
| `bench-tak` | off | 2,603,936,806 | 2,604,720,102 | +0.030% |
| `bench-mandelbrot` | off | 1,106,631,576 | 1,106,551,354 | -0.007% |
| `nqp-backedge` | off | 14,642,371,483 | 14,642,375,080 | +0.000% |
| `bench-fib` | on | 1,498,712,992 | 1,499,983,919 | +0.085% |
| `bench-tak` | on | 1,743,997,234 | 1,744,776,039 | +0.045% |
| `bench-mandelbrot` | on | 663,204,510 | 663,419,849 | +0.032% |
| `nqp-backedge` | on | 11,016,642,286 | 11,024,251,001 | +0.069% |

Everything is inside 0.5%, gate 1c's JIT-on rows included. The bench CI remains the source of truth
for *wall-clock* numbers in documents (`CLAUDE.md`); this table is an instruction count, which is what
gates 1 and 1c actually specify.

**Gates 1, 1b, 1c and 2, after Slice 2 (2026-09-18).** Gates 1 and 1c re-measured the same way as
the Slice 1 table above — callgrind `Ir`, which is deterministic and load-independent, against the
release binary of `528a8d02` (main immediately before Slice 2), `MUTSU_GC=off` throughout:

| workload | JIT | baseline Ir | after Slice 2 | delta |
|---|---|---:|---:|---:|
| `bench-fib` | off | 2,714,066,803 | 2,712,869,354 | -0.044% |
| `bench-tak` | off | 2,610,268,252 | 2,609,518,483 | -0.029% |
| `bench-mandelbrot` | off | 1,106,672,058 | 1,106,282,683 | -0.035% |
| `nqp-backedge` | off | 8,502,241,991 | 8,502,242,264 | +0.000% |
| `bench-fib` | on | 1,508,885,410 | 1,507,687,521 | -0.079% |
| `bench-tak` | on | 1,750,371,783 | 1,749,622,357 | -0.043% |
| `bench-mandelbrot` | on | 663,795,911 | 663,411,679 | -0.058% |
| `nqp-backedge` | on | 8,506,274,145 | 8,506,274,641 | +0.000% |

Everything is inside 0.5% and the sign is negative, i.e. the differences are binary-layout noise
rather than a cost. That is the expected result and worth saying why: Slice 2 adds **no codegen** —
the location-carrying shim shapes and the decision to emit them were Slice 1's, and the sampler's
work is behind the same `t.profiler` branch the exact counters already sit behind.

One caveat on the last two rows. The `nqp-backedge` fixture of §8.2 was not kept in the tree, and the
reconstruction used here (an `nqp::while` inside a routine called 500 times, so the routine body
reaches the hotness threshold — a mainline chunk is entered once and never does) gets one range
compiled but shows no instruction-count difference between JIT on and off, so it is weaker evidence
for the native backedge than the original row was. The three real benchmarks' JIT-on rows are the
substantive JIT-side evidence here, and gate 1c's specific hazard — an ip argument emitted
unconditionally — is Slice 1's, unchanged.

**Gate 1b (disarmed memory): pass.** Peak RSS (`VmHWM`, worst of five runs) on `bench-json-fast`,
which loads enough of the batteries to multiply any per-chunk or per-thread cost: 40,804 kB baseline
against 39,224 kB after Slice 2 — within the sampling noise of the measurement, and in the direction
that is not a regression. This is the gate that would catch a buffer landing on `Interpreter`; the
sampler's buffers and its tick thread are created by the poll network's arm-time trigger computation
and by nothing else, so a disarmed run has neither.

**Gate 2 (armed cost): pass, at 1.10x against a budget of 1.30x.** `bench-json-fast`, release, median
of seven runs: 423 ms disarmed against 464 ms at the 1000 Hz default. At 100 Hz it is 422 ms, i.e.
free. The armed number covers the exact counters *and* the sampler together, because one environment
gate arms both. For scale, §2.1's note stands: NYTProf is commonly 2-6x.

These are instruction counts and a local wall-clock ratio, which is what these gates specify; the
bench CI remains the source of truth for wall-clock numbers quoted in documents (`CLAUDE.md`).

**Gates 3 and 4, after Slice 3 (2026-09-18): pass, and are now asserted.** On a `while` loop whose
body runs exactly 5,000 times, both body lines report `hits=5000` and the condition line `hits=5001`
(it is evaluated once more, the time it is false), with every line outside the loop at 1 — the trip
count, exactly, which is gate 3. Running the same fixture with `MUTSU_JIT=off` and with
`MUTSU_JIT=on MUTSU_JIT_THRESHOLD=1` produces **identical** line, routine and callsite tables, which
is gate 4. `tests/profile_counts.rs` holds both, and asserts that the JIT actually entered before
claiming parity from it. They could not be asserted before that: `flush_at_exit` folded its snapshot
into a static nothing read, so the counters had no consumer outside the crate
([#8713](https://github.com/tokuhirom/mutsu/issues/8713)).

**Gate 4's sampled half, after Slice 2 (2026-09-18): pass.** On the same shape of fixture, a run with
`MUTSU_JIT=off` and one with `MUTSU_JIT=on MUTSU_JIT_THRESHOLD=1` name the identical set of sampled
lines, routines and caller edges (`tests/profile_samples.rs`, which also asserts the JIT entered).
Nothing about the *times* is compared, and nothing could be: a native body and an interpreted one
take neither the same polls nor the same time per line.

### 8.2 What gate 1c's fixture had to be, and why

Measuring gate 1c turned up a property of the bytecode worth recording: **no Raku loop form places a
backward jump inside a JIT-compiled range.** `while`, `for`, `loop`, C-style `loop` and `repeat` all
compile to compound opcodes whose body is a *separate* compiled range, entered once per iteration, so
that range holds no backedge of its own — 0 native backedge polls on every one of those shapes, and on
`bench-fib` / `bench-tak` / `bench-mandelbrot`. The one shape that reaches the emitted backedge hook is
`nqp::while` (`src/compiler/nqp_forms.rs`), which emits a plain backward `Jump` into the enclosing
chunk, and that is what the `nqp-backedge` row of §8.1 exercises. Anything else would have measured the
Slice 1 shim specialization on a path that never runs.

This says nothing about line coverage, which does **not** come from the backedge hook: Slice 3 emits
`helpers::profile_line` at chunk entry, at every jump target, and at every sequential line transition
inside the compiled body, all gated on the profiler being armed (`src/vm/vm_jit_compile.rs`). That is
why gate 4 above holds exactly rather than approximately. (An earlier revision of this section claimed
the opposite — that a native body recorded only at its entry, leaving gate 4 unachievable. It was
wrong: it inferred the emission sites from a grep for the `vm_poll` function name rather than the
helper's.)

## 9. Implementation status

**Slices 0-3 are shipped** ([#8699](https://github.com/tokuhirom/mutsu/issues/8699),
[#8701](https://github.com/tokuhirom/mutsu/issues/8701),
[#8702](https://github.com/tokuhirom/mutsu/issues/8702),
[#8703](https://github.com/tokuhirom/mutsu/issues/8703);
`news/2026-09/a-compiled-chunk-knows-which-file-its-lines-belong-to.md`,
`news/2026-09/profiler-safepoint-poll-network.md`,
`news/2026-09/profiler-exact-line-counts.md` and
`news/2026-09/profiler-sampler.md`): `CompiledCode` carries
`source_file: Option<Symbol>` and `location_at(ip) -> Option<(Symbol, u32)>`. One deviation from the
sketch above: rather than threading the unit path through `Compiler::compile` and its ~40 chunk-
compiler construction sites, the unit's identity is published for the duration of a compile through a
thread-local (`src/unit_source_file.rs`) that `CompiledCode::new()` reads — the shape the parser
already uses for `$?FILE` — so every chunk a compile produces is stamped, including the nested ones no
walker enumerates. `--dump-bytecode` is its first consumer.

Slice 1 generalizes the safepoint network into `vm_poll`, with GC as its first consumer and a
profiler gate/site ABI ready for Slice 2; the JIT supplies the bytecode ip on native backedges only
when the profiler is armed. `tests/jit_diff.rs` pins that arming the profiler consumer preserves JIT
execution. Slice 3 adds per-thread exact line-transition counters, routine entries, and callsite
calls, folding them off the hot path; native code emits line-entry hooks while armed so JIT counts
are not silently partial. Being *transition* counters has a consequence the design did not call out
and the first profiles made visible: a loop whose body occupies a single line never transitions, so
its `hits` is one per loop entry rather than one per trip, for every loop form. Whether a backedge
landing on the same line should record a hit — which would touch the counter's cheapest branch and
re-open the §8 gates — is [#8737](https://github.com/tokuhirom/mutsu/issues/8737); `docs/profiler.md`
documents the behaviour as it is.

Slice 2 adds the sampler: a detached tick thread at `MUTSU_PROFILE_RATE` Hz (1000 by default), a
thread-local `last_seen` compared inside the armed branch, and per-thread buffers reserved at arm
time that fold into `(file, line)` and routine tables when they fill or at exit. Three deviations from
the sketch, all of them things only the implementation showed, are recorded in §5.1: a sample credits
the *previous* poll's line, the sampler rides the JIT's per-line hook rather than only `poll_code`,
and per-thread tables are registered rather than folded on `Drop`. Time spent in a GC collect, a
stop-the-world park or a blocking `sleep`/join/read is discounted from the weighting
(`profile::exclude_non_raku`). `MUTSU_PROFILE_TICK=every-poll` replaces the timer with "every poll is
a tick", which is what lets `tests/profile_samples.rs` assert the sampler's *structure* without
asserting a duration (D5). The one thing the slice worked around rather than fixed — one source
file carrying both a canonicalized and a spelled name — was settled at the source by
[#8719](https://github.com/tokuhirom/mutsu/issues/8719) (see Slice 3 above).

**Slice 4 is shipped** ([#8704](https://github.com/tokuhirom/mutsu/issues/8704);
`news/2026-09/profiler-subsystem-regions.md`): a sample carries a `Region` tag naming the interpreter
subsystem it caught running — `interp`, `call-resolve`, `method-dispatch`, `native-builtin`, `nqp`,
`regex`, `parse`, `gc` — and the report prints a whole-run split, a per-`(file, line)` split, and a
`top_region` header field. One deviation from the sketch, and it is the whole mechanism: **a
thread-local "current region" read at the sample point cannot work**, because mutsu's sampler is
poll-based and the regions worth naming are exactly the long native stretches that do not poll, so by
the time a poll notices the tick the region has returned and the tag reads `interp` every time.
Instead the region *claims* the tick on its way out — one relaxed load and a compare, no clock read
and no hash on the region path — and the first claim wins, which is correct rather than arbitrary: a
claim only happens when the tick was already pending at that exit, so a region that runs afterwards
demonstrably was not running when the tick fired. `Interp` is an answer ("bytecode was running"), not
a residue bucket, so there is no `unknown` tag to explain away. GC and stop-the-world time, which §5's
Slice 2 already subtracts from the sampled weight, is *measured* by the two clock reads that
subtraction already costs and reported in a separate `excluded-region` table — named rather than a
silent hole. `tests/profile_regions.rs` asserts the split the only way D5 permits: under
`MUTSU_PROFILE_TICK=every-poll` the region rows are a function of the executed bytecode, so which
subsystem each sample was charged to is reproducible while its nanoseconds are never asserted.

**Slice 5 is shipped** ([#8705](https://github.com/tokuhirom/mutsu/issues/8705);
`news/2026-09/profiler-output.md`, reference documentation in
[docs/profiler.md](../profiler.md)): the `--profile[=FILE]` /
`--profile-kind` / `--profile-rate` / `--profile-report` / `--profile-jit` surface with
`MUTSU_PROFILE*` twins (`src/profile/options.rs`), one document built once from both
snapshots (`src/profile/document.rs`) and rendered as pretty-printed JSON and as the text
summary D7 asks for (`src/profile/text.rs`). The scaffolding report §9 described above is
gone; `tests/profile_counts.rs`, `tests/profile_samples.rs` and `tests/profile_regions.rs` now
read the **published** document through one shared reader (`tests/profile_doc/`), so the same
assertions that pin the counters also pin the schema, and `t/tooling/profiler-report.t` plus
`t/tooling/profiler-cli-options.t` pin gate 3 and the CLI from Raku. Three things the
implementation settled that the sketch left open:

- **Absent is not zero.** Every measured field is omitted when this run did not measure it, so
  `hits` on a line only the sampler reached, `self_us` on a line only the counters reached, and
  the whole half `--profile-kind` did not ask for are all *missing* rather than `0`. §4's D7
  said this for the halves; the line and routine rows need it for the same reason, and a zero
  that means "not measured" is the one error a consumer cannot detect.
- **A flag and an environment variable disagree on purpose.** `--profile-kind=heap` is rakudo's
  `Unknown profiler specified` on stderr with exit 1 (measured against rakudo 2026.07, which is
  *not* the ADR-0017 option-list shape — that stays exit 0 for an unknown option like
  `--profile-frobnicate`), while `MUTSU_PROFILE_KIND=heap` warns and falls back: a flag was typed
  by a person just now, a variable is usually inherited from somewhere else.
- **A call site's file comes from the enclosing body, not from `$?FILE`.** The first profile of a
  multi-file program (`benchmarks/bench-json-fast.raku`) filed every callsite inside JSON::Fast
  under the *script's* path with the *module's* line numbers -- a caller row reading
  `bench-json-fast.raku:275` for a file 84 lines long -- because a frame recorded its call site's
  file as the dynamically-scoped `?FILE`, which still names the mainline while a `use`d module's
  routine runs. The profiler used to paper over this with its own reconciliation pass
  (`ProfileAggregate::resolve_caller_files`, an outward walk over the sampled stack). Settled at
  the source by [#8743](https://github.com/tokuhirom/mutsu/issues/8743): every
  `push_*_routine_with_location` now resolves the call site the same way, via
  `Interpreter::executing_source_file_sym` (an outward walk over the *live* `routine_stack`, done
  once at push time), so `RoutineFrame.file` -- and therefore a backtrace, `CallFrame.file`, and
  the profiler's own counters -- all read the correct file with no reconciliation pass left to run.
  Its sibling, the canonicalized-versus-spelled split that `src/profile/paths.rs` used to
  reconcile, was settled by #8719 and that file is gone the same way.

Slice 6 is explicitly optional and unstarted, tracked as
  [#8738](https://github.com/tokuhirom/mutsu/issues/8738). Each slice landed as its own PR with its
  own gate (§8), and Slices 0-5 were each independently useful, as claimed.

Per-line allocation attribution is also shipped ([#8740](https://github.com/tokuhirom/mutsu/issues/8740);
`news/2026-09/profiler-per-line-allocation-attribution.md`). It is deliberately a separate,
measurement-only mode: an `alloc-stats` build with `MUTSU_ALLOC_STATS=1` keeps exact allocation
count and requested-byte totals in a thread-local line accumulator and publishes them as
`files[].lines[].allocations`. The sampler is not armed in that mode, because the counting
allocator changes allocation timing; `header.allocation_stats` marks the document and `sampling`
is absent. The default build and all normal profiles pay no allocation-attribution cost. The
regression test runs the feature build and checks the published document, while the normal profile
tests continue to assert the sampled-time and exact-count halves independently.

## 10. Open questions

- **Rate and clock.** The 1000 Hz default, borrowed from stackprof/py-spy, now has a number against
  it: 1.10x on `bench-json-fast` (§8.1), where 100 Hz is free. It stays. The clock is `Instant`
  (wall), which is right for an interpreter whose costs include I/O and GC; whether a
  `CLOCK_THREAD_CPUTIME_ID` mode is worth having for threaded runs is still open, and is the thing
  that would make `sampled_ns` comparable to `wall_ns` on a multi-threaded profile instead of
  summing past it. Tracked as
  [#8739](https://github.com/tokuhirom/mutsu/issues/8739).
- **Allocation attribution per line.** Resolved by the opt-in exact mode above: the counting
  allocator attributes count and requested bytes to the VM/JIT source line currently executing.
  It is feature-gated and uses no sampled time in the same document, so the numbers answer an
  allocation question without pretending that an instrumented build is a timing run.
- **Inclusive time across `EVAL` and thread boundaries.** Resolved by
  [#8741](https://github.com/tokuhirom/mutsu/issues/8741): `incl_us` remains the sampled time of
  locations present on the sample's own stack. There is no synthetic fold from a `start` worker into
  the spawning thread and no separate `spawned_incl_us` field; an ordinary call-site line that is
  already represented by a worker's block frame may receive its normal stack credit, but the report
  does not invent a parent-thread edge. `EVAL` follows the same rule. This keeps the line table
  faithful to the stack that was sampled and leaves the known multi-thread `sampled_us` versus
  `wall_us` relationship explicit rather than hiding it in a mixed inclusive column.
