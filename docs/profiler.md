# The Raku-level profiler

mutsu can tell you which **Raku** line and which **Raku** routine a run spends its
time in, and — because the answer for an interpreter is usually "in mutsu, on that
line's behalf" — which part of mutsu that was. It is armed with one flag:

```console
$ mutsu --profile myscript.raku
```

which runs the program normally, prints a summary to stderr, and writes a JSON
document to `mutsu-prof.json`.

The design, and the reasoning behind every choice below, is
[ADR-0106](adr/0106-raku-level-profiler.md). This page is the reference: the
surface, the document's schema, and the things the numbers do and do not mean.

## The one thing to know first

**Counts are exact. Times are sampled.**

- `hits`, `entries` and `calls` are counted at chokepoints the VM already runs
  through. They are deterministic: the same program run twice produces the same
  numbers, in a debug build as in a release one.
- every `*_us` field is **statistical**. A timer fires at `--profile-rate` Hz and
  the next VM poll records the Raku stack it is standing on. Two runs of the same
  program will not agree, and a short run may not sample a line at all.

That is why the document says `"time_is_sampled": true` in its header and the text
report says so on its second line. Numbers that go into PERFORMANCE.md, PLAN.md
or `news/` come from the bench CI (`bench-history.tsv`), never from a profile —
a profile tells you *where* to look, not *how fast* something is.

## The surface

| Flag | Default | What it does |
| --- | --- | --- |
| `--profile[=FILE]` | `mutsu-prof.json` | Profile the run and write the JSON document to `FILE`. |
| `--profile-kind=line\|routine\|both` | `both` | Which half of the profile to report. |
| `--profile-rate=<Hz>` | `1000` | Sampling rate, 1..1000000. |
| `--profile-report=json\|text\|both` | `both` | Which renderings to emit. `text` writes no file. |
| `--profile-jit=on\|off` | (unchanged) | Force the JIT for this run, for an explicit A/B. |

Every flag has an environment twin, which is how to profile a run mutsu did not
spawn itself — a `prove` harness, a module's own test, an embedder:

| Variable | Equivalent to |
| --- | --- |
| `MUTSU_PROFILE=1` | `--profile`, except that it writes **no file** unless `MUTSU_PROFILE_OUT` names one |
| `MUTSU_PROFILE_OUT=FILE` | `--profile=FILE` |
| `MUTSU_PROFILE_KIND` / `_REPORT` / `_RATE` | the matching flag (a flag wins when both are given) |
| `MUTSU_PROFILE_TICK=timer\|every-poll` | the tick source (see below) |

A flag and a variable differ in how they treat a value mutsu does not implement,
deliberately: a **flag** is something a person just typed, so a bad value is an
error (stderr, exit 1 — `--profile-kind=heap` is rakudo's `Unknown profiler
specified`, because mutsu does not do heap profiling and declines to claim the
spelling). A **variable** is often inherited from somewhere else, so a bad value
warns and falls back to the default rather than failing somebody's test run.

An unknown *option* — `--profile-frobnicate` — is an ordinary option-list error:
the message and the usage on stderr, exit **0**, per
[ADR-0017](adr/0017-cli-option-errors-follow-rakudo.md).

One deliberate difference from rakudo: a flag's value is checked even when the run
turns out not to profile, where rakudo runs the program and ignores a
`--profile-kind` given without `--profile`. A value mutsu does not implement was
still typed by somebody, and telling them beats silently doing something else.

### `MUTSU_PROFILE_TICK=every-poll`

Replaces the timer with "every poll is a tick". Far slower than the timer, and
not a way to profile a real program — but the samples a run takes become a
function of the bytecode it executes rather than of the clock, which is what lets
the profiler's own tests assert its *structure* without asserting a duration.

## Reading the text report

```
mutsu-prof: 6.20s wall, 6,143 samples @1000Hz (timer) on 1 thread, jit=on gc=on, kind=both
            hits are exact; every time below is SAMPLED -- never quote it as a measurement
TOP SELF LINES
   1  modules/JSON-Fast/lib/JSON/Fast.pm6:412   41.3% self  hits 1,904,000  [nqp 78% | call-resolve 14%]
   2  modules/JSON-Fast/lib/JSON/Fast.pm6:389   18.7% self  hits 238,000    [call-resolve 61% | gc 9%]
TOP ROUTINES (inclusive)
   1  JSON::Fast::str-escape   88.1% incl  entries 1,204
        <- modules/JSON-Fast/lib/JSON/Fast.pm6:117  1,190 calls (99% of the calls seen)
REGIONS (sampled self time)
  nqp 61.2% interp 30.1% gc 8.7%
EXCLUDED (measured, not sampled; not in the tables above)
  gc 1.2ms
```

Four things are on screen at once, and the combination is the point:

- **the line**, ranked by sampled self time;
- **its exact hit count** — a count an order of magnitude larger than the loop it
  sits in is a quadratic scan, and that is visible from the *width* of the number
  before it is read;
- **the subsystem split** in brackets: which part of mutsu was running on that
  line's behalf. A line that is 70% `call-resolve` is a mutsu bug report, not
  slow user code;
- **who called the routine**, with exact call counts.

A `-` in a percentage column means *not sampled* — never `0.0%`, which would be a
measurement that says "no time", a different claim.

`EXCLUDED` is time deliberately kept **out** of the tables above: a GC collect, a
stop-the-world park. It is measured rather than sampled, and it is named instead
of being left as a gap between the wall clock and the sampled total.

## The JSON document

Pretty-printed, one object, stable field order, deterministic row order: two runs
of the same program differ only in their timing fields, so profiles can be
diffed. Fields are **absent when this run did not measure them** — never zero,
because a zero that means "not measured" is a lie a tool reads as data.

```json
{
  "mutsu_prof_version": 1,
  "header": {
    "mutsu_version": "0.23.0",
    "argv": ["mutsu", "--profile", "bench.raku"],
    "kind": "both",
    "report": "both",
    "jit": "on",
    "gc": "on",
    "time_is_sampled": true,
    "blocked_threads_absent": true,
    "sampling": {
      "rate_hz": 1000,
      "tick": "timer",
      "wall_us": 6203417.0,
      "samples": 6143,
      "sampled_us": 6102884.0,
      "truncated_samples": 0,
      "threads": 1,
      "top_region": "nqp"
    }
  },
  "files": [
    {
      "path": "/abs/path/bench.raku",
      "lines": [
        {
          "line": 412,
          "hits": 1904000,
          "self_us": 2520011.0,
          "incl_us": 2520011.0,
          "regions": [{ "region": "nqp", "self_us": 1965608.0 }]
        }
      ]
    }
  ],
  "routines": [
    {
      "package": "JSON::Fast",
      "name": "str-escape",
      "file": "/abs/path/lib/JSON/Fast.pm6",
      "entries": 1204,
      "self_us": 41233.0,
      "incl_us": 5375210.0,
      "callers": [
        { "file": "/abs/path/lib/JSON/Fast.pm6", "line": 117, "calls": 1190, "incl_us": 5320011.0 }
      ]
    }
  ],
  "regions": [{ "region": "nqp", "self_us": 3735, "samples": 3761 }],
  "excluded_regions": [{ "region": "gc", "us": 1204.0 }]
}
```

### Header

| Field | Meaning |
| --- | --- |
| `mutsu_prof_version` | Schema version. Bumped when a field changes meaning or leaves, never when one is added, so a consumer that ignores what it does not know keeps working. |
| `mutsu_version`, `argv` | Which mutsu, invoked how. |
| `kind`, `report` | The options this document was produced under. |
| `jit`, `gc` | The configuration the profiled program **ran in**. Profiling does not change it (ADR-0106 D6): a profiler that changed JIT eligibility would measure a program nobody runs. |
| `time_is_sampled` | Always `true`. |
| `blocked_threads_absent` | Always `true`, and a property rather than a defect: the sampler is poll-based, so a thread parked in `sleep`, IO, `await` or a GC park does not poll and contributes nothing. It is *absent* from the tables, not shown as idle. |
| `sampling` | Absent when the sampler never ran (a counts-only run). Its presence is what marks the statistical numbers as a group. |
| `sampling.wall_us` vs `sampled_us` | Wall time from arm to report, against the time the tables account for. The gap is blocked and excluded time. |
| `sampling.truncated_samples` | Samples whose stack was deeper than the walk limit (192 frames). Their self time is still exact; only inclusive credit below the cut is missing. |
| `sampling.top_region` | The subsystem with the most sampled self time. *Which* tag is on top is a function of the program; the nanoseconds under it are a function of the machine. |

### Line rows

| Field | Exact? | Meaning |
| --- | --- | --- |
| `hits` | exact | How many times execution **entered** this line. |
| `self_us` | sampled | Time sampled with this line on top of the stack. |
| `incl_us` | sampled | Time sampled with this line anywhere on the stack — so a line holding a call carries what the call cost. |
| `regions[]` | sampled | The subsystem split of `self_us`. |

`hits` counts *line entries*, and a line entry is a line-**transition** edge: the
line is counted each time control arrives at it *from a different line*. It is not
a statement-execution count, and the difference is not academic. Two consequences
to know before reading any `hits` column:

- **A line that calls a routine is entered twice** per execution — once to make
  the call, and once more when control returns to finish the statement.
- **A loop whose body occupies a single line is counted once per loop entry, not
  once per trip.** Staying on the same line is not a transition, so there is
  nothing to count. Only a body spanning two or more lines gets one hit per line
  per trip:

  ```raku
  for 1..100 { $b = $b + 1 }       # that line: hits 1

  for 1..100 {
      $b = $b + 1;                 # that line: hits 1  (body is still one line)
  }

  for 1..100 {
      $b = $b + 1;                 # hits 100
      $c = $c + 1;                 # hits 100
  }
  ```

  This is a property of the counting mechanism, not of any one loop form: `for`,
  `while`, `loop` and `repeat` all behave this way, and it applies to the loop's
  header line too: a one-line `while $i < 100 { ... }` reports `hits 1`, while a
  `while` whose body is on separate lines reports one hit per condition
  evaluation on its header line — trips plus the final false one. That second
  shape is the one `tests/profile_counts.rs` pins (5,000 trips: body lines
  5,000 each, header 5,001).

So a `hits` column is a reliable trip count only where the loop body is spread
over several lines, and comparing it against NYTProf's statement counts is not
meaningful in general. [#8737](https://github.com/tokuhirom/mutsu/issues/8737)
tracks whether the counter should record a hit on a loop backedge that lands on
the same line, which would make `hits` a trip count for every loop form.

### Routine rows

`package` is `GLOBAL` for the mainline's own subs; `name` can be empty for a
synthetic or anonymous body (the text report shows `<anon>`). `file` is where the
body was **declared**, so a module's routine names the module.

`callers[]` is the per-call-site breakdown, keyed by the line the call was made
on, with `calls` exact and `incl_us` sampled. Caller rows are attached to a
routine by `(package, name)`, so two same-named routines in one package share a
caller table.

### Regions

The closed set of subsystem tags: `interp`, `call-resolve`, `method-dispatch`,
`native-builtin`, `nqp`, `regex`, `parse`, `gc`. There is no `unknown` —
`interp` is the *answer* "mutsu was running bytecode", not a residue bucket. The
split is a partition of the samples, so `sum(regions[].samples) == samples`.

`excluded_regions` is the separate, **measured** table described above.

## Limits worth knowing

- **A poll-based sampler samples where it polls.** Polls sit in the bytecode
  dispatch loops (and in the JIT's per-line hooks), so a loop-shaped program
  samples densely while a run whose time goes into one long native call attributes
  that time to the line that made the call — which is the honest answer, and why
  the region split exists to say *which* native thing it was.
- **Nothing is asserted about a duration anywhere in mutsu's own tests**
  (ADR-0106 D5), and nothing should be asserted about one in yours: a test that
  says "this line got at least N samples" is a flaky test by construction.
- **File identity.** mutsu names one file two ways at runtime — a chunk's
  canonicalized path and a frame's `$?FILE` spelling — and the profiler
  reconciles them at report time so its tables can be joined
  ([#8719](https://github.com/tokuhirom/mutsu/issues/8719) tracks settling it at
  the source). The call site of a call made inside a `use`d module is resolved
  from the enclosing routine's declaring file for the same reason.
- **`EVAL` and threads.** An `EVAL`'d unit appears under its own name
  (`EVAL_<n>`), as it does in a backtrace. A `start` block's samples belong to
  its own thread's stack and are not folded into the line that spawned it.
- **Out of scope for now**: HTML, allocation-per-line, compile-phase profiling and
  a MoarVM-shaped export (ADR-0106 Slice 6 — none of them blocked, none of them
  built).

## Related

- [ADR-0106](adr/0106-raku-level-profiler.md) — the design, the prior art it
  borrows from, the gates, and the measured overhead.
- [docs/perf-callpath-scouting.md](perf-callpath-scouting.md) — the Rust-level
  side. A profile's region tag is what tells you which callgrind question to ask.
- `MUTSU_VM_STATS=1`, `alloc_scope!` + `MUTSU_ALLOC_STATS=1` — the deterministic
  counters. When a *count* is the question, they answer it without sampling.
