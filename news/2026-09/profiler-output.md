# The profiler has output: a CLI, a JSON document, and a report you can read

ADR-0106's profiler could measure a run since Slice 2 and tag which mutsu subsystem
the time went to since Slice 4, but nothing could *read* it. The data folded into
process-global tables behind a scaffolding `eprintln!` whose own doc comment called
itself "not the profile document", there was no way to ask for it except an
environment variable, and the only consumers were three Rust integration tests
scraping `profile: line …` out of stderr. Slice 5 ([#8705](https://github.com/tokuhirom/mutsu/issues/8705))
is the half that makes it a tool.

## What landed

One debug-build run of `benchmarks/bench-json-fast.raku`, abridged. The times below
are sampled, so they are a shape rather than a measurement — a wall-clock figure for
this benchmark comes from the bench CI, never from here:

```console
$ mutsu --profile benchmarks/bench-json-fast.raku
mutsu-prof: 14.96s wall, 1,974 samples @1000Hz (timer) on 1 thread, jit=on gc=on, kind=both
            hits are exact; every time below is SAMPLED -- never quote it as a measurement
TOP SELF LINES
   1  modules/JSON-Fast/lib/JSON/Fast.pm6:275   56.6% self  hits 452    [nqp 66% | native-builtin 30% | call-resolve 4%]
   2  modules/JSON-Fast/lib/JSON/Fast.pm6:435   26.3% self  hits 1,217  [nqp 95% | interp 5%]
   3  modules/JSON-Fast/lib/JSON/Fast.pm6:253    7.4% self  hits 309    [nqp 64% | native-builtin 20% | call-resolve 16%]
TOP ROUTINES (inclusive)
   1  JSON::Fast::jsonify   73.0% incl  entries 573
        <- modules/JSON-Fast/lib/JSON/Fast.pm6:275  353 calls (62% of the calls seen)
REGIONS (sampled self time)
  nqp 71.2% native-builtin 20.6% call-resolve 6.5% interp 1.7%
Writing profiler output to mutsu-prof.json
```


- **The surface** (`src/profile/options.rs`): `--profile[=FILE]` (default
  `mutsu-prof.json`), `--profile-kind=line|routine|both`, `--profile-rate=<Hz>`,
  `--profile-report=json|text|both`, `--profile-jit=on|off`, each with a
  `MUTSU_PROFILE*` environment twin so a run mutsu did not spawn itself — a `prove`
  harness, a module's own test suite, an embedder — can still be profiled.
- **The document** (`src/profile/document.rs`), schema documented in
  [docs/profiler.md](../../docs/profiler.md): a pretty-printed JSON object with a
  header (version, argv, JIT/GC state, rate, wall time, sample count, an explicit
  `"time_is_sampled": true`), per-file per-line rows carrying exact `hits` beside
  sampled `self_us`/`incl_us` and the subsystem split, and routine rows with the
  per-caller breakdown.
- **The text report** (`src/profile/text.rs`), which had to be good enough to make
  HTML optional (ADR-0106 D7), because HTML is out of scope: top self lines with
  their hit counts and region split, top routines by inclusive time, who called
  them, the whole-run region split, and the separately-named *excluded* time that
  the line table deliberately does not contain.

Both renderings read the same document, built once from the two snapshots, so the
JSON and the text cannot drift apart. Adding a third renderer later (HTML, a
MoarVM-shaped export) is another reader of the same value, not another traversal of
the raw tables.

## Absent is not zero

Every measured field is **omitted** when this run did not measure it. A line the
counters saw and the sampler never sampled has no `self_us`; a line the sampler
reached only as an inclusive frame has no `hits`; the half `--profile-kind` did not
ask for is missing rather than present and empty. The reason is that a zero is
indistinguishable from data: `"hits": 0` would make a consumer conclude the line
never ran, when what happened is that a different half of the profiler measured it.
The text report follows the same rule — an unsampled row's percentage column is
`-`, never `0.0%`.

## The tests now read the published artifact

`tests/profile_counts.rs`, `tests/profile_samples.rs` and `tests/profile_regions.rs`
used to parse the scaffolding report's stderr lines. They now go through one shared
reader (`tests/profile_doc/`) over the real JSON document, which means the
assertions that pin the counters (gate 3's "this line's `hits` is exactly the trip
count", gate 4's JIT parity, Slice 4's region partition) also pin the schema. Two
new Raku-level tests cover what a Rust test cannot: `t/tooling/profiler-report.t`
runs a fixture through the actual CLI and checks the document from Raku, and
`t/tooling/profiler-cli-options.t` pins the option surface — including the
deliberate split where a *flag* with an unimplemented value is an error (stderr,
exit 1: `--profile-kind=heap` is rakudo's `Unknown profiler specified`, since mutsu
does not do heap profiling and declines to claim the spelling) while an *environment
variable* with one warns and falls back, and where an unknown option stays
ADR-0017's exit 0. No test anywhere asserts a duration or a sample count: ADR-0106
D5 makes that a flaky test by construction.

## What the first real profile found

Pointing it at `benchmarks/bench-json-fast.raku`, the intended first customer,
immediately produced a wrong answer — and not in the new code. Every callsite inside
JSON::Fast was filed under the *benchmark script's* path carrying the *module's*
line numbers: a caller row reading `bench-json-fast.raku:275` for a file that is 84
lines long. A frame records its call site's file as the dynamically-scoped `?FILE`,
which still names the mainline while a `use`d module's routine is running
([#8719](https://github.com/tokuhirom/mutsu/issues/8719)), and no consumer had ever
read that field across a module boundary before.

The call site is in the body of the *enclosing* routine, so its file is that
routine's declaring file. The sampled side now computes it in one outward pass over
the stack during the fold (a `def_file: None` — "the same file as the caller" —
inherits from the frame outside it, which is what its contract says), and the exact
counter resolves it from the frame stack at the push site, where the stack's top
still *is* the caller. Like `src/profile/paths.rs`, this is a profiler-side
reconciliation that changes no Raku-visible file: `$?FILE`, backtraces and
`CallFrame.file` are untouched, and #8719 still owns settling the divergence at the
source. `t/tooling/profiler-report.t` pins it with a script-plus-module fixture.

That is the argument for building the output half at all, in miniature: the data had
been collected correctly for weeks, and it took one readable report on one real
multi-file program to show that a column of it was pointing at the wrong file.
