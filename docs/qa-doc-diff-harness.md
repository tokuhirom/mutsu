# QA doc-diff harness (PLAN.md §8.1)

`scripts/doc-diff-harness.raku` is the differential tester for the finalization / QA
campaign. It extracts runnable code examples from `raku-doc`, runs each through the
reference `raku` and through `mutsu`, and reports only the cases where the two
disagree.

## How it works

- **Oracle = `raku`, in three modes.** What raku itself does with a block decides how
  the block is compared:

  | raku does | mode | what is compared |
  |---|---|---|
  | exits 0, prints something | **stdout parity** | stdout |
  | fails at **run** time | **error parity** | the failure: does mutsu fail, and with the same message |
  | exits 0, prints nothing | **silent parity** | mutsu must also exit 0 quietly |
  | fails at **compile** time (`===SORRY!===`), or times out | *no oracle* | nothing — a fragment, not an example |

  Run the harness with **system `raku`**, never mutsu, so the oracle is independent of
  the code under test.

  Only stdout parity existed until 2026-09-09. That left **3916 of the corpus's 7768
  blocks (50%) never compared at all**, because "raku did not exit 0 with output" was
  read as "not a runnable example" when it usually meant "an example that
  deliberately fails" — the entire `Type/X*.rakudoc` corpus among them. The two new
  modes are what PLAN.md §6 calls *error / exception parity*. The compile-error and
  timeout cases are what the gate was really for, and they still skip.

  Error parity ranks its findings by what the divergence means, because they are not
  equally interesting:

  - **`mutsu-accepts`** — raku refuses the program, mutsu runs it and exits 0. A
    *semantic* divergence: mutsu is silently accepting something invalid. Highest
    signal in the whole harness.
  - **`mutsu-hangs`** — raku fails, mutsu hits the timeout.
  - **`error-mismatch`** — both fail, with different messages. Usually about wording
    or the `X::` type (see #7750).
  - **`mutsu-error-on-silent-success`** / **`mutsu-extra-output`** — the silent-parity
    twins: raku succeeds quietly, mutsu dies or chatters.

  Messages are compared **without the backtrace beneath them** — frame text is
  implementation detail that would never match — and without compile-time warnings,
  which raku prints ahead of the exception and mutsu does not.

  `--/error-parity` restores the old stdout-only behaviour. It is roughly 2x faster,
  because the new modes run the oracle twice on blocks that used to cost one run.
- **Corpus = `raku-doc`.** Extracts explicit `=begin code`/`=end code` and `=for code`
  blocks (honouring `:preamble<...>` and `:skip-test`) plus 4-space indented code
  blocks. `raku-doc` is one corpus among several (see PLAN.md §8) — the same runner
  works on any set of `.rakudoc` files or a real-module corpus.
- **Noise control.**
  - Non-deterministic examples (`rand`/`.pick`/`.roll`/`now`/`Supply`/…) are skipped by
    heuristic — a first pass only; the load-bearing policy is the oracle-twice gate
    below. Explicit `# ERROR` examples used to be skipped here too, which threw away
    the very blocks error parity exists to compare; they are now let through.
  - **The oracle is run twice and the block is dropped unless raku agrees with
    itself.** This is the whole nondeterminism policy, and it deliberately replaces
    growing the pattern list above: no list can practically enumerate unordered
    container iteration (`Set`/`Bag`/`Mix`/`*Hash`/`Map`/`Hash.kv`/enum `.keys`),
    object addresses and `WHICH` ids, thread ids, `$*DISTRO`/`$*VM`/`dir` order. Such
    blocks otherwise diverge on the unreproducible token alone, every run, forever.
    They are counted as `skipped (oracle not reproducible)` — the noise floor, not
    findings.
  - Every mismatch is cross-checked against the block's own `# OUTPUT: «…»` annotation,
    and when *mutsu* matches the doc the finding carries a note saying so. That is an
    **annotation, not a bucket**: it records provenance (the doc was written against an
    older raku), never priority. There used to be a separate `raku-drift-from-doc`
    bucket described as "not mutsu bugs, lowest priority"; because it was only
    reachable once mutsu already differed from raku, 67 of its 114 blocks were real
    mutsu bugs against 5 the name fit, and nine bugs hid there. See #7590.

## Usage

```
raku scripts/doc-diff-harness.raku [--mutsu=PATH] [--timeout=N] [--limit=N]
                                   [--report=FILE] [--/error-parity] [FILES-OR-DIRS ...]
```

Defaults: `--mutsu=target/debug/mutsu`, `--timeout=10`, corpus =
`raku-doc/doc/Type` + `raku-doc/doc/Language`, report = `tmp/doc-diff-report.txt`,
error parity **on** (`--/error-parity` to disable).
The debug and release binaries produce identical output, so the debug build is fine
for correctness triage (only speed differs).

The report groups findings by kind (`output-mismatch`, `mutsu-error`, and the
error-parity kinds above), each with the exact program, the oracle's output — raku's
**stderr** for an error-parity finding, since that is what is being compared — and
mutsu's stdout/stderr — i.e. a ready-made minimal repro.
Each captured section is capped at 40 lines with an explicit truncation marker, so one
runaway example cannot bury a sweep.

The harness writes each candidate program to a per-PID scratch file
(`tmp/ddh/prog-<pid>.raku`), so multiple invocations may run **concurrently**
without clobbering each other.

### Sweeping the whole corpus in parallel

A single invocation processes its files serially, so the full ~440-file corpus
takes hours. `scripts/doc-diff-sweep.sh` fans the corpus out across worker
processes (one report per file) and writes an aggregate ranked by signal:

```
scripts/doc-diff-sweep.sh [-j N] [-o OUTDIR] [-m MUTSU] [ROOT ...]
```

Defaults: `-j8`, `-o tmp/sweep`, `-m target/debug/mutsu`, corpus = Type +
Language. Outputs `OUTDIR/reports/<file>.txt` (per file), `OUTDIR/progress.txt`
(one stats line per file), and `OUTDIR/summary.txt` (corpus totals + files
ranked by `mismatch + crash + err`, high-signal first). `mism` and `crash` keep
their pre-2026-09-09 meaning so their counts stay comparable across sweeps; the
error/silent-parity findings are the separate `err` column. Always re-verify a
finding directly before treating it as a real bug — the harness can only compare
what a doc block actually does.

## First run (2026-07-18, 8 core Type files: Str/Array/List/Hash/Num/Rat/Range/Map)

525 blocks extracted → 270 raku-clean comparisons → **50 high-signal divergences
(18.5%)**: 25 `output-mismatch` + 25 `mutsu-error`, plus 8 in the since-retired
`raku-drift-from-doc` bucket (#7590). ~2 min wall-clock (debug mutsu). The signal is dense and the
findings are genuine and cluster by root cause — validating §8.1's premise.

### First root-cause cluster found: sequence/lazy argument truncation

`1, 3 ... 11` (and lazy `gather`/`Seq`) passed as a **method/routine argument** is
materialised to only its first two elements instead of being expanded:

```raku
my @foo = <a b c>;
@foo.prepend: 1, 3 ... 11;
say @foo;   # raku: [1 3 5 7 9 11 a b c]   mutsu: [1 3 a b c]
say 600.polymod(lazy gather { take 3*$_ for 1..3 });
            # raku: (0 2 6 3)               mutsu: (600)
```

Other confirmed real findings from the same run (for the backlog): autoviv-hole
`.List`/`.Slip` renders `(Any)` instead of `Nil`/`(Int)` and drops separators; `pop`
on an empty `Array` does not throw `X::Cannot::Empty`; `try [-∞...∞].elems` does not
throw `X::Cannot::Lazy`.

## Discovery-vs-fix discipline (PLAN.md §8)

The harness is a **discovery** tool: its deliverable is a ranked backlog of minimal
repros grouped by root cause. Interpreter fixes are a separate, controlled step — do
not let a breadth-first pass bolt on slow-path fallbacks or special-cased outputs just
to make a diff go green (see the standing rules in CLAUDE.md).
