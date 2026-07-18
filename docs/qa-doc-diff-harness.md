# QA doc-diff harness (PLAN.md §8.1)

`scripts/doc-diff-harness.raku` is the differential tester for the finalization / QA
campaign. It extracts runnable code examples from `raku-doc`, runs each through the
reference `raku` and through `mutsu`, and reports only the cases where the two
disagree.

## How it works

- **Oracle = `raku`.** A block is compared *only* when raku itself runs it cleanly
  (exit 0, no compile `SORRY`, non-empty stdout). This naturally filters out doc
  fragments, intentional-error snippets, and non-runnable examples — no hand-curation
  needed. Run the harness with **system `raku`**, never mutsu, so the oracle is
  independent of the code under test.
- **Corpus = `raku-doc`.** Extracts explicit `=begin code`/`=end code` and `=for code`
  blocks (honouring `:preamble<...>` and `:skip-test`) plus 4-space indented code
  blocks. `raku-doc` is one corpus among several (see PLAN.md §8) — the same runner
  works on any set of `.rakudoc` files or a real-module corpus.
- **Noise control.**
  - Non-deterministic examples (`rand`/`.pick`/`.roll`/`now`/`Supply`/…) and explicit
    `# ERROR` examples are skipped by heuristic.
  - Every mismatch is cross-checked against the block's own `# OUTPUT: «…»` annotation.
    When raku no longer matches the doc, the finding is bucketed as
    **`raku-drift-from-doc`** (raku changed since the doc was written; mutsu may well
    match the doc) and de-prioritised versus a true `output-mismatch` where raku and the
    doc agree but mutsu is wrong. This implements the §8.4 language-version caveat.

## Usage

```
raku scripts/doc-diff-harness.raku [--mutsu=PATH] [--timeout=N] [--limit=N]
                                   [--report=FILE] [FILES-OR-DIRS ...]
```

Defaults: `--mutsu=target/debug/mutsu`, `--timeout=10`, corpus =
`raku-doc/doc/Type` + `raku-doc/doc/Language`, report = `tmp/doc-diff-report.txt`.
The debug and release binaries produce identical output, so the debug build is fine
for correctness triage (only speed differs).

The report groups findings by kind (`output-mismatch`, `mutsu-error`,
`raku-drift-from-doc`), each with the exact program, raku stdout, and mutsu
stdout/stderr — i.e. a ready-made minimal repro.

## First run (2026-07-18, 8 core Type files: Str/Array/List/Hash/Num/Rat/Range/Map)

525 blocks extracted → 270 raku-clean comparisons → **50 high-signal divergences
(18.5%)**: 25 `output-mismatch` + 25 `mutsu-error`, plus 8 low-priority
`raku-drift-from-doc`. ~2 min wall-clock (debug mutsu). The signal is dense and the
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
