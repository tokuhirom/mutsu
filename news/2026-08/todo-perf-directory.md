# Performance findings get their own `todo/perf/` directory

`todo/` split open findings two ways — `tickets/` for small self-contained
slices, `deep/` for high-blast-radius problems needing design. Both axes are
about the *size* of the work. Performance findings do not sort cleanly onto
that axis, because what makes them different is the **process**, not the size:

- the next step is profiling, not a guessed code change;
- any number that ends up in a document must come from the bench CI
  (`bench-history.tsv` on the `bench-data` branch), not a local run, because
  local A/B measurements drift with thermals and binary layout;
- the implementation agent must run **solo** — parallel perf agents on one box
  produce measurements that drift and never converge, which defeats the entire
  point of a perf finding.

Mixing them into the `tickets/`+`deep/` parallel-agent pipeline therefore
guaranteed one of two bad outcomes: either a perf finding got picked up
alongside two other build agents and produced numbers nobody could trust, or it
sat at the top of an oldest-first queue getting skipped every round.

They now live in `todo/perf/`, with eight files moved there.

## Which directory a finding belongs in

`todo/TRIAGE.md` already carried the right criterion for its perf *section*, so
this promotes that criterion from a periodically-regenerated snapshot to the
directory structure itself. A finding belongs in `perf/` only if its own next
step is measurement/profiling, or the fix is perf-only, or it is blocked purely
on a design/perf tradeoff.

The important half is the exclusion: **a perf-flavoured finding that also fixes
a genuinely wrong answer stays in `tickets/` or `deep/`.** Correctness ranks
above speed, and filing such a finding under `perf/` would hide a real bug
behind a benchmark — precisely the failure mode this repo has hit before, where
a matcher that was silently skipped made seventeen real bugs look like passing
tests.

The move is reversible in both directions and the split stays a guide rather
than a wall: a `perf/` finding that profiling reveals to be a wrong answer
rather than a slow one moves back out.

## What changed

- `todo/perf/` created; eight findings moved into it — six from `tickets/`
  (`yaml-parse-throughput`, `digest-ripemd-start-per-block-overhead`,
  `bench-ctor-construction-parity`, `bigint-repeated-addition-performance-gap`,
  `closure-sequence-evolution-performance-gap`, `uniname-sort-performance-gap`)
  and two from `deep/` (`adr0019-g3-diffuse-bless-allocation-cost`,
  `interpreter-call-path-in-hot-loops`).
- `todo/README.md` documents the third directory, the "which one" rule, and the
  three-way tally (`ls todo/perf/ | wc -l`).
- `CLAUDE.md` now records `todo/perf/` as a filing destination, states that it
  is **not** part of the `todo/deep` + `todo/tickets` parallel-agent pipeline,
  and adds the solo-agent rule next to the pipeline's other agent rules.
- Live navigational references were repointed: `PLAN.md`, four ADRs,
  `docs/batteries/digest.md`, `docs/doc-diff-backlog.md`,
  `docs/per-task-clone-slimming.md`, `todo/TRIAGE.md`, and one sibling
  `todo/deep/` file.

Historical `news/` entries that cite the old `todo/tickets/…` or `todo/deep/…`
paths were deliberately left alone. They are records of what was true when they
were written, and rewriting fourteen of them to chase a directory move would be
churn against files whose whole purpose is to be a frozen account.
