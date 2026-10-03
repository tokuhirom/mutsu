# CI on `main` and the Pages deploy run on a schedule

The repository is public, so GitHub Actions minutes are free, but the free
plan caps hosted runners at 20 concurrent jobs. At ~100 merges a day PR runs
were queueing 5-15 minutes per job: a PR whose jobs did ~25 minutes of work
took ~33 minutes of wall clock, one 2-second aggregator job waited 3 minutes
for a runner.

Two consumers of that budget did not need per-push granularity:

- **CI on `main`** ran the whole workflow (seven jobs, ~40 runner-minutes) on
  every merge, keeping one full run in flight around the clock. It now runs
  hourly. The `changes` job diffs against the last green run on `main`, so an
  unchanged `main` skips every build job and a run of docs-only merges skips
  them like a docs-only PR. The cargo caches PRs restore are still saved there.
- **Pages** deployed on every push and again after every Bench run. It now
  deploys every two hours, plus after a Release and after the daily Ecosystem
  sweep.

`docs/ci-pipeline.md` ("Runner budget") records the arrangement.
