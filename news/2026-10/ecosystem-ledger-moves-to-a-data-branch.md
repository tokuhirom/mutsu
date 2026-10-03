# The ecosystem ledger moves to the `ecosystem-data` branch

The ecosystem parity measurements — one JSON record per zef distribution, the
rolled-up summary, the KPI history and its chart, and the index snapshot — no
longer live on `main`. `.github/workflows/ecosystem-sweep.yml` now commits them
straight to an orphan `ecosystem-data` branch with the default `GITHUB_TOKEN`,
the same way `bench.yml` maintains `bench-data`.

Until now every sweep ended as a pull request into `main`. Because a pull
request opened with `GITHUB_TOKEN` starts no CI, the workflow only pushed an
`ecosystem/sweep-*` branch, and a scheduled Claude Code routine (the
`ecosystem-sweep-landing` skill) opened the PR as the maintainer, enabled
auto-merge, rebuilt the branch on conflicts and deleted it afterwards. None of
that bought anything. A records-only diff is classified docs-only, so CI checked
nothing, and every guard that makes the numbers trustworthy already runs inside
the workflow. Meanwhile each nightly corpus sweep added ~1600 changed files to
`main`'s history, and feature PRs that rewrote a record conflicted with it.

Now:

- `main` keeps only the hand-maintained inputs: `ecosystem/README.md`,
  `exclude.txt` and `accepted-divergences.toml`. The measured files are
  gitignored there.
- `scripts/ecosystem-ledger.sh pull` puts the newest records into a checkout's
  `ecosystem/`, so `ecosystem-sweep.py`, `ecosystem-tickets.py`,
  `gen-ecosystem-manifest.py` and the roulette picker read the same paths as
  before. Each of them now stops with a pointer to that command instead of
  treating a missing ledger as an empty one.
- `pages.yml` pulls the ledger and redeploys `site/ecosystem.html` whenever a
  sweep run completes.
- `ecosystem-dist-fix` no longer commits a record. The PR body reports the
  re-measured verdict, and the next nightly sweep records it.
- The landing skill is gone. Its one judgement-bearing step, filing issues for
  new root-cause clusters, is now the `ecosystem-cluster-filing` skill.

The decision is ADR-0085's fourth D9 amendment, which also retires the old
"store the data on a side branch" rejection.
