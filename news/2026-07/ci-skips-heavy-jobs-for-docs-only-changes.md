# CI skips the heavy jobs on a documentation-only change

Roughly one commit in six on this repo touches nothing but documentation — an
ADR, a `news/` entry, a `todo/` ticket, a line in `PLAN.md`. Until now every one
of those paid the full CI bill: four jobs (`test`, `wasm-e2e`, `gc-stress`,
`jit-stress`), two cargo builds each on the native ones, and three separate
sweeps of the whole roast whitelist. Twenty-five to thirty minutes of runner
time, three times over, to prove that editing a Markdown file did not break the
interpreter.

The obvious fix — a workflow-level `paths-ignore` on `ci.yml` — is a trap. The
`main` ruleset requires the `test`, `wasm-e2e`, and `gc-stress` status checks,
and when a workflow is filtered out by `paths-ignore` GitHub does not create the
check runs at all. They stay pending forever and the PR becomes permanently
unmergeable. The supported way to skip a *required* check is the opposite: let
the job exist and skip it with a job-level `if:`, because a skipped job counts
as success for branch protection.

So `ci.yml` gained a `changes` job that runs first and classifies the diff, and
the four heavy jobs now carry `needs: changes` plus
`if: needs.changes.outputs.docs-only != 'true'`. The classifier lives in
`scripts/ci-docs-only.sh`.

It is deliberately an allowlist rather than a denylist, and it fails safe in
every direction. A path counts as documentation only if it sits under `docs/`,
`news/`, `todo/`, `TODO_roast/`, `old-design-docs/`, or `raku-doc/`, or is a
top-level `*.md` (or `LICENSE`). Everything else — `src/`, `t/`, `roast/`,
`scripts/`, `wasm-demo/`, `modules/`, `Cargo.*`, and `.github/**` itself —
forces the full suite. `**/*.md` is *not* on the list: a `README.md` under
`modules/` sits next to files the build reads, and the cost of guessing wrong
there is a silently-untested merge. The script exits 0 unconditionally and
prints `false` for anything it cannot determine: an unknown event, an
unreachable `before` SHA after a force-push, an empty file list, or a diff over
300 files (which pagination or the API cap could have truncated). A wrong
`false` costs runner minutes; a wrong `true` lets untested code reach `main`.

Because a bad edit to those path rules would be invisible — the jobs would just
quietly stop running — the script carries a `--self-test` mode with fifteen
classification cases, and the `changes` job runs it before classifying anything.
It costs about a second and turns a broken allowlist into a red CI instead of a
skipped one.

`bench.yml` got the same treatment through the ordinary `paths-ignore`, which is
safe there because no bench check is required by the ruleset. Benchmarking a
docs merge could not move a number anyway, and the row it appended to
`bench-history.tsv` differed from its predecessor only by runner noise — noise
that reading the history already requires normalizing away.

Two pieces of dead configuration went with it: the `if:` guards on `wasm-e2e`,
`gc-stress`, and `jit-stress` still skipped "tagpr's release PR" by matching a
`tagpr-from-` branch prefix, and tagpr was removed from this repo in July 2026.
