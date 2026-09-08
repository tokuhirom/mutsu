# GitHub issues — the open-findings queue

Discovered bugs and missing features that are too large to fix right now are
tracked as **GitHub issues on `tokuhirom/mutsu`**, one issue per finding.

This replaced the in-repo `todo/` directory on 2026-09-08. The 57 files that
were open at that point were migrated verbatim; the frozen slug-to-issue map is
[todo-issue-map.md](todo-issue-map.md), which is what resolves the `todo/...md`
paths still cited from older ADRs, `news/` entries and source comments.

## The one hard constraint

**Only ever file, edit, label, comment on or close issues in
`tokuhirom/mutsu`.** Never touch issues in any other repository — above all not
the Raku organization's (`roast`, `raku-doc`, `rakudo`), where an AI has
actually mis-filed a mutsu issue before. This applies to pull requests too. If a
task seems to want an issue elsewhere, stop and ask the user.

## Why issues and not files

A file path is not a durable reference. A resolved finding is `git mv`d to
`news/YYYY-MM/<slug>.md`, so every code comment, ADR and news entry that cited
its `todo/...md` path rots the moment it is fixed — at migration time **108 of
the 122 distinct `todo/` paths cited from `src/` were already dangling**. An
issue number survives being closed, so an issue reference in a code comment
keeps resolving forever, and GitHub shows the PR that closed it.

Issues also give the backlog things a directory of files cannot: cross-linking
between findings, the PR that closed one, a comment thread recording an
investigation that did not end in code, and — the reason the `working` label
below exists — a place for parallel agents to see what someone else already
started.

## Labels

Two independent axes. Both use a prefix so they never collide with the
`feat` / `fix` / `perf` / `docs` / `maintenance` labels that `label-pr.yml`
applies to *pull requests* for release-note categorization.

### Kind — what sort of work it is

| Label | Meaning |
| --- | --- |
| `todo:ticket` | Small, self-contained, well-scoped. Pick it up and finish it in a session (a missing method, a parser slice, a narrow compat gap). Low risk, no design needed. |
| `todo:deep` | Deep and hard. High blast radius, multi-session, needs design or an ADR before touching (dual-store decoupling, GC, large refactors, gnarly semantics). The body must capture enough analysis that a future session can pick it up cold. |
| `todo:perf` | mutsu is *correct but slow*. Split out because the **process** differs, not the size: it needs profiling rather than a guessed change, its numbers must come from the bench CI (see "Benchmark numbers in documents" in `CLAUDE.md`), and its implementation agent must run **solo** — parallel perf agents produce measurements that drift and never converge. |

**Which kind a finding gets.** `todo:perf` is decided by the *next step*, not by
the flavour: a finding is `todo:perf` only if its own next step is
measurement/profiling, or the fix is perf-only, or it is blocked purely on a
design/perf tradeoff. **A perf-flavoured finding that also fixes a genuinely
wrong answer is `todo:ticket` or `todo:deep`** — correctness ranks above speed,
and burying it under `todo:perf` would hide a real bug behind a benchmark.

The kinds are a guide, not a wall. Relabel freely: a `todo:deep` problem that
turns out to be a quick fix becomes `todo:ticket`, and a `todo:perf` finding
that profiling reveals to be a wrong answer rather than a slow one moves out of
`todo:perf`.

### Tier — how much it matters

Assigned by the triage regen (see [triage.md](triage.md)), not by the filer.
Leave it off when you file; a missing tier just means "not yet triaged".

| Label | Meaning |
| --- | --- |
| `tier:S` | Soundness. Silent data loss, hangs, wrong answers with no diagnostic. |
| `tier:B` | Broad correctness — a whole language construct, or a dist-blocking battery gap. |
| `tier:N` | Narrow correctness, diagnostics, permissiveness. |
| `tier:icebox` | Blocked on a decision, or a pure record with no actionable next step. |

### `working` — someone is on it right now

Agents run in parallel, and two of them picking the same issue wastes a build
slot and produces conflicting PRs. So:

- **Before starting work on an issue, declare it**: post a comment saying you
  are starting, and add the `working` label.
- **When you finish, remove the `working` label.** "Finish" means the PR merged,
  or you stopped — an abandoned or blocked issue must not keep the label, or it
  silently removes itself from every other agent's queue.
- **Before picking an issue, check for the label.** An issue carrying `working`
  belongs to someone else; take the next one instead.
- A closing PR removes the issue from the queue anyway, but remove the label
  explicitly rather than relying on that — the label is what other agents read
  while your PR is still in CI.

## Filing an issue

Use the templates in `.github/ISSUE_TEMPLATE/`. Title: the finding itself, as a
sentence, the way the H1 of the old files read (`@a[0 .. $n]` hangs forever when
`$n` holds a negative value) — not a bare component name.

Body, in prose (the templates carry these headings):

- **Root cause** — what actually goes wrong, and where.
- **Affected files** — the modules/paths involved.
- **Why it is large** — why it cannot be fixed in one sitting.
- **Repro** — a minimal script or roast path that exhibits it, with mutsu's
  output and `raku`'s side by side.

A `todo:deep` issue naturally carries more analysis than a `todo:ticket` one.
Write it so a session that has never seen the problem can pick it up cold; the
body is the handoff, and there is no other record.

Do not let a hard finding evaporate at the end of a session. Filing the issue
costs one tool call and is always worth it.

## Lifecycle

- **open, unlabelled `working`** → available. Anyone may take it.
- **open + `working`** → an agent has declared it and is on it.
- **closed** → the fixing PR closed it (`Closes #NNNN` in the PR body), or it
  was closed as `not_planned` because it evaporated. Either way, write the
  accomplishment up as `news/YYYY-MM/<slug>.md` as before — `news/` is still the
  narrative record of what was done, and the issue is the tracking record.

`PLAN.md` stays for planned strategic/campaign work; ad-hoc discovered findings
go to issues.

Roast per-test pass/fail status stays in its own ledger,
`TODO_roast/BLOCKERS.md` — do not duplicate roast tracking in issues. (There is
no special rule for roast: a genuinely deep, non-roast-specific problem you
happen to hit via a roast test still gets a `todo:deep` issue.)

## Tooling

Some sessions have the `gh` CLI (already authenticated via
`~/.config/gh/hosts.yml`); ephemeral remote containers generally do **not**, and
direct `api.github.com` calls from them are rejected by the proxy. Use whichever
is actually present:

```sh
# gh, where it exists
gh issue list  --repo tokuhirom/mutsu --label todo:ticket --search '-label:working'
gh issue create --repo tokuhirom/mutsu --title '...' --body-file tmp/issue.md --label todo:ticket
gh issue edit  <n> --repo tokuhirom/mutsu --add-label working
gh issue edit  <n> --repo tokuhirom/mutsu --remove-label working
```

Otherwise use the GitHub MCP tools (`list_issues`, `issue_write`,
`add_issue_comment`, `issue_read`), always with
`owner: tokuhirom`, `repo: mutsu`. **`issue_write` with `method: "update"`
*replaces* the whole label set**, so to add `working` you must pass the issue's
existing labels alongside it — read them first, or you will silently drop its
kind and tier. `gh issue edit --add-label` / `--remove-label` do not have this
hazard.

Do **not** wrap `gh` in `dotenvx run --`: the `GH_TOKEN` in `.env` is stale and
would override the working token with bad credentials.
