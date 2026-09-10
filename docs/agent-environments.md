# Agent environments: the local box vs. a remote container

`CLAUDE.md` is written as a set of shell recipes, and most of them were first written on the
maintainer's own machine. Sessions actually run in **two different kinds of environment**, and a
recipe that is exactly right in one is impossible in the other — `gh pr create` does not exist in a
remote container, and `.claude/worktrees/` sub-agent batches sized for a 12-core box wedge a 4-core
one. This file is the single place where those differences are recorded. When a recipe elsewhere in
the repo assumes the wrong environment, translate it with the tables below rather than concluding
that the task cannot be done.

## Which one am I in?

Check, do not assume — the same repo, the same branch and the same `CLAUDE.md` are present in both.

| Signal | Local dev box | Remote container |
| --- | --- | --- |
| `command -v gh` | a path | nothing |
| `/root/.ccr/` | absent | present (agent proxy CA bundle, `README.md`) |
| `SessionStart` output | hook is a no-op | `session-start: environment ready` (see below) |
| `nproc` | 12 | typically 4 |
| GitHub MCP tools (`mcp__github__*`) | usually absent | present |

One command that settles it: `command -v gh || echo remote`.

**Local dev box** — the dedicated mutsu LXC container the maintainer develops in. `gh` is
authenticated via `~/.config/gh/hosts.yml`, rustup and rakudo are already installed, `.mise.toml`
pins the toolchain, and `target/` is warm across sessions.

**Remote container** — a fresh, ephemeral container created for a session started from the Claude
app or Claude Code on the web. The repo is cloned at session start, there is no `gh`, outbound
HTTPS goes through a session proxy, and the container is reclaimed when the session ends.

## GitHub operations

On the local box use `gh`. In a remote container `gh` is absent **and direct `api.github.com` calls
are rejected by the session proxy**, so the GitHub MCP tools are the only path. `GH_TOKEN` /
`GITHUB_TOKEN` may be set in the environment there — they do not help, do not try to use them with
`curl`. Note also that on the local box `gh` must never be wrapped in `dotenvx run --`: the
`GH_TOKEN` in `.env` is stale and would override the working token.

All MCP calls take `owner: tokuhirom`, `repo: mutsu`.

| Operation | Local (`gh`) | Remote (MCP tool) |
| --- | --- | --- |
| Open a PR | `gh pr create` | `create_pull_request` |
| Read a PR | `gh pr view <n> --json ...` | `pull_request_read` method `get` |
| PR diff | `gh pr diff <n>` | `pull_request_read` method `get_diff` |
| CI status of a PR | `gh pr checks <n>` | `pull_request_read` method `get_check_runs` (or `get_status`) |
| Failing job logs | `gh run view --log-failed` | `get_job_logs` with `run_id`, `failed_only: true`, `return_content: true` |
| Enable auto-merge | `gh pr merge --auto --merge <n>` | `enable_pr_auto_merge` with `mergeMethod: "MERGE"` |
| List workflow runs | `gh run list --branch <b>` | `actions_list` method `list_workflow_runs`, `workflow_runs_filter: {branch: <b>}` |
| Dispatch a workflow | `gh workflow run tag-release.yml -f version=X.Y.Z` | `actions_run_trigger` method `run_workflow` (`workflow_id`, `ref`, `inputs`) |
| File / update an issue | `gh issue create` / `gh issue edit` | `issue_write` method `create` / `update` (labels go in `labels`) |
| Update the branch from base | `git fetch origin main && git rebase origin/main` | same, or `update_pull_request_branch` |
| Comment on an issue | `gh issue comment` | `add_issue_comment` |
| Read issues | `gh issue list --label todo:ticket` | `list_issues` / `issue_read` |

`git` itself works identically in both (the remote container pushes through a git proxy), so
`git checkout -b`, `git commit`, `git push -u origin <branch>`, `git fetch`, and rebases need no
translation. **Only the GitHub API layer differs.**

Two consequences worth spelling out:

- **`--merge`, never `--squash`.** Squash merging is disabled on this repository. In MCP terms that
  is `mergeMethod: "MERGE"` (`"REBASE"` is also allowed); `"SQUASH"` fails and silently leaves
  auto-merge off.
- **`issue_write` with `method: "update"` replaces the whole label set.** To add `working` you must
  pass the issue's existing labels alongside it — read them first, or you silently drop its kind and
  tier labels. `gh issue edit --add-label` / `--remove-label` have no such hazard. The rest of the
  issue conventions are in [issue-workflow.md](issue-workflow.md).
- **Watching CI is a polled read in both worlds.** Locally, a `run_in_background` loop over
  `gh pr checks`. Remotely there is no shell command to loop on, so either call
  `pull_request_read`/`get_check_runs` again after doing other work, or use
  `subscribe_pr_activity` so CI results and review comments wake the session.

## Provisioning: rust, raku and the native C libraries

`.claude/hooks/session-start.sh` (registered as a `SessionStart` hook in `.claude/settings.json`)
installs the highest Rust version the repo declares, runs `.agents/skills/install-raku/install-raku.sh`
when `raku` is missing, installs the C shared libraries the bundled batteries `dlopen`, and warms
the crate cache with `cargo fetch`. It is idempotent (~0.3s when everything is in place) and does
nothing on a local checkout unless `MUTSU_SETUP_FORCE=1` is set — a developer machine is pinned by
`.mise.toml` and owns its own toolchain.

The native-library list is the `NATIVE_LIBS` array at the top of `setup_native_libs()`, one
`"<soname> <apt package>"` row each. It currently holds a single entry, `libmysqlclient.so.21` /
`libmysqlclient21` (`libpq` and `libsqlite3` already ship in the base image). Note that
`default-libmysqlclient-dev` is *not* a substitute: on Ubuntu it pulls MariaDB's `libmariadb.so.3`,
which DBIish's `NativeLibs::Searcher.try-versions('mysqlclient', 16..21)` does not match. Installing
it costs about 2s on a cold container; the whole hook then takes ~0.14s on every later run.

So **do not hand-install rustc or rakudo at the start of a remote session** — it has already
happened, and `raku` is available as the oracle. If a build still fails with `E0658`, the hook did
not run (look for its `session-start: environment ready` line) and
`.claude/skills/rustc-too-old/SKILL.md` applies. When a version pin moves the hook follows it with
no edit; only the *sources* of the pins are hardcoded, so add a new one there if the repo ever
grows a `rust-toolchain.toml`.

## Cores, parallelism and sub-agents

The concurrency numbers in `CLAUDE.md` ("at most 3 concurrent agents that build") were measured on
the 12-core local box. **They are a ratio, not a constant** — on a 4-core remote container, roughly
one building agent is the equivalent, and running the batch inline in the main session is usually
better than paying for worktree copies of `target/`. Check `nproc`, `uptime` and `pgrep -c -x rustc`
before launching anything parallel; a two-digit `rustc` count is oversubscribed on any of these
boxes.

The same ratio applies to timings quoted anywhere in the repo: `make lint` at "about 5 minutes" and
the roast suite's wall-clock are 12-core numbers, so budget more on a smaller container. Run only
the specific tests your change touches while you iterate — but the pre-publication gate (`make test`
and `make roast` once each, both green, before opening the PR) is the same in both environments. A
smaller box makes that gate slower, not optional.

## Environment-only `make roast` failures — the remote container's fixed set

`make roast` cannot come back fully green in a remote container, for reasons that have nothing to do
with any change. Re-deriving this every session is pure waste, so here is the whole set. **These
three files, and only these three, may be red when you publish**; anything else is a real failure and
the "do NOT dismiss them as pre-existing" rule in `CLAUDE.md` applies in full.

| File | Shape | Why |
|---|---|---|
| `roast/6.c/S32-io/file-tests.t` | `Failed: 4` — tests 6-8, 10 | runs as `uid 0` |
| `roast/S16-filehandles/filetest.t` | `Failed: 25` — tests 57-64, 69-72, 77-80, and 101/103/105/107/109/111/117/121/125 | runs as `uid 0` |
| `roast/S32-io/IO-Socket-Async.t` | exit 124, "planned 40 ran 17" | sandboxed network |

The first two are the same cause: both `chmod` a file and then assert `.r` / `.w` / `.x` is `False`,
and **root bypasses the permission bits**, so every such assertion is `True`. Check with `id -u` —
`0` means these cannot pass, no matter how correct the interpreter is. They pass in CI, which runs as
an ordinary user.

`IO-Socket-Async.t` times out part-way through in this container and passes in CI; the container's
network sandbox is the difference. The mechanism has not been pinned down further, so treat a *new*
failure shape there (a concrete `not ok`, rather than the timeout) as real.

Two things this list is not:

- **It is not a licence to skim the summary.** Read the `Test Summary Report` and confirm the failing
  set is a subset of these three by name. A fourth file, or a different subtest range inside these,
  is your change.
- **It does not apply to the local box**, which runs as an ordinary user with a working network.
  There, `make roast` is expected to be green.

## Environment-only `scripts/battery-testsuite.sh` failures

The battery gate is not part of CI (it runs at release time, and on demand when a change can affect
a bundled library's own code paths). Historically it could not come back green in a remote container
for one reason: **`libmysqlclient` was not installed**, so DBIish's MySQL files could not run. The
tell was a `NativeCall: symbol 'mysql_init' not found in 'this process': dlsym failed` raised from
`DBDish/mysql.rakumod`, and a `FAIL(ok=0/N,notok=0)` — zero assertions ran, none failed.

**The `SessionStart` hook now installs `libmysqlclient21`** (see "Provisioning" above), so this
should no longer happen. If you do see that shape:

| Files | Shape | Why |
|---|---|---|
| `DBIish/24-mysql-types.rakutest`, `24-mysql-types-json`, `25-mysql-common`, `26-mysql-blob`, `27-mysql-datetime`, `28-mysql-connection-lock` | `FAIL(ok=0/N,notok=0)`, or a `timeout 120` part-way through | no `libmysqlclient` in the container |

`ldconfig -p | grep -c mysql` returning `0` confirms it, and means the hook did not run or could not
reach the archive — re-run it by hand with `MUTSU_SETUP_FORCE=1 .claude/hooks/session-start.sh`
rather than treating the failures as environmental. The same two caveats as the roast list above
apply: confirm the failing set is a subset of these *by name*, and a `notok` greater than zero in any
of them is a real failure, not this.

Run the gate **alone** — two of them in a shared `tmp/battery-testsuite` workdir produce a false
REGRESSION — and remember it needs network access to fetch each upstream suite at its pinned commit.

## Disk

Local: `target/` and `.claude/worktrees/` are the two hogs; clean them per
`.agents/skills/reclaim-disk/SKILL.md`.

Remote: writable disk is a **fixed per-session allowance**, so `df` misleads — "Avail" near zero
with low "Used" means the allowance is spent, not that the machine is broken. Deletes still succeed
while writes fail, so on "no space left on device" remove build artifacts, caches and stale clones
(`target/*/incremental` first, per the same skill) and the freed space is immediately writable.

## Ephemerality

Both environments can disappear: the local LXC container may be destroyed at any time, and a remote
container is reclaimed after the session ends or goes idle. The rule is the same in both — **commit
and push promptly**; anything not pushed to a branch on `origin` is not saved. This is also why the
PR workflow insists an agent lands its own PR end-to-end instead of handing back an unpushed diff.

## What does *not* differ

Do not "adapt" these to the environment — they are the same everywhere, and a remote session is not
a licence to relax them:

- Build, test and run commands: `cargo build`, `make test`, `make roast`, `prove -e 'target/debug/mutsu'`,
  `MUTSU_FUDGE=1` for roast, `timeout 30` when running mutsu.
- The PR policy: never commit to `main`, always a feature branch and a PR, auto-merge with `--merge`,
  verify the PR is not `DIRTY` right after opening it, fix CI forward on the same branch.
- Repository artifacts (commits, PR titles/bodies, docs, `news/`, ADRs, code comments) are written
  in English even when the conversation is in Japanese.
- Temporary scripts go to the project-local `tmp/`, never to `/tmp/`.
- Issues and PRs are only ever filed against `tokuhirom/mutsu`.
