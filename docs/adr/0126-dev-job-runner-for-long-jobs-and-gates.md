# ADR-0126: Long jobs and the pre-publication gate run through one job runner, `scripts/dev`

- Status: Proposed
- Date: 2026-09-27
- Deciders: tokuhirom, Claude
- Related: `AGENTS.md` "Build, run and test" / "Before publishing a PR" / "Waiting for long
  jobs"; [docs/agent-environments.md](../agent-environments.md) (the container-only roast
  failures); [#8221](https://github.com/tokuhirom/mutsu/issues/8221) (`pipefail` masking a red
  suite)

## 1. Context

An agent session spends a large share of its wall clock on four long jobs: `cargo build
--release`, `make test`, `make roast` and `make lint`. Before any PR it must run the last three
and judge the result. How it starts those jobs, waits for them and reads their results is not
defined anywhere. `AGENTS.md` only says what *not* to do: do not poll more than every 30 minutes,
do not re-run a suite to see its output, do not run a suite twice at once. Each session improvises
the rest, and the improvisations fail in the same few ways. One session (the #9494 perf work,
2026-09-26) hit every one of them:

- **Waiting by process name matches itself.** `while pgrep -f "make lint"; do sleep 20; done` runs
  inside `bash -c '… make lint …'`, so `pgrep -f` always finds its own shell. The loop never ends.
  It happened twice: once on `cargo build --release`, which ran into the 10-minute tool timeout,
  and once on `make lint`, where the wait went on until the user asked whether it was working.
  `pkill -f "make test"` killed the calling shell the same way, twice.
- **A result file is not tied to the code it describes.** The ad-hoc pattern `make X > tmp/x.out;
  echo exit=$? >> tmp/x.out` left a finished run's `exit=0` in place when the next run started.
  A watcher keyed on `grep -q ^exit=` then almost reported a result for code that no longer
  existed.
- **The tool timeout pushes jobs out of the harness.** A foreground tool call is capped at 10
  minutes, and `make test` plus `make roast` plus `make lint` take about an hour on a 4-core
  container. Sessions start them with `setsid nohup …`, and so lose the harness's completion
  notification. They then need a hand-written wait loop, which brings back the first failure.
- **A container restart loses jobs silently.** A restart killed a detached `make roast` / `make
  lint` pair mid-run. Nothing on disk said that they had died rather than were still running.
- **The gate verdict is read by eye.** A remote container always fails four roast files (`uid 0`,
  sandboxed network). Every session compares `make roast`'s failures against the table in
  `docs/agent-environments.md` by hand, including failure counts. This is slow, and it is the
  exact place a real regression hides.
- **Nothing remembers a result.** After a rebase that changed nothing a suite reads, the whole
  gate runs again. It costs about an hour, and no one can tell whether a stored result is still
  valid.

All six are one missing abstraction: the unit of work is a *job* with a durable identity, not a
process, and the gate's answer is a *report*, not a log to read.

## 2. Decision

Add one entry point, `scripts/dev`, a self-contained Python 3 script (stdlib only, no build step).
It owns starting, waiting for, stopping and reporting on long jobs, and running the
pre-publication gate. `make` stays the definition of *how* each suite runs; `scripts/dev` is the
orchestration layer on top of it.

### 2.1 Jobs are directories, not processes

```
scripts/dev run <name> -- <command...>   # start a detached job; prints its id
scripts/dev wait <id> [--max 540]        # block until done or --max seconds; see 2.3
scripts/dev status [<id>]                # running / passed / failed / lost, for one or all
scripts/dev stop <id>                    # SIGTERM the job's process group
scripts/dev log <id> [--tail N]          # the job's combined output
```

A job lives in `tmp/jobs/<id>/`:

| file | written | holds |
| --- | --- | --- |
| `meta.json` | at start | name, argv, tree id (2.4), start time, pid / process-group id, host |
| `log` | while running | combined stdout+stderr |
| `exit` | at the end | the exit status, written last, atomically (write-then-rename) |
| `report.json` | at the end, gate only | the structured verdict (2.5) |

The job's state is a pure function of these files. `exit` exists → finished. No `exit` and the
recorded pid alive → running. No `exit` and the pid gone → **lost**: the job died without
reporting (container restart, OOM kill). The pid is the child's own recorded pid, checked with
`kill(pid, 0)` and matched against its recorded start time, so pid reuse cannot fake it. Nothing
matches process command lines, so the self-match failure cannot happen. `stop` signals the
recorded process group, never a pattern.

### 2.2 One job per suite at a time

Starting a job takes a per-name lock (`tmp/jobs/lock/<name>`, `flock`). A second `run test` while
one is running refuses, names the running job's id, and exits non-zero. This makes the existing
rule against running a suite twice at once (shared build locks, logs and harness state) something
the tool enforces instead of something the agent must remember.

### 2.3 Waiting is bounded and re-entrant

`wait` returns within `--max` seconds, default 540, just under the 10-minute tool cap. It never
blocks past that. It prints either the final verdict, or `running <id> <elapsed>` with exit
status 75 (`EX_TEMPFAIL`). The agent runs `scripts/dev wait <id>` with `run_in_background: true`
and gets the harness notification when it returns. If the job is still running, it starts another
`wait`. There is no hand-written loop left to get wrong, and no reason to detach a job from the
harness to escape the tool timeout: the job is already detached by `run`, and the *wait* is the
short-lived harness task. A `wait` on a lost job returns at once with status 70 and says so.

### 2.4 Results are keyed by the tree they describe

Every job records a tree id: `git write-tree` over the index plus the working tree (computed on a
temporary index, so the user's own index is untouched). A gate result is valid for exactly that
tree. `scripts/dev gate` first looks for a finished gate on the current tree id. If one exists, it
reports it and runs nothing. So "do not re-run a suite just to see its output" holds by
construction, and a rebase that changes no file reuses the result, while any real change runs the
gate again. `tmp/` is gitignored, so the job directories never affect the tree id.

### 2.5 The gate is one job with one structured report

```
scripts/dev gate [--only test,roast,lint] [--fresh]
```

`gate` starts a single job that runs, in order, `cargo fmt --all -- --check`, `make lint`, `make
test` and `make roast`. It stops at the first stage that fails outright (fmt, lint, a `make test`
build failure). It continues past failing *test files*, so one run shows every red file. The job
writes `report.json`:

```json
{
  "tree": "4b825dc6…", "commit": "0339ae9e", "host": "remote-4c",
  "verdict": "pass",
  "stages": {
    "fmt":   {"status": "pass"},
    "lint":  {"status": "pass"},
    "test":  {"status": "pass", "files": 4981},
    "roast": {"status": "pass", "files": 1437,
              "known_env": ["roast/S16-io/eof.t", "…"],
              "unexpected": []}
  }
}
```

`verdict` is `pass` exactly when every stage is `pass`. For `test` and `roast`, a stage passes
when its `unexpected` list is empty. `wait` and `status` print a short human summary of the
report. The report, not the log, is what a PR body quotes and what the agent acts on.

### 2.6 Environment-only failures are data

The container-only roast failures move from prose in `docs/agent-environments.md` into
`ci/known-env-failures.toml`:

```toml
[[failure]]
file   = "roast/S16-filehandles/filetest.t"
when   = "uid0"            # uid0 | no-network | restricted-proc
shape  = { failed = [57, 58, 59, 60, 61, 62, 63, 64, 69, 70, 71, 72, …] }
reason = "root bypasses permission bits"
```

`scripts/dev` detects the conditions itself (`os.getuid() == 0`, a failed loopback socket probe,
an unreadable `/proc/1/environ`). A failing file matches an entry only if the entry's condition
holds on this host *and* the failure has the recorded shape: the same failed test numbers, or the
same exit status and planned/ran counts. Any other failure, including a known file failing in a
new way, lands in `unexpected`. The docs table is rewritten to point at the data file, so there
are no longer two lists to keep in step.

### 2.7 `AGENTS.md` changes

"Before publishing a PR" becomes: run `scripts/dev gate`, wait with `scripts/dev wait`, and
publish only on `verdict: pass`. "Waiting for long jobs" becomes: start long jobs with `scripts/dev
run` (or `gate`), wait only with `scripts/dev wait`, and never locate or stop a job by process
name (`pgrep -f` / `pkill -f`). The 30-minute polling floor is dropped; a bounded `wait` does not
poll. `make test` / `make roast` / `make lint` stay available for a human at a terminal and for
iterating on one suite.

## 3. Consequences

- The failures in §1 cannot happen through the documented path, because the mechanisms they
  depended on are gone: process-name matching, unkeyed result files, hand-written wait loops,
  eyeballed verdicts.
- The gate becomes cheaper to re-run after no-op rebases, and exactly as expensive as today after
  a real change.
- One more tool to maintain. It is a single stdlib-only script with its own self-test
  (`scripts/dev self-test`, run by `make check-dev`, which `make test` depends on like the other
  `check-*` guards). The self-test covers the lost-job detection, the lock, the tree-id cache and
  the known-failure matcher against a fixture TAP log.
- The known-env data file must be kept exact. That is the point, and it is also the cost: a known
  file that starts failing differently is reported as unexpected, and someone has to look.

## 4. Alternatives considered

- **Tighten `AGENTS.md` only** ("never use `pgrep -f`"). Rejected. It covers the first failure
  and none of the other five. Rules that must be remembered are what failed here.
- **Put the logic in `make`** (a `make gate` target plus shell helpers). Rejected as the primary
  mechanism. Job identity, lost-job detection, locks, a JSON report and a known-failure matcher
  are awkward and fragile in make and shell, and #8221 already showed how easily a make pipeline
  can report success on a red suite. `make` remains the per-suite definition that `dev` calls.
- **Rely on the harness's background tasks alone** (`run_in_background` on `make test`
  directly). This is what AGENTS.md prescribes today. It does not survive a container restart,
  gives no durable result tied to a tree, and does not remove the eyeballed roast verdict. `dev
  wait` keeps using the harness notification, only for the short-lived wait instead of the job.
- **A general-purpose task runner** (`just`, `task`, a CI-style runner). Rejected. None of them
  provides the lost-job detection, the tree-keyed cache or the roast verdict that are the actual
  requirements, and each adds an install step to every container.

## 5. Implementation plan

1. `scripts/dev` job core: `run` / `wait` / `status` / `stop` / `log`, lost detection, per-name
   lock, `self-test`, `make check-dev`.
2. `gate` with the tree-id cache and `report.json`: stage parsing for fmt / lint / test / roast
   from the existing make targets.
3. `ci/known-env-failures.toml`, the host-condition probes and the shape matcher; rewrite the
   `docs/agent-environments.md` table to reference it.
4. `AGENTS.md` "Before publishing a PR" and "Waiting for long jobs" rewritten per §2.7; the
   `mutsu-ticket-flow` / `perf-tuning` skills' gate steps pointed at `scripts/dev gate`.
