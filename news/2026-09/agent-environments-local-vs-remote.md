# CLAUDE.md no longer assumes the maintainer's local box

`CLAUDE.md` grew up on the maintainer's LXC dev box, so its recipes named `gh` unconditionally,
quoted 12-core concurrency caps and wall-clock timings, and described "the container" as if there
were only one. A growing share of sessions now start from the Claude app / Claude Code on the web
and land in an ephemeral remote container instead, where `gh` does not exist at all, direct
`api.github.com` calls are rejected by the session proxy, there are roughly four cores, and disk is
a fixed per-session allowance. The mismatch was a recurring source of confusion: an agent reading
"open a PR with `gh pr create`" in an environment with no `gh` has to rediscover the GitHub MCP
tools from scratch, and one reading "at most 3 concurrent building agents" on a 4-core box takes the
number literally.

The differences are now recorded once, in **`docs/agent-environments.md`**:

- how to tell the two environments apart (`command -v gh || echo remote`, `/root/.ccr/`, `nproc`,
  the `SessionStart` hook's `environment ready` line);
- a `gh` → GitHub-MCP mapping table covering the whole PR lifecycle (`create_pull_request`,
  `pull_request_read` with its `get` / `get_diff` / `get_check_runs` methods, `enable_pr_auto_merge`
  with `mergeMethod: "MERGE"`, `actions_list`, `actions_run_trigger`, `get_job_logs`, `issue_write`),
  plus the `issue_write` label-replacement hazard;
- how rust and raku get provisioned by `.claude/hooks/session-start.sh`, so no session hand-installs
  a toolchain that is already there;
- that the parallel-agent caps and quoted timings are 12-core numbers to be scaled, not constants;
- the remote disk allowance behaviour, where `df` misleads;
- and, deliberately, a list of what does **not** differ: every `cargo` / `make` / `prove` command,
  all `git` operations, and the entire PR policy — a remote session is not a licence to relax them.

`CLAUDE.md` itself gained a short "Where this session is running" section near the top and keeps the
`gh` form inline for brevity, with the MCP equivalent named at each point where it previously implied
a command that half of all sessions do not have (PR open, auto-merge, mergeability check, CI watch,
PR diff, release workflow dispatch). The two `gh`-heavy skills (`mutsu-ticket-flow`, `cut-release`)
now say up front that their commands are the local form and point at the mapping table, and
`docs/issue-workflow.md` — which already handled both worlds for issues — cross-links it.
