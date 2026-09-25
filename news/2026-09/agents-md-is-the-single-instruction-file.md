# AGENTS.md is the single agent instruction file

The repository used to carry two instruction files for coding agents: `CLAUDE.md`
(~88 KB, the full rulebook, read by Claude Code) and `AGENTS.md` (~8 KB, a
Codex-oriented summary of the same rules). Every rule change had to be made in
both, and several `news/2026-09/` entries record doing exactly that; the summary
drifted anyway (it still told agents to initialize git submodules the repository
no longer has, and to block on a foreground `gh pr checks --watch` that
`CLAUDE.md` forbids).

Claude Code v2.1.277 added AGENTS.md support: in a project with **no**
`CLAUDE.md`, it reads `AGENTS.md`. That removed the only reason for two files.
The full rulebook now lives in `AGENTS.md` and `CLAUDE.md` is gone (its history
up to the merge is `git log -- CLAUDE.md`). Adding a `CLAUDE.md` back would switch the
fallback off and hide `AGENTS.md` from Claude Code, which is why the file's
opening paragraph says so.

The old Codex summary's content that the rulebook lacked was folded in rather
than dropped: a "Start here" reading order, the standard Rust naming line, the
"preserve unrelated working-tree changes / no destructive Git" rule, "a PR is
done when it is `MERGED` and reachable from `origin/main`", the documentation-only
verification recipe (`git diff --check`), and the ban on running the same full
suite twice concurrently. The stale submodule instruction was dropped, and the
foreground `--watch` is kept only as the fallback for a harness with no
background-completion notification.

References to `CLAUDE.md` outside the frozen records were repointed to
`AGENTS.md` (skills, docs, scripts, workflow comments, source comments, the
site's internals page). `news/` and `docs/adr/` were left untouched: they
describe what was true when they were written.
