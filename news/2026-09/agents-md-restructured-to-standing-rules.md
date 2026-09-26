# AGENTS.md restructured to the rules every session needs

With `CLAUDE.md` merged into it, `AGENTS.md` was a ~90 KB file loaded into
every agent session — about 22k tokens — of which a large part was reference
material, history, and procedures only some tasks use. The same rule was often
stated two or three times (the pre-publication full-suite gate was split across
three sections; the PR workflow appeared in the working agreements, the
queue-request section and the `mutsu-ticket-flow` skill), and parts had gone
stale (the VM module list named files that no longer exist; the opcode count
said ~340 against ~375; it still pointed at `runtime/test_functions.rs` after
the native `Test` provider was deleted).

`AGENTS.md` is now ~27 KB and ordered by the life of a task: start here (with
an index of skills and reference docs), hard rules, where the session runs,
build/run/test and the pre-publication gate, code rules, design judgment (gain
and risk, refactor boldly, ADRs, trust `main`), roast, git/PR/CI, issues and
news, and agents and queue requests. Every rule was kept; each is stated once.

What moved out, and where:

- The module map, parser error metadata, the slang note and the GC/JIT status
  went to the new `docs/architecture.md`, with the stale VM file list replaced
  by a description of the file families.
- The `raku-doc/` file index went to the new `docs/raku-doc-guide.md`.
- The flake history and the suspected-flake triage protocol became §7 and §8
  of `docs/flaky-test-policy.md`, which already owned the mechanism.
- The CI job layout and the docs-only allowlist details went to the new
  `docs/ci-pipeline.md`, together with a note learned while landing the merge
  PR: a push cancels the previous run, and the aggregator jobs then report that
  cancelled run as red on the superseded commit.
- The debugging recipes (AST dump, `MUTSU_TRACE`, scripted `rust-gdb`, the
  env-gated backtrace build) became the `debugging` skill.
- The parallel-agent issue pipeline and the rationale for the build-agent cap
  became the `issue-backlog-pipeline` skill.
- The mzef/distribution working rules were appended to
  `docs/mzef-install-pipeline.md`; the SessionStart-hook details already lived
  in `docs/agent-environments.md`.

References to the removed sections from skills, docs and `PLAN.md` were
repointed to their new homes.
