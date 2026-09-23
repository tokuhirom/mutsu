# CLAUDE.md prompt audit: contradictions and dead tool references removed

A prompt audit of the agent-facing instruction surface (`CLAUDE.md`, `.agents/skills/*`,
`.claude/skills/*`) against the current Claude models found the skills clean and `CLAUDE.md`
carrying a handful of stale or self-contradicting instructions. They were fixed:

- **Tools that do not exist.** `CLAUDE.md` and two skills told agents to search with "the Grep tool"
  and "the Grep and Glob tools" and forbade `grep` via Bash. Current harnesses do not always expose
  such tools, which made the rule impossible to follow (and it already clashed with the debugging
  section's own "`git grep` the opcode arm"). The rule now states the intent: use a dedicated search
  tool when the harness has one, otherwise `git grep` / `rg`.
- **Two pairs of rules that disagreed.** The "Refactor boldly" section still said "push and let CI's
  full roast run be the safety net", contradicting the pre-publication gate ("run both full suites
  yourself before publishing"); it now points at that gate. A leftover "Update (ADR-0001): Track B
  must NOT be started standalone" parenthetical contradicted the GC section, which records that rule
  as superseded by ADR-0013; it was removed, as was the "This supersedes the older rule" aside.
- **A pinned model name.** The agent-prompt checklist hardcoded a `Co-Authored-By: Claude Sonnet 5`
  trailer; it now defers to the attribution trailer the session's harness specifies, so it does not
  go stale at the next model release.
- **An unenforced rule.** "Keep each Rust source file under 500 lines ... split immediately" was
  violated by 404 of 1114 files under `src/` with nothing checking it. It is restated as the rule
  that is actually followed: do not grow a file past 500 lines, split the one your change pushes over.
- A list of commit hashes cited as evidence for the `make lint` rule was dropped; the reason
  ("a fix-up commit roughly monthly") stays.
