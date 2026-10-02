# `migrate-t-layout.py --where` and a write-time placement hook for `t/`

A test's directory under `t/` is decided by its basename (`OVERRIDES`, `RULES`,
then `SUBRULES` in `scripts/migrate-t-layout.py`), but nothing said so up front:
`docs/t-directory-layout.md` described the choice as a judgment about what the
test catches, and the first sign of a mismatch was `make check-t-layout`
failing in the gate. The #11054 fix hit exactly that — its test, named
`method-...`, was written to `t/oo/` because a similar test sat there, and the
rules wanted `t/oo/method/`. In a remote container lefthook does not run, so the
gate was the earliest check.

- `scripts/migrate-t-layout.py --where NAME|NAME.t|PATH...` prints the path a
  test file must have, flags a given path that is elsewhere, an unruled name,
  and a basename already taken in another directory.
- `.claude/hooks/check-t-placement.py`, a Claude Code `PostToolUse` hook on
  `Write`, runs `--where` on every `t/**.t` written and reports a misplaced file
  to the agent immediately.
- `docs/t-directory-layout.md` §3 and `AGENTS.md` now state that the directory
  follows from the basename: choose the name, then write the file at the path
  `--where` prints.
