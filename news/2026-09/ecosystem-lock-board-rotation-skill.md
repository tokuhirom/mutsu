# Lock board rotation is now a skill

The ecosystem lock board has rotated twice in two weeks: #7884 to #8977, then #8977 to #10045.
Until now the procedure was a paragraph in the board issue's own body. The
[`ecosystem-lock-board-rotation`](../../.agents/skills/ecosystem-lock-board-rotation/SKILL.md)
skill now holds it, with the trigger (about 250 comments, or an overflowing `get_comments`
read), the step order, and how to correct a mistake on an append-only log.

The skill also ships `live-locks.py`. It replays saved comment pages and prints four things:

- the live locks;
- `Releasing:`-style lines, which release nothing;
- unlocks that match no lock;
- with `--check-origin`, which live locks meet the 24-hour, no-origin-branch stale rule.

The #8977 -> #10045 rotation first carried six locks that had been released days earlier,
because the live set came from memory instead of from the log. The skill requires the script's
output as the only source for the carry list.

`AGENTS.md` lists the skill. `ecosystem-dist-roulette` points to it where it used to point to the
board's Rotation section.
