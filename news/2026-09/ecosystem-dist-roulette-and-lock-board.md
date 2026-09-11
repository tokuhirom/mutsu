# Picking an ecosystem distribution at random, and locking it so two agents cannot pick the same one

`ecosystem-dist-fix` (added the day before) answers "make `String::Utils`'s tests
pass". It does not answer the request that actually scales the parity campaign:
*make some module work* — no name given, several agents at once. That request has
two problems the per-distribution loop does not have, and neither is about
Raku.

**The first is sampling.** The ledger publishes a figure about the ecosystem, and
the figure is only worth reading if the distributions that got worked are a fair
sample of it. An agent left to choose for itself picks the record with one
failing assertion over the one that will not load, which moves the count without
moving the interpreter — the same trade `CLAUDE.md` already bans for roast, where
cherry-picking easy tests is called out by name. So the draw is uniform over the
actionable records, and the skill says outright that "it looks hard" is not a
reason to re-roll. The three legitimate reasons to skip a candidate are all
facts: somebody holds it, an open PR already covers it, or rakudo fails it too
(`no_baseline`, which is never charged to mutsu).

**The second is mutual exclusion.** Parallel agents already avoid colliding on
issues by claiming them in the issue's own comments. A distribution has no issue
to carry that claim, and nothing in the repository can carry it either: a lock
file in git conflicts on every write and outlives the session that wrote it, and
a label is a read-then-write two agents can both win — the same reason
`docs/issue-workflow.md` demotes the `working` label to a fast filter and makes
the comment log the record.

So the claim goes on a **lock board**:
[#7884](https://github.com/tokuhirom/mutsu/issues/7884), one long-lived issue
labelled `ecosystem:lock`, never worked and never closed. An agent posts
`Locking: <Dist::Name> <branch>`, re-reads, and yields unless its comment id is
the lowest among the live locks on that distribution; it posts
`Unlocking: <Dist::Name> <branch>` plus one line on how the run ended. That is
deliberately the protocol issue claims already use, pointed at a different
resource — an append-only log GitHub returns in creation order is the only thing
every agent reads identically, so the board does not prevent a collision, it
makes both sides agree on who lost.

Two details are new, because a board has failure modes a per-issue claim does
not:

- **Breaking a lock its session died holding** is evidence-based, never a
  judgement call: the comment must be more than 24 hours old **and** the branch
  must be absent from `origin` (or its PR merged/closed). Both halves are
  checkable by anyone, which is what stops "looks abandoned" from reintroducing
  the race. The 24-hour floor is what protects a fresh lock that has nothing
  pushed yet.
- **Releasing is mandatory on a bad outcome too.** A distribution that was tried
  and correctly ended in a filed issue still reads `red` in the ledger, so the
  release note is the only record that it was attempted at all — and an
  unreleased lock removes that distribution from every other agent's pool for
  good.

## What landed

- `.agents/skills/ecosystem-dist-roulette/SKILL.md` — the draw, the lock, the
  handover to `ecosystem-dist-fix`, and the release.
- `.agents/skills/ecosystem-dist-roulette/pick-dist.py` — uniform sampler over
  `ecosystem/dists/**.json`; default pool `red` / `partial` / `blocked_load` on
  the `pure` axis (104 of the 251 records today), with `--exclude` for held
  locks, plus `--count`, `--status`, `--axis`, `--exclude-file`, `--seed`,
  `--json` and `--pool`.
- Issue #7884, and the `ecosystem:lock` label that identifies it if the number
  ever drifts.
- `ecosystem-dist-fix` gained step 1b: take the lock **however** you arrived at
  the distribution. A user naming one does not make it unclaimed, and a named-dist
  run that skips the board can still collide with a random one.
- `docs/issue-workflow.md` records the generalization — locking a resource that
  is not an issue — next to the claim protocol it reuses; `ecosystem/README.md`
  and `docs/ecosystem-parity.md` point at the skill.

No `src/`, `t/`, `Makefile` or `scripts/` change.
