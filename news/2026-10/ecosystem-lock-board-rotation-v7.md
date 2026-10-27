# Ecosystem lock board rotated from #12725 to #15044

The ecosystem lock board reached 325 comments, past the ~250 rotation threshold, so it moved
from #12725 to a fresh issue, #15044 (v7). The old board is closed with a redirect comment.

- Carried to the new board: Red (`fix/14094-red-clusters-d`, originally locked 2026-10-27T01:40:17Z).
- Not carried: BDD::Behave `eco/bdd-behave` (stale: no origin branch after 82h; unlocked on the old
  board first). Three orphan unlocks (Test::Async, Rakudo::CORE::META, and BDD::Behave under the
  branch `eco/bdd-behave-imported-attr-type`) matched no live lock.
- The skills, docs, `ecosystem/README.md`, `PLAN.md` and `.github/scripts/sync-working-label.sh`
  now point at #15044.
- The new board's body names `ecosystem-dist-queue` as the procedure; the old body still linked
  the retired `ecosystem-dist-roulette` path.
