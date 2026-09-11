---
name: ecosystem-dist-roulette
description: Pick ONE random zef distribution out of the ecosystem/ ledger, lock it on the lock board (issue #7884) so no other agent works the same one, then take it from red to green with the ecosystem-dist-fix loop and release the lock. Use when asked to make some/any module work rather than a named one ("ランダムにモジュールを一個選んで動くようにして", "pick a random dist and make its tests pass", "grab an ecosystem module and fix it"), or when running several such agents in parallel and they must not collide.
metadata:
  short-description: Draw a random distribution, lock it, make it green
---

# Ecosystem distribution roulette

One run = **one distribution, drawn at random, held under a lock, fixed, released**.

This skill is only the *selection and mutual-exclusion* wrapper. The work itself — checkout, rakudo
baseline, reduction, fix-versus-issue, `t/` pin, re-measure, PR — is
[`ecosystem-dist-fix`](../ecosystem-dist-fix/SKILL.md) and is not repeated here. Read that skill
before your first run; this page assumes it.

## Why random, and why a lock

**Random, because the parity number has to mean something.** The ledger's KPI is a claim about the
ecosystem, and it only holds if the distributions worked are a fair sample of it. An agent that
picks whichever record looks cheapest moves the count without moving the language — the same way
cherry-picking easy roast tests games the roast count, which `CLAUDE.md` already bans. A uniform
draw also surfaces the gaps nobody would have volunteered for, which is where the interesting
interpreter bugs live.

**A lock, because a distribution is not an issue.** Parallel agents already avoid colliding on
issues by claiming them in the issue's own comments (`docs/issue-workflow.md`). A distribution has
no issue to carry that claim, and nothing else in the repo can: a lock file in git conflicts and
goes stale, and a label is a read-then-write two agents can both win. So the claim lives on one
long-lived board issue, whose comments are an append-only log GitHub returns in creation order —
the only thing every agent reads identically.

The board does not prevent a collision. It makes both sides **agree on who lost**, which is all an
optimistic protocol can do, and all it needs to do.

**The lock board is [tokuhirom/mutsu#7884](https://github.com/tokuhirom/mutsu/issues/7884)** — the
single open issue labelled `ecosystem:lock`. If that number is ever wrong, the label is the
authority: `gh issue list --repo tokuhirom/mutsu --label ecosystem:lock` (remote: `list_issues` with
`labels: ["ecosystem:lock"]`), and there is exactly one.

## 1. Read the board first

```sh
gh issue view 7884 --repo tokuhirom/mutsu --comments
```

Remote container: `issue_read` with `method: "get_comments"`, `owner: tokuhirom`, `repo: mutsu`,
`issue_number: 7884`. Page to the end — the log is oldest-first and the live locks are spread
through it, not only at the bottom.

Build the held set: a `Locking: <dist> <branch>` line is **live** until a matching
`Unlocking: <dist> <branch>` (same distribution, same branch) appears after it. Keep the comment id
of every live lock; you will need it in step 3.

## 2. Draw a shortlist

```sh
.agents/skills/ecosystem-dist-roulette/pick-dist.py --exclude Held::One --exclude Held::Two
```

It samples uniformly from `ecosystem/dists/**.json`, default pool `red` / `partial` /
`blocked_load` on the `pure` axis — the records where fixing the interpreter is what moves them.
`--count`, `--status`, `--axis any`, `--exclude-file`, `--seed` and `--json` are there; `--pool`
prints the whole matching set instead of a sample. Pass every held distribution as `--exclude`, so
the shortlist is already lock-free.

**Take the first candidate. Skipping one because it looks hard is cherry-picking** and defeats the
reason the draw is random. There are exactly three legitimate reasons to move to the next
candidate, and all three are facts, not impressions:

- the board holds it (which `--exclude` has already handled);
- an open PR or a live `working` issue already covers it — check before you spend anything;
- rakudo does not pass its files either, so the record is `no_baseline` and nothing here is charged
  to mutsu. You discover this in `ecosystem-dist-fix` step 3, *after* locking; release the lock
  saying so, then draw again.

`guts` and `native` records are usually the issue-filing case rather than the fix case. That is a
reason to expect an issue as the outcome, not a reason to re-roll; the default `--axis pure` already
keeps them out unless you asked for them.

## 3. Lock it

Post one comment to #7884 whose **first line is exactly**:

```
Locking: String::Utils claude/ecosystem-string-utils-ab12
```

The distribution as the ledger spells it (`::`, not the filename's `--`), then the branch you will
push. The branch is what identifies you — every agent posts as the same GitHub user, so a lock that
names no branch names nobody.

```sh
gh issue comment 7884 --repo tokuhirom/mutsu --body 'Locking: String::Utils claude/<branch>'
```

Then **read the comments again** and compare ids. Among the live locks for *that distribution*, the
**lowest comment id wins** — ids increase, so it is the earliest entry. Compare ids, never
`created_at`: it has one-second resolution and two agents can tie on it.

If you did not win, you lost. Post `Unlocking: <dist> <your-branch>`, go back to step 2's next
candidate, and do not argue the point: "my branch is further along", "the other lock has nothing
pushed", "the user pointed me at this one" do **not** override an earlier live lock. That rule is
exclusive by design and it is the same one, for the same reason, as
[`docs/issue-workflow.md`](../../../docs/issue-workflow.md) — a criterion that needs judgement
reintroduces the race the log exists to settle.

### Breaking a stale lock

Sessions die and leave locks behind. Break one **only** on evidence:

- its comment is more than **24 hours** old, **and**
- `git ls-remote --heads origin <their-branch>` is empty, or that branch's PR is merged or closed.

Then post, before your own `Locking:` line:

```
Unlocking: String::Utils claude/their-branch (stale: no origin branch after 26h)
```

Both conditions are checkable by anyone, which is the point. A lock whose branch is simply not
pushed *yet* is normal — the 24-hour floor is what protects it, so never break a lock on age alone
or on "looks abandoned".

## 4. Do the work

Hand over to [`ecosystem-dist-fix`](../ecosystem-dist-fix/SKILL.md) and follow it as written: read
the record, `checkout-dist.py`, load probe, rakudo first and mutsu second per file, reduce into
`tmp/`, fix what is bounded and file a `tokuhirom/mutsu` issue for what is not, pin every fix with a
`t/` test, re-measure the record, and open the PR with auto-merge.

Two things that belong to this wrapper rather than that one:

- **Mention the lock in the PR body** ("locked on #7884"), so a reviewer can see the run was
  serialized and can find the release.
- **Re-read the board at `ecosystem-dist-fix`'s own checkpoints** — before the pre-publication
  `make test` + `make roast` run, and immediately before opening the PR. Steps 1-3 settle only the
  locks that existed in the first few seconds; they cannot see an agent who locks later and declines
  to yield, nor a PR that lands while your suites run. If you lost at a checkpoint, follow "Losing a
  claim late" in `docs/issue-workflow.md`: read what landed, verify your repro against it, close
  your PR with the comparison written down, and offer the delta rather than pushing it.

## 5. Release, always

The moment the run ends — merged, filed, blocked, `no_baseline`, or you simply stopped:

```sh
gh issue comment 7884 --repo tokuhirom/mutsu --body 'Unlocking: String::Utils claude/<branch>
Merged as #7901. t/03 still needs #7902 (nqp::unipropcode).'
```

The first line is the machine-readable release; the rest is one line on how it ended. A lock that is
never released removes that distribution from every other agent's pool for good, and the release
note is what makes the board double as the record of what has actually been *attempted* — which the
ledger cannot show, because a distribution that was tried and correctly ended in an issue still
reads `red`.

Releasing is not optional on a bad outcome. **"I found nothing and stopped" is exactly the case
where the next agent most needs to see the lock gone**, and to read why.

## Done means

One distribution: locked, worked to `ecosystem-dist-fix`'s finish line (record `green`, or every
remaining non-`parity` baseline file explained by an open `tokuhirom/mutsu` issue named in the PR),
and unlocked with a one-line outcome.

Then, if you were asked for several: draw again from step 1 with a **fresh** shortlist — the board
has moved while you worked, and a candidate list from an hour ago is stale.
