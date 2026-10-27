---
name: ecosystem-dist-queue
description: Take the next zef distribution off the ordered ecosystem queue (`scripts/ecosystem-tickets.py --queue`), lock it on the lock board (issue #15044) so no other agent works the same one, diagnose it with the ecosystem-dist-fix loop, then fix it or file its root cause in the right lane, and release the lock. Use when asked to make some/any module work rather than a named one ("モジュールを一個動くようにして", "work the ecosystem queue", "grab the next ecosystem module and fix it"), or when running several such agents in parallel and they must not collide.
metadata:
  short-description: Take the next queued distribution, lock it, fix or file it
---

# Ecosystem distribution queue

One run = **one distribution, the highest-ranked free one in the queue, held under a lock,
diagnosed, then fixed or filed, released**.

This skill is only the *selection, routing and mutual-exclusion* wrapper. The work itself —
checkout, rakudo baseline, reduction, fix-versus-issue, `t/` pin, PR — is
[`ecosystem-dist-fix`](../ecosystem-dist-fix/SKILL.md) and is not repeated here. Read that skill
before your first run; this page assumes it.

## Why an ordered queue, and why a lock

**An order, because the remainder is small enough to work in full.** This skill used to draw a
distribution at random (it was `ecosystem-dist-roulette` until 2026-10-11). The draw existed to stop
agents cherry-picking whichever record looked cheapest, which moves the parity KPI without moving
the language. With ~240 actionable records left, the same guarantee comes from a fixed order over
**the whole remainder**: nothing is skipped for looking hard, the list is simply worked top-down,
and that cannot be gamed. The order also puts the biggest payoff first — a distribution many others
depend on unblocks all of them.

**A lock, because a distribution is not an issue.** Parallel agents already avoid colliding on
issues by claiming them in the issue's own comments (`docs/issue-workflow.md`). A distribution has
no issue to carry that claim, and nothing else in the repo can: a lock file in git conflicts and
goes stale, and a label is a read-then-write two agents can both win. So the claim lives on one
long-lived board issue, whose comments are an append-only log GitHub returns in creation order —
the only thing every agent reads identically.

The board does not prevent a collision. It makes both sides **agree on who lost**, which is all an
optimistic protocol can do, and all it needs to do.

**The lock board is [tokuhirom/mutsu#15044](https://github.com/tokuhirom/mutsu/issues/15044)** — the
single open issue labelled `ecosystem:lock`. If that number is ever wrong, the label is the
authority: `gh issue list --repo tokuhirom/mutsu --label ecosystem:lock` (remote: `list_issues` with
`labels: ["ecosystem:lock"]`), and there is exactly one. (The board rotates to a fresh issue once
its comment log grows too large for a single `get_comments` call to return — the procedure is the
[`ecosystem-lock-board-rotation`](../ecosystem-lock-board-rotation/SKILL.md) skill. #7884 was the first board, closed 2026-09-21 at 318 comments.)

## 1. Compute the queue

```sh
scripts/ecosystem-ledger.sh pull     # the records live on the `ecosystem-data` branch
scripts/ecosystem-tickets.py --queue             # the head, 30 rows
scripts/ecosystem-tickets.py --queue --all       # the whole queue (--json OUT for a file)
```

The queue is every `red` / `partial` / `blocked_load` record (any axis), ordered by

1. **dependents**, descending — how many other measured distributions have it in their dependency
   closure (`deps.resolved`);
2. **failing files**, ascending — its baseline files graded `regression` / `partial`;
3. the distribution name, so the order is a pure function of the ledger.

Left out automatically: `ecosystem/exclude.txt` (confirmed dead ends, see step 6), a `blocked_load`
whose every load error is `raku_also_fails`, and a distribution whose every failing file is an
accepted divergence (`ecosystem/accepted-divergences.toml`). Each row also shows the first failure's
root-cause signature and, when that signature fails **3 or more** distributions, its cluster id —
that is the step 3 (c) case.

The order is recomputed from the ledger every time; never commit a generated queue file (the
script's docstring says why). The ledger only changes with the nightly sweep, so the queue does too.

## 2. Read the board, take the first free entry

```sh
gh issue view 15044 --repo tokuhirom/mutsu --comments
```

Remote container: `issue_read` with `method: "get_comments"`, `owner: tokuhirom`, `repo: mutsu`,
`issue_number: 15044`. Page to the end — the log is oldest-first and the live locks are spread
through it, not only at the bottom. If a single `get_comments` call now exceeds the tool's response
size limit, the board needs rotating again — follow
[`ecosystem-lock-board-rotation`](../ecosystem-lock-board-rotation/SKILL.md) before locking.

Build the held set: a `Locking: <dist> <branch>` line is **live** until a matching
`Unlocking: <dist> <branch>` (same distribution, same branch) appears after it. Keep the comment id
of every live lock; you will need it in step 4.

Then walk the queue from rank 1 and **take the first entry that**

- has no live lock on the board, and
- has no open issue already tracking it — search open issues labelled `ecosystem` for the
  distribution name (`gh issue list --repo tokuhirom/mutsu --label ecosystem --search '"<Dist::Name>"'`;
  remote: `search_issues` with `repo:tokuhirom/mutsu is:issue is:open label:ecosystem "<Dist::Name>"`),
  and no open PR working it.

Those are the only reasons to pass an entry. **Skipping one because it looks hard is
cherry-picking** and is exactly what the order exists to rule out. An entry waiting on an issue is
passed because someone owns its cause already, not because it is hard; it comes back into play when
that issue closes and the next sweep re-measures it.

## 3. Lock it, then diagnose and route

Lock it per step 4 below, then run [`ecosystem-dist-fix`](../ecosystem-dist-fix/SKILL.md)'s loop
through its diagnosis: read the record, `checkout-dist.py`, load probe, rakudo first and mutsu second
per file, reduce into `tmp/`. If rakudo does not pass the files either (`no_baseline` in fact), say
so in the unlock note and take the next entry.

Then decide by **where the root cause lives** — not by how big the fix looks:

- **(a) A small, self-contained fix in a free queue lane** (the lanes doc, `docs/lanes.md`, #12729):
  fix it, pin it with a `t/` test, open the PR with auto-merge, as `ecosystem-dist-fix` describes.
- **(b) The cause is in a campaign's area, or in a lane someone is working.** The campaigns are env /
  name resolution ([ADR-12529](../../../docs/adr/12529-frames-resolve-names-through-the-static-link.md),
  #12529), built-in method-table rows
  ([ADR-11276](../../../docs/adr/11276-built-in-methods-are-handler-rows.md), #11276) and RakuAST as
  the frontend ([ADR-10723](../../../docs/adr/10723-rakuast-is-the-frontend-ir.md), #7564). Do
  **not** fix it on the path the campaign is retiring. File an issue with its `lane:*` label and the
  `ecosystem` label (plus `todo:ticket`/`todo:deep`), make it a sub-issue of the campaign's carrier
  when the campaign's ADR covers it, unlock with `filed #N`, and take the next entry.
- **(c) The same signature fails 3 or more distributions** (the queue row shows a cluster id): file
  one issue for the cause with the
  [`ecosystem-cluster-filing`](../ecosystem-cluster-filing/SKILL.md) skill
  (`scripts/ecosystem-tickets.py --issue <id>` drafts the body) rather than a one-off fix in this
  distribution's PR, unlock with `filed #N`, and take the next entry.

`guts` and `native` records more often end in (b) or an issue than in (a); that decides the outcome,
never whether you take the entry.

### Stop condition

When every remaining queue entry is locked, waiting on an open issue, or waiting on a campaign, say
so (in the final report, listing the issues it waits on) and **stop**. The queue is re-checked after
each nightly sweep, when closed issues have turned into re-measured records.

## 4. Lock protocol

Post one comment to #15044 whose **first line is exactly**:

```
Locking: String::Utils claude/ecosystem-string-utils-ab12
```

The distribution as the ledger spells it (`::`, not the filename's `--`), then the branch you will
push. The branch is what identifies you — every agent posts as the same GitHub user, so a lock that
names no branch names nobody.

```sh
gh issue comment 15044 --repo tokuhirom/mutsu --body 'Locking: String::Utils claude/<branch>'
```

Then **read the comments again** and compare ids. Among the live locks for *that distribution*, the
**lowest comment id wins** — ids increase, so it is the earliest entry. Compare ids, never
`created_at`: it has one-second resolution and two agents can tie on it.

If you did not win, you lost. Post `Unlocking: <dist> <your-branch>`, go back to step 2's next
entry, and do not argue the point: "my branch is further along", "the other lock has nothing
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

## 5. Do the work (case a)

Hand over to [`ecosystem-dist-fix`](../ecosystem-dist-fix/SKILL.md) and follow it as written: fix
what is bounded and file a `tokuhirom/mutsu` issue for what is not (label it `ecosystem` and its
`lane:*` besides its kind label), pin every fix with a `t/` test, and open the PR with auto-merge.

Three things that belong to this wrapper rather than that one:

- **Do NOT re-measure** (user decision, 2026-10-01). Skip `ecosystem-dist-fix` step 7 entirely:
  no `--only` re-measure. Parallel PRs each rewrote their own record from a different
  `main`, and the records conflicted constantly; that is why the records now live on the
  `ecosystem-data` branch, which only the nightly sweep writes. Verify the fix with
  the distribution's own test files (step 3) and the `t/` pin, and report "file X now passes under
  mutsu" in the PR body in place of a record transition.
- **Mention the lock in the PR body** ("locked on #15044"), so a reviewer can see the run was
  serialized and can find the release.
- **Re-read the board at `ecosystem-dist-fix`'s own checkpoints** — before the pre-publication
  `make test` + `make roast` run, and immediately before opening the PR. Steps 2-4 settle only the
  locks that existed in the first few seconds; they cannot see an agent who locks later and declines
  to yield, nor a PR that lands while your suites run. If you lost at a checkpoint, follow "Losing a
  claim late" in `docs/issue-workflow.md`: read what landed, verify your repro against it, close
  your PR with the comparison written down, and offer the delta rather than pushing it.

## 6. Release, always

The moment the run ends — merged, filed, blocked, `no_baseline`, or you simply stopped:

```sh
gh issue comment 15044 --repo tokuhirom/mutsu --body 'Unlocking: String::Utils claude/<branch>
Merged as #7901. t/03 still needs #7902 (nqp::unipropcode).'
```

The first line is the machine-readable release; the rest is one line on how it ended (`filed #N`
for cases b and c). A lock that is never released removes that distribution from every other
agent's queue for good, and the release note is what makes the board double as the record of what
has actually been *attempted* — which the ledger cannot show, because a distribution that was tried
and correctly ended in an issue still reads `red`.

Releasing is not optional on a bad outcome. **"I found nothing and stopped" is exactly the case
where the next agent most needs to see the lock gone**, and to read why.

### The dead-end case: add it to `ecosystem/exclude.txt`

Sometimes the finding is not "too big to fix now" but "cannot be fixed, ever, without reversing a
decision the project already made and does not intend to revisit" — a distribution whose whole
suite depends on mutsu spoofing something it has explicitly, permanently decided never to spoof
(`Rakudo::Version` gating on `$*RAKU.compiler.name eq 'rakudo'` against ADR-0104's outright
rejection of that is the worked example). That is not the same as `no_baseline`: rakudo passes it
fine, and the record stays `red` forever, correctly.

For exactly this case — not for an ordinary `todo:ticket`/`todo:deep` finding that a future PR might
still close — add one line to `ecosystem/exclude.txt` (format and rules in its own header) citing
the issue you filed per `ecosystem-dist-fix` step 5. `--queue` reads that file automatically, so
no later run re-spends an investigation on the same dead end. Do this before releasing the lock,
and mention the exclude-list entry in the unlock comment.

When the dead end is **one test file** rather than the whole distribution (rakudo's pass rests on
an implementation artefact the project decided not to copy), add a per-file entry to
`ecosystem/accepted-divergences.toml` instead, following `ecosystem-dist-fix` step 5. The file is
graded `accepted` and stops counting as failing, while the rest of the distribution stays queued.

## Done means

One distribution: locked, then either worked to `ecosystem-dist-fix`'s finish line (every baseline
file passes by direct run, or each one still failing is explained by an open `tokuhirom/mutsu` issue
named in the PR; the ledger record is *not* touched, see step 5) or routed to a filed issue (case b
or c), and unlocked with a one-line outcome.

Then, if you were asked for several: go back to step 1 with a **fresh** read of the board — it has
moved while you worked — until the stop condition holds.
