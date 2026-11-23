---
name: ecosystem-dist-queue
description: Take the next zef distribution off the ordered ecosystem queue (`scripts/ecosystem-tickets.py --queue`), claim it through its tracking issue (filing one with an `eco-dist:` marker when none exists) with the ordinary `Claiming:` comment, diagnose it with the ecosystem-dist-fix loop, then fix it or file its root cause in the right lane, and release the claim. Use when asked to make some/any module work rather than a named one ("モジュールを一個動くようにして", "work the ecosystem queue", "grab the next ecosystem module and fix it").
metadata:
  short-description: Take the next queued distribution, claim its issue, fix or file it
---

# Ecosystem distribution queue

One run = **one distribution, the highest-ranked unclaimed one in the queue, claimed through its
issue, diagnosed, then fixed or filed, released**.

This skill is only the *selection and routing* wrapper. The work itself — checkout, rakudo
baseline, reduction, fix-versus-issue, `t/` pin, PR — is
[`ecosystem-dist-fix`](../ecosystem-dist-fix/SKILL.md) and is not repeated here. Read that skill
before your first run; this page assumes it.

## Why an ordered queue, and why claims go through issues

**An order, so nothing is cherry-picked.** A fixed order over **the whole remainder** means nothing
is skipped for looking hard; the list is simply worked top-down, and that cannot be gamed. The
order also puts the biggest payoff first — a distribution many others depend on unblocks all of
them. (This skill drew at random as `ecosystem-dist-roulette` until 2026-10-11.)

**Claims go through issues.** A distribution is claimed on the open `ecosystem` issue that tracks
it, with the same `Claiming:` / `Releasing:` protocol as any other issue
([`docs/issue-workflow.md`](../../../docs/issue-workflow.md)); a distribution with no tracking issue
gets one filed first. Until 2026-11-23 distributions were locked on a dedicated lock-board issue
(last #17702) and summarized on a dashboard issue (#15488). Both were retired once dist parity
reached 99.9% and the free queue was only sweep fallout: at that size a separate lock protocol,
its rotation and the dashboard job cost more than they saved. Regressions are now triaged from the
ledger's own rollup, the "Regressed since the previous ledger" section of `ecosystem/summary.md`.

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

## 2. Find each entry's issue, take the first unclaimed one

A distribution's tracking issue is an open `ecosystem` issue whose body carries an
`eco-dist: <Dist::Name>` marker line (the name as the ledger spells it), or an
`eco-cluster: <id>` line for a cluster containing it (the line `ecosystem-tickets.py --issue`
emits). Find it with a body search:

```sh
gh api -X GET search/issues -f q='repo:tokuhirom/mutsu is:issue is:open "eco-dist: String::Utils"'
gh issue list --repo tokuhirom/mutsu --label ecosystem --state open   # the whole set
```

Remote container: `list_issues` with `labels: ["ecosystem"]`, `state: "OPEN"`, and read the
bodies. An open issue that *mentions* a queued distribution without a marker line probably tracks
it too: read it, and add the marker line if it does.

Then walk the queue from rank 1 and **take the first entry that**

- has no open issue carrying a live claim (a `working` label, or a `Claiming:` comment not yet
  followed by its `Releasing:`), and no open PR working it.

An entry with an **unclaimed** tracking issue is taken by claiming that issue. An entry with **no**
tracking issue gets one before you start: a short `todo:ticket` issue (`ecosystem` label, its
`lane:*` label, the `eco-dist:` marker line, the queue row as the symptom), which you then claim.

A claimed entry is the only one to pass. **Skipping one because it looks hard is cherry-picking**
and is exactly what the order exists to rule out.

## 3. Claim it, then diagnose and route

Claim it per step 4 below, then run [`ecosystem-dist-fix`](../ecosystem-dist-fix/SKILL.md)'s loop
through its diagnosis: read the record, `checkout-dist.py`, load probe, rakudo first and mutsu second
per file, reduce into `tmp/`. If rakudo does not pass the files either (`no_baseline` in fact), say
so in the release note and take the next entry.

Then decide by **where the root cause lives** — not by how big the fix looks:

- **(a) A small, self-contained fix in a free queue lane** (the lanes doc, `docs/lanes.md`, #12729):
  fix it, pin it with a `t/` test, open the PR with auto-merge, as `ecosystem-dist-fix` describes.
- **(b) The cause is in a campaign's area, or in a lane someone is working.** The campaigns are env /
  name resolution ([ADR-12529](../../../docs/adr/12529-frames-resolve-names-through-the-static-link.md),
  #12529), built-in method-table rows
  ([ADR-11276](../../../docs/adr/11276-built-in-methods-are-handler-rows.md), #11276) and RakuAST as
  the frontend ([ADR-10723](../../../docs/adr/10723-rakuast-is-the-frontend-ir.md), #7564). Do
  **not** fix it on the path the campaign is retiring. File an issue with its `lane:*` label and the
  `ecosystem` label when a dist file fails, hangs, times out or flakes against raku (plus
  `todo:ticket`/`todo:deep`) and an `eco-dist: <Dist::Name>` line in the
  body, make it a sub-issue of the campaign's carrier
  when the campaign's ADR covers it (when the issue you claimed already is that issue, give it the
  lane and carrier instead), release with `filed #N`, and take the next entry.
- **(c) The same signature fails 3 or more distributions** (the queue row shows a cluster id): file
  one issue for the cause with the
  [`ecosystem-cluster-filing`](../ecosystem-cluster-filing/SKILL.md) skill
  (`scripts/ecosystem-tickets.py --issue <id>` drafts the body) rather than a one-off fix in this
  distribution's PR, release with `filed #N`, and take the next entry.

`guts` and `native` records more often end in (b) or an issue than in (a); that decides the outcome,
never whether you take the entry.

### Stop condition

When every remaining queue entry's issue is claimed, or it waits on a campaign, say so (in the
final report, listing the issues it waits on) and **stop**. The queue is re-checked after
each nightly sweep, when closed issues have turned into re-measured records.

## 4. Claim protocol

Claim the tracking issue exactly as [`docs/issue-workflow.md`](../../../docs/issue-workflow.md)
says for any issue: a comment whose first line is `Claiming: <branch>`, a re-read of the comments,
and a yield to a live claim with a lower comment id — the lowest id is the whole tiebreaker. Pick
the branch name first (`eco/<slug>` or `fix/<issue>-<slug>`), since the claim line carries it. The
`working` label follows from the comments (`claim-label.yml`); never set it yourself. A claim whose
session died is released by the same evidence test as any other (older than 24 hours and no such
branch on `origin`).

## 5. Do the work (case a)

Hand over to [`ecosystem-dist-fix`](../ecosystem-dist-fix/SKILL.md) and follow it as written: fix
what is bounded and file a `tokuhirom/mutsu` issue for what is not (label it with its `lane:*` besides its kind
label, and `ecosystem` only when a dist file fails, hangs, times out or flakes against raku), pin every fix with a `t/` test, and open the PR with auto-merge.

Three things that belong to this wrapper rather than that one:

- **Do NOT re-measure** (user decision, 2026-10-01). Skip `ecosystem-dist-fix` step 7 entirely:
  no `--only` re-measure. Parallel PRs each rewrote their own record from a different
  `main`, and the records conflicted constantly; that is why the records now live on the
  `ecosystem-data` branch, which only the nightly sweep writes. Verify the fix with
  the distribution's own test files (step 3) and the `t/` pin, and report "file X now passes under
  mutsu" in the PR body in place of a record transition.
- **Close the tracking issue from the PR** (`Closes #N`) when the PR makes every baseline file
  pass; otherwise name it and say what still fails.
- **Re-read the issue's comments at `ecosystem-dist-fix`'s own checkpoints** — before the pre-publication
  `make test` + `make roast` run, and immediately before opening the PR. Step 4 settles only the
  claims that existed in the first few seconds; they cannot see an agent who claims later and declines
  to yield, nor a PR that lands while your suites run. If you lost at a checkpoint, follow "Losing a
  claim late" in `docs/issue-workflow.md`: read what landed, verify your repro against it, close
  your PR with the comparison written down, and offer the delta rather than pushing it.

## 6. Release, always

The moment the run ends — merged, filed, blocked, `no_baseline`, or you simply stopped — post
`Releasing: <branch>` on the issue you claimed, with one line on how it ended (`filed #N` for cases
b and c). A claim that is never released keeps that distribution out of every other agent's queue
until the stale-claim test frees it, and the release note is the record of what was *attempted* —
which the ledger cannot show, because a distribution that was tried and correctly ended in an issue
still reads `red`.

Releasing is not optional on a bad outcome. **"I found nothing and stopped" is exactly the case
where the next agent most needs to see the claim gone**, and to read why.

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
no later run re-spends an investigation on the same dead end. Do this before releasing the claim,
and mention the exclude-list entry in the release comment.

When the dead end is **one test file** rather than the whole distribution (rakudo's pass rests on
an implementation artefact the project decided not to copy), add a per-file entry to
`ecosystem/accepted-divergences.toml` instead, following `ecosystem-dist-fix` step 5. The file is
graded `accepted` and stops counting as failing, while the rest of the distribution stays queued.

## Done means

One distribution: claimed through its issue, then either worked to `ecosystem-dist-fix`'s finish line (every baseline
file passes by direct run, or each one still failing is explained by an open `tokuhirom/mutsu` issue
named in the PR; the ledger record is *not* touched, see step 5) or routed to a filed issue (case b
or c), and released with a one-line outcome.

Then, if you were asked for several: go back to step 1 with a **fresh** read of the open issues —
they have moved while you worked — until the stop condition holds.
