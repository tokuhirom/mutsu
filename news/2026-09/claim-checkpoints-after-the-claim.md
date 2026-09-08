# The claim protocol gets checkpoints after the claim

Two agents worked [#7569](https://github.com/tokuhirom/mutsu/issues/7569) to
completion in parallel on 2026-09-08. Both ran `make lint`, `make test` and
`make roast` end to end; both opened a PR. One of the two was thrown away.

The claim protocol is deliberately optimistic — `docs/issue-workflow.md` says so
itself: "it does not prevent a collision, it makes both sides *agree on who
lost* and back off." The collision showed that agreement only holds if **both**
sides keep looking, and the protocol only ever asked one of them to.

## What the four steps could not see

The protocol was claim → re-read → yield if you lost → label. Every one of those
happens within seconds of arriving at the issue, and then nobody looks again. So
the winner is blind to anything that happens afterwards:

- an agent who claims **later** and declines to yield;
- a sibling PR that lands while the winner's suites are running;
- an agent that skipped the protocol altogether — it leaves no comment, but it
  does leave a PR.

The log of #7569 is the shape exactly. The first claim landed at 12:04:33
(comment 5584841010). The second landed at 12:35:19 (comment 5585216412) —
31 minutes later, higher id, so by the rule it lost. It posted a note saying so,
and proceeded anyway. The first agent then worked for another 50 minutes,
including two full-suite runs, without reading the issue once; the stand-down
note sat unread until after its PR was open.

Neither half of that is exotic. A `todo:deep` ticket takes an hour, the
pre-publication gate alone is ~40 minutes on a remote container, and the window
between claiming and publishing is where every collision lives.

## Two checkpoints, one tool call each

Step 5 now re-reads the comments **before the expensive phase** (the
pre-publication `make test` + `make roast` run, or any long build/measurement
campaign) and **immediately before opening the PR**, the second together with a
check for a PR that already closes the issue. Both are a single `get_comments`;
the alternative is discovering the collision from a merge conflict on a finished
PR, which is the most expensive moment available.

The ticket-flow skill carries the same step, and repeats the pre-publish check
inside its own "Publish" section, since that is where the PR is actually opened.

## The tiebreaker is exclusive, and now says so

The other half of the failure was that the rule read as a default rather than as
the whole rule, so it could be reasoned around. It now states that the lowest
comment id is the entire criterion, **by design** — an ordered append-only log is
the only thing every agent reads identically, so any criterion needing judgement
reintroduces the race the log exists to settle — and names the rationalizations
that do not override an earlier claim:

- "the earlier branch has nothing pushed yet and no PR open" — a claim exists
  precisely to cover the window before anything is pushed; if being unpushed
  forfeited it, it would protect nothing;
- "this session was pointed at the issue by the user" — so, routinely, was the
  other one; that is *why* two agents arrived;
- "this session is further along", "this session's approach is better".

An earlier claimant who has gone quiet is released only by a matching
`Releasing:` comment.

## Losing late is not a reason to bin the work silently

A new section says what to do when a checkpoint shows you lost: read what landed
and check it against your own findings, **verify rather than assume** (run your
repro against the merged `main`), close your PR with the comparison written down,
and offer any delta rather than pushing a third PR unprompted. The
knowledge-preservation rule applies to a PR closed as a duplicate exactly as it
does to one closed for conflicts — in this case the losing branch had found and
pinned a `state`-reset ordering bug and a `let`-scoping trap, and both were worth
recording even though the merged work covered them.
