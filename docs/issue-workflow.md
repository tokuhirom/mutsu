# GitHub issues — the open-findings queue

Discovered bugs and missing features that are too large to fix right now are
tracked as **GitHub issues on `tokuhirom/mutsu`**, one issue per finding.

This replaced the in-repo `todo/` directory on 2026-09-08. The 57 files that
were open at that point were migrated verbatim; the frozen slug-to-issue map is
[todo-issue-map.md](todo-issue-map.md), which is what resolves the `todo/...md`
paths still cited from older ADRs, `news/` entries and source comments.

## The one hard constraint

**Only ever file, edit, label, comment on or close issues in
`tokuhirom/mutsu`.** Never touch issues in any other repository — above all not
the Raku organization's (`roast`, `raku-doc`, `rakudo`), where an AI has
actually mis-filed a mutsu issue before. This applies to pull requests too. If a
task seems to want an issue elsewhere, stop and ask the user.

## Why issues and not files

A file path is not a durable reference. A resolved finding is `git mv`d to
`news/YYYY-MM/<slug>.md`, so every code comment, ADR and news entry that cited
its `todo/...md` path rots the moment it is fixed — at migration time **108 of
the 122 distinct `todo/` paths cited from `src/` were already dangling**. An
issue number survives being closed, so an issue reference in a code comment
keeps resolving forever, and GitHub shows the PR that closed it.

Issues also give the backlog things a directory of files cannot: cross-linking
between findings, the PR that closed one, a comment thread recording an
investigation that did not end in code, and — the reason the `working` label
below exists — a place for parallel agents to see what someone else already
started.

## Labels

Two independent axes. Both use a prefix so they never collide with the
`feat` / `fix` / `perf` / `docs` / `maintenance` labels that `label-pr.yml`
applies to *pull requests* for release-note categorization.

### Kind — what sort of work it is

| Label | Meaning |
| --- | --- |
| `todo:ticket` | Small, self-contained, well-scoped. Pick it up and finish it in a session (a missing method, a parser slice, a narrow compat gap). Low risk, no design needed. |
| `todo:deep` | Deep and hard. High blast radius, multi-session, needs design or an ADR before touching (dual-store decoupling, GC, large refactors, gnarly semantics). The body must capture enough analysis that a future session can pick it up cold. |
| `todo:perf` | mutsu is *correct but slow*. Split out because the **process** differs, not the size: it needs profiling rather than a guessed change, its numbers must come from the bench CI (see "Benchmark numbers in documents" in `CLAUDE.md`), and its implementation agent must run **solo** — parallel perf agents produce measurements that drift and never converge. |

**Which kind a finding gets.** `todo:perf` is decided by the *next step*, not by
the flavour: a finding is `todo:perf` only if its own next step is
measurement/profiling, or the fix is perf-only, or it is blocked purely on a
design/perf tradeoff. **A perf-flavoured finding that also fixes a genuinely
wrong answer is `todo:ticket` or `todo:deep`** — correctness ranks above speed,
and burying it under `todo:perf` would hide a real bug behind a benchmark.

The kinds are a guide, not a wall. Relabel freely: a `todo:deep` problem that
turns out to be a quick fix becomes `todo:ticket`, and a `todo:perf` finding
that profiling reveals to be a wrong answer rather than a slow one moves out of
`todo:perf`.

### Tier — how much it matters

Assigned by the triage regen (see [triage.md](triage.md)), not by the filer.
Leave it off when you file; a missing tier just means "not yet triaged".

| Label | Meaning |
| --- | --- |
| `tier:S` | Soundness. Silent data loss, hangs, wrong answers with no diagnostic. |
| `tier:B` | Broad correctness — a whole language construct, or a dist-blocking battery gap. |
| `tier:N` | Narrow correctness, diagnostics, permissiveness. |
| `tier:icebox` | Not in the queue. **Why** it is not is a separate label — see the icebox-reason axis below, which every `tier:icebox` issue also carries. |

### Icebox reason — why an iceboxed issue is not queued

`tier:icebox` says an issue is out of the queue; it does not say *why*, and the
reasons are not interchangeable. One is unblocked by somebody making a decision,
another by an unrelated PR merging, another by nothing at all. Carrying them
under one label made the whole icebox read as "ignore this", which is wrong for
three of the four.

**Every `tier:icebox` issue also carries exactly one `icebox:*` label**, chosen
by a single question: *what would make this actionable?*

| Label | Meaning | What makes it actionable |
| --- | --- | --- |
| `icebox:decision` | A design or product call has to be made before any code. The shape of the fix is genuinely open — typically the body says it "wants an ADR paragraph", or names a measurement whose result decides the design. | Somebody deciding: an ADR, an amendment to one, or a user call on a question CLAUDE.md reserves for them. |
| `icebox:blocked` | The design is settled; it waits on *other* work landing — another issue, or a slice of an ADR that already exists. **The body must name the blocker** as an issue number or an ADR slice. A blocker named only as a `todo/...md` path is stale: add a comment giving the issue number. | That work merging. The issue then becomes ordinary queue work. |
| `icebox:opportunistic` | Real, understood, and measured as not worth a session of its own — a corpus scan found no consumers, or what remains is a non-gating cleanup. Not blocked on anything and not waiting for a decision. | Somebody being in that code for another reason. Land it as a rider on the next change that touches the same plumbing. |
| `icebox:record` | No actionable next step at all. The issue exists so a settled decision or an expensive measurement is not re-derived, and it states its own reopen conditions. | Only those reopen conditions coming true — and then a re-measurement, never the recorded numbers. |

The distinction that matters most is **`decision` vs `blocked`**: a `decision`
issue is waiting on *this project's judgement* and can be unblocked in a single
conversation, while a `blocked` one is waiting on *code* and cannot be hurried
by talking about it. Reading an icebox listing without that split makes both
look equally dead.

Relabel freely as the state changes — the reason is a property of *now*, not of
the filing. An `icebox:decision` issue becomes `icebox:blocked` the moment its
ADR is written but not yet implemented, and an `icebox:blocked` issue whose
blocker lands leaves the icebox entirely: drop both labels and let the next
triage regen tier it.

A `tier:icebox` issue with no `icebox:*` label has not been classified since
2026-09-09. Treat that as unknown, not as `icebox:record`.

### `working` — someone is on it right now

Agents run in parallel, and two of them picking the same issue wastes a build
slot and produces conflicting PRs.

**The label is a fast filter, not the record.** Adding a label is a
read-then-write with no compare-and-set, so two agents can both read "no
`working` label" and both add it — the label alone cannot decide who wins.
(Neither can a GitHub Projects status field, for the same reason. Projects is
also unreachable from an ephemeral container: it is GraphQL-only, the MCP
surface has no project tools, and direct `api.github.com` calls are rejected.)

**Comments are the record**, because they are an append-only log GitHub returns
in creation order, so every agent that reads it sees the same winner. The claim
protocol below is optimistic — it does not prevent a collision, it makes both
sides *agree on who lost* and back off.

**Every agent posts as the same GitHub user** (the token is the repository
owner's), so a claim is worthless unless it names *which* agent made it. Use the
branch name you will push — unique, and it turns up again on the eventual PR.

To claim an issue:

1. **Read its comments** (`get_comments` / `gh issue view --comments`). If a
   live claim by a different agent is already there, this issue is taken: go to
   the next one. A claim is live until a matching `Releasing:` comment appears.
2. **Post the claim.** Its first line must be exactly:

   ```
   Claiming: <your-branch-name>
   ```

3. **Read the comments again.** Among the live claims, the one with the
   **lowest comment id** wins — the list comes back oldest-first and ids
   increase, so it is the first entry. Compare ids, not `created_at`: that has
   one-second resolution and two agents can tie on it. If you did not win, post
   `Releasing: <your-branch-name>` and take a different issue.
4. **Only then add the `working` label** and start work. Re-reading *after*
   posting is what makes this converge; adding the label first defeats it.
5. **Re-read the comments again before you spend, and again before you
   publish.** Steps 1-3 settle only the claims that exist in the first few
   seconds. They cannot see an agent who claims *later* and declines to yield,
   and they cannot see work that lands while yours is running. Two checkpoints,
   one `get_comments` call each:
   - **before the expensive phase** — the pre-publication `make test` +
     `make roast` run, or any long build/measurement campaign;
   - **immediately before opening the PR** — together with a check for an open
     or merged PR that already closes the issue, since an agent that skipped
     the protocol entirely leaves no comment but does leave a PR.

   If either checkpoint shows you lost, stop and follow "Losing a claim late"
   below. A run that skips them can only discover the collision from a merge
   conflict on a finished PR, which is the most expensive possible moment.

To finish — the PR merged, you stopped, or you are blocked — post
`Releasing: <your-branch-name>` **and** remove the `working` label. An abandoned
issue that keeps either one silently removes itself from every other agent's
queue. A closing PR takes the issue out of the queue anyway, but release it
explicitly: the label and the comment are what other agents read while your PR
is still in CI.

### The comment id is the whole tiebreaker

Among live claims, the lowest comment id wins. That is the entire rule, and it
is exclusive **by design**: an ordered append-only log is the only thing every
agent reads identically, so any criterion that needs judgement reintroduces the
race the log exists to settle. None of the following overrides an earlier claim,
however true they are:

- "the earlier branch has nothing pushed yet and no PR open" — a claim exists
  precisely to cover the window *before* anything is pushed; if being unpushed
  forfeited it, it would protect nothing;
- "this session was pointed at the issue by the user" — so, routinely, was the
  other one; that is *why* two agents arrived;
- "this session is further along / already has the work finished and verified";
- "this session's approach is better".

If you are the later claim, you lost: post `Releasing:` and take another issue.
An earlier claimant who has gone quiet is not thereby released — only a matching
`Releasing:` comment releases a claim. If you believe an earlier claim is stale,
say so in a comment and take another issue anyway; do not proceed on your own
finding. (Got wrong on
[#7569](https://github.com/tokuhirom/mutsu/issues/7569): the later of two claims
noted the earlier one, judged it forfeit for being unpushed, and proceeded. Both
agents then ran the full suites and opened a PR for the same work — one of which
was thrown away.)

### Locking a resource that is not an issue — the lock board

The protocol above needs somewhere to put the claim, and it uses the issue
itself. Work that is *not* an issue has nowhere: an `ecosystem/` distribution is
picked out of a ledger of 251 records, and two agents drawing the same one waste
a build slot exactly as two agents on one issue do.

For those, the claim goes on a **lock board** — one long-lived issue whose
comments are the append-only log, holding `Locking:` / `Unlocking:` lines that
name the resource and the claiming branch. The tiebreaker, the exclusivity rule
and the "re-read before you spend and before you publish" checkpoints are the
same as above; only the resource differs. Nothing else in the repository can
carry it: a lock file in git conflicts on every write and outlives the session
that wrote it, and a label — as the `working` section explains — is a
read-then-write two agents can both win.

The one addition a board needs is a way to break a lock its session died
holding, and it is evidence-based rather than a judgement call: 24 hours old
**and** no such branch on `origin` (or its PR merged/closed). Both halves are
checkable by anyone, which is what keeps it from reintroducing the race.

The board for `ecosystem/` distributions is
[#7884](https://github.com/tokuhirom/mutsu/issues/7884) — the single open issue
labelled `ecosystem:lock`, which is the authority if the number ever drifts — and
the procedure around it is
[`.agents/skills/ecosystem-dist-roulette/SKILL.md`](../.agents/skills/ecosystem-dist-roulette/SKILL.md).
A board issue is infrastructure: it is never worked and never closed.

### Losing a claim late

Finding out at a checkpoint that someone else's work has landed is not a reason
to bin yours silently. Before closing anything:

1. **Read what landed** and check it against every finding you made. It usually
   went further than yours in some places and not in others.
2. **Verify, do not assume.** Run your own repro against the merged `main`. A
   fix that covers your bug is a fact to establish, not to infer from a commit
   message.
3. **Close your PR with the comparison written down** — what superseded it, what
   of yours was already covered, and what was not. The knowledge preservation
   rule in CLAUDE.md applies to a PR closed as duplicate exactly as it does to
   one closed for conflicts.
4. **Offer the delta, do not push it unprompted.** A gap the landed work left —
   an unpinned regression, a case it does not cover — is worth a small follow-up,
   but a third PR on an issue that just collided is the user's call.

## Filing an issue

Use the templates in `.github/ISSUE_TEMPLATE/`. Title: the finding itself, as a
sentence, the way the H1 of the old files read (`@a[0 .. $n]` hangs forever when
`$n` holds a negative value) — not a bare component name.

Body, in prose (the templates carry these headings):

- **Root cause** — what actually goes wrong, and where.
- **Affected files** — the modules/paths involved.
- **Why it is large** — why it cannot be fixed in one sitting.
- **Repro** — a minimal script or roast path that exhibits it, with mutsu's
  output and `raku`'s side by side.

A `todo:deep` issue naturally carries more analysis than a `todo:ticket` one.
Write it so a session that has never seen the problem can pick it up cold; the
body is the handoff, and there is no other record.

This page defines the labels; **how to actually work an issue that already
carries `todo:deep`** — the campaign-versus-single-problem split, the slice loop,
and when such an issue may be closed — is in
[`.agents/skills/mutsu-ticket-flow/SKILL.md`](../.agents/skills/mutsu-ticket-flow/SKILL.md).

Do not let a hard finding evaporate at the end of a session. Filing the issue
costs one tool call and is always worth it.

## Lifecycle

- **open, no `working` label and no live claim comment** → available. Claim it
  by the protocol above.
- **open + `working`** → an agent has claimed it and is on it. The claim comment
  says which branch; the label is the quick filter.
- **closed** → the fixing PR closed it (`Closes #NNNN` in the PR body), or it
  was closed as `not_planned` because it evaporated. Either way, write the
  accomplishment up as `news/YYYY-MM/<slug>.md` as before — `news/` is still the
  narrative record of what was done, and the issue is the tracking record.

`PLAN.md` stays for planned strategic/campaign work; ad-hoc discovered findings
go to issues.

Roast per-test pass/fail status stays in its own ledger,
`TODO_roast/BLOCKERS.md` — do not duplicate roast tracking in issues. (There is
no special rule for roast: a genuinely deep, non-roast-specific problem you
happen to hit via a roast test still gets a `todo:deep` issue.)

## Tooling

Some sessions have the `gh` CLI (already authenticated via
`~/.config/gh/hosts.yml`); ephemeral remote containers generally do **not**, and
direct `api.github.com` calls from them are rejected by the proxy. Use whichever
is actually present — the full environment comparison and the `gh` → MCP mapping
for PRs and workflows are in
[agent-environments.md](agent-environments.md):

```sh
# gh, where it exists
gh issue list  --repo tokuhirom/mutsu --label todo:ticket --search '-label:working'
gh issue create --repo tokuhirom/mutsu --title '...' --body-file tmp/issue.md --label todo:ticket
gh issue edit  <n> --repo tokuhirom/mutsu --add-label working
gh issue edit  <n> --repo tokuhirom/mutsu --remove-label working
gh issue view  <n> --repo tokuhirom/mutsu --comments   # check for a live claim
gh issue comment <n> --repo tokuhirom/mutsu --body 'Claiming: <branch>'
# the pre-publish checkpoint: has someone already opened a PR for this issue?
gh pr list --repo tokuhirom/mutsu --state all --search '<n> in:body' --json number,state,title
```

Otherwise use the GitHub MCP tools (`list_issues`, `issue_write`,
`add_issue_comment`, `issue_read`), always with
`owner: tokuhirom`, `repo: mutsu`. **`issue_write` with `method: "update"`
*replaces* the whole label set**, so to add `working` you must pass the issue's
existing labels alongside it — read them first, or you will silently drop its
kind and tier. `gh issue edit --add-label` / `--remove-label` do not have this
hazard.

Do **not** wrap `gh` in `dotenvx run --`: the `GH_TOKEN` in `.env` is stale and
would override the working token with bad credentials.
