# The `working` label's concurrency group has to be per-issue

[#8117](https://github.com/tokuhirom/mutsu/pull/8117) made the `working` label
*derived* from the claim log so the two could no longer disagree. The parsing
half of that worked. The delivery half had a hole, and it reintroduced exactly
the failure the PR was written to eliminate — an issue that reads as free while
somebody is on it — with the added insult that it struck hardest when the queue
was busiest.

## Caught in the act

I released [#7988](https://github.com/tokuhirom/mutsu/issues/7988) at
`12:59:07Z`. Both `Claiming:`/`Releasing:` pairs on it were then matched and
nobody else had claimed it, yet an hour later it still carried `working`. A
`workflow_dispatch` scoped to that one issue cleared it instantly — so the
parser read the comment correctly and the run for it had simply never happened:

```
44  12:59:10Z  issue_comment  -> success
43  12:59:09Z  issue_comment  -> cancelled     <-- the #7988 release
42  12:58:59Z  issue_comment  -> success
…
39  12:52:16Z  issue_comment  -> cancelled     <-- the same thing, seven minutes earlier
```

## Why a global group loses events

```yaml
concurrency:
  group: claim-label          # one constant, for every issue
  cancel-in-progress: false
```

`cancel-in-progress: false` protects a run that is already *executing*. It says
nothing about queueing, and GitHub allows only **one pending run per group**:
queueing a new one cancels the previously pending one. With a single global
key, a comment on issue B therefore cancels the still-pending run for issue A.

That would be harmless if every run reconciled the whole queue — but an
`issue_comment` run is scoped to its own event:

```yaml
ISSUE: ${{ github.event.issue.number || inputs.issue }}
```

So the run that supersedes a cancelled one reconciles a *different* issue, and
the cancelled one's issue is skipped outright. Claims and releases are dropped
in proportion to how much traffic the queue is carrying.

The 3-hourly `schedule` run is the designed backstop — its own comment says it
exists "to catch an event that was never delivered" — and it does self-heal
this. But three hours is long enough for another agent to pick up an issue
somebody is already working, which is the whole thing the label is for.

## The fix

Key the group by the issue the run is about:

```yaml
group: claim-label-${{ github.event.issue.number || inputs.issue || 'all' }}
```

Comments on different issues now never cancel one another. Collapsing two runs
for the *same* issue stays correct and is the point of having a group at all:
each run replays that issue's entire comment log, so it is idempotent, and the
surviving (newer) run sees strictly more comments than the one it replaced. The
whole-queue runs — `schedule`, and a dispatch with no `issue` — share the `all`
key and still serialise against each other as before.

## Verified before merging, not after

A bad expression in a workflow-level `concurrency` key does not fail one run; it
makes the workflow unparseable and takes down every run. Since the `inputs`
context's availability there is the one thing this change depends on, it was
checked by **dispatching the workflow from the PR branch** rather than by
reading the docs: a `workflow_dispatch` on `claude/mutsu-issue-7996-41oc6h`
reconciled #8165 and completed green, which proves the expression both parses
and evaluates on a real event. The cheap checks came first — `yaml.safe_load`
on the file, and the script's own 14-case `--self-test`, which is untouched
here because this is delivery, not parsing.
