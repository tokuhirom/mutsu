# The ecosystem sweep runs itself now, at 03:20 JST

`ecosystem-sweep.yml` has a `schedule:` — `20 18 * * *`, which is 03:20 JST every
day — and a scheduled run measures the whole corpus, rolls up, appends a
`history.tsv` row and lands the records as a pull request that auto-merges. The
parity KPI stops being something a person has to remember to refresh.

## The decision this reverses, and why

ADR-0085 D9 said the sweep was operator-run and explicitly *not* CI-scheduled,
and its 2026-09-10 amendment — which added the `workflow_dispatch` entry point —
reaffirmed that half: "there is no `schedule:` in the workflow, and adding one
needs a new decision, not an edit here."

The new decision rests on a number D9 had estimated rather than measured. Its
rejection was costed at "20 CPU-hours of Actions time every week". The first real
corpus run on hosted runners took **78 minutes of wall time and about 5.5 hours
of job time** across 27 shards at `max-parallel: 8`, heaviest shard 43 minutes.
That is a quarter of the assumed figure, free on a public repository, and it fits
inside a night. A trade-off decided on an estimate that was wrong by 4x deserved
to be re-decided once the estimate became a measurement.

D9's other argument — that the number "moves on the timescale of interpreter
fixes rather than of pushes" — was an argument for *less often than pushes*, and
nightly is that: mutsu takes several merges a day, so a daily sweep is already
coarser than what it measures, while being fine enough to attribute a regression
to one day's merges instead of to however long it had been since somebody ran it.
It also turns `history.tsv` into a series rather than a few irregular points,
which is what makes the KPI chart worth drawing at all.

One of D9's two stated consequences is retired by this — "the KPI updates only
when someone runs it" — and the other stands untouched: this is a *measurement*,
not a gate. A nightly sweep fails no build and blocks no merge, so PLAN.md's
"working-module regression CI" is still a separate, unstarted item. A red shard
produces a warning, fewer records and a refused history row.

## The trap a scheduled workflow sets

**On a `schedule` event, `inputs.*` are all empty.** A `workflow_dispatch`
default does not apply to a scheduled run — it is not a dispatch. A workflow
written for the dispatch path alone therefore plans an *empty scope* the first
night, and would have quietly measured nothing.

Every input is now read as `inputs.x || <default>`, and the two booleans as
`inputs.x || github.event_name == 'schedule'`, so the nightly run is a full
`scope: all` sweep with `rollup` and `history` on while a dispatch keeps its
declared defaults (`scope: stale`, `history: false`). That is the thing to
remember when adding the next input: without its fallback, the nightly run gets
the empty value and nothing says so. The workflow header, ADR-0085 and
`docs/ecosystem-parity.md` §8.1 all carry the warning, because it is invisible in
the diff of whoever adds input number eleven.

Two other mechanics matter and were already in place. `concurrency:
ecosystem-sweep` with `cancel-in-progress: false` now earns its keep: a
dispatched sweep and the nightly one queue instead of racing the same records,
and queueing rather than cancelling is right because a cancelled sweep throws
away hours of measurement. And a night that changes nothing skips the rollup and
opens no pull request, so an unchanged corpus costs runner time and nothing else.

The cadence is a knob, not a principle. If a daily data PR turns out not to pay
for itself the cron moves to weekly and nothing else changes. What must not move
quietly is the `scope: all` *and* all-shards-green condition on the history row —
that is what keeps the series comparable from one point to the next.
