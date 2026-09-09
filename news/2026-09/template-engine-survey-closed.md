# The template-engine survey is closed: the field is healthy under mutsu

`Every Raku template engine is blocked on mutsu bugs`
([#7553](https://github.com/tokuhirom/mutsu/issues/7553), opened as
`todo/deep/template-engines-blocked-on-mutsu.md` on 2026-07-25) recorded a field
where **every** candidate for the template battery slot was healthy under raku
and broken under mutsu. The battery decision was blocked on interpreter work,
and the ticket existed to find out which interpreter work.

It is now closed. Both columns were re-measured in full on 2026-09-09 against
`main` @ `a75c551`, every tarball re-fetched from the REA archive at the version
that archive currently serves:

| dist | version | raku | mutsu | lowest recorded |
| --- | --- | --- | --- | --- |
| `Template::Mustache` | 1.2.6 | 11/13 ¹ | **13/13** | 1/13 |
| `Template6` | 0.16.0 | 12/12 | **12/12** | 0/12 |
| `Template::Mojo` | 0.2.2 | 5/5 | **5/5** | 4/5 ² |
| `Template::Nest::Fast` | 0.3.0 | 10/10 | **10/10** | 0/10 |
| `Template::Classic` | 0.0.3 | 1/1 | **1/1** | 0/1 |
| `SP6` | 0.2.1 | 10/11 | **10/11** | 6/11 |
| `Template::HAML` | 0.9.6 | 84/85 | 31/85 | 14/83 |
| `Template::Jinja2` | 0.3.0 | 23/24 | 8/24 | 0/23 |

¹ raku's two `91/92-specs` failures are a harness gap — they need `JSON::Fast`
from the ecosystem, which the baseline install does not have.
² The "lowest recorded" column is the worst figure this survey ever wrote down
for the row, not necessarily its state on day one; `Template::Mojo`'s day-one
count predates the first recorded measurement.

**Six of the eight dists are at parity with raku**, including both engines that
were ever in contention for the slot. The premise of the ticket — "the whole
field is broken" — no longer holds, so the ticket has nothing left to track.

## What it cost, and what it bought

The survey's own standing instruction was to reduce each row by deleting
constructs until a divergence falls out, rather than theorising from the first
error line. Every single row obeyed it: **not one** of the blockers was in
template machinery, and not one was where the recorded symptom pointed. The
count is roughly two dozen general interpreter bugs, in subsystems as unrelated
as the regex engine, the argument binder, `IO::Path` timestamps, closure package
resolution, and array subscript assignment. The last three, all fixed on the
final day:

- an attribute/parameter default that *calls* a file-scoped sub could not see it
  when `.new` ran inside another module's `BUILD`
  ([#7733](https://github.com/tokuhirom/mutsu/issues/7733), #7741) — one root
  cause worth **28** `Template::HAML` files, since it killed the dist at load;
- an aliased subrule (`$<part> = <text>`) inside an alternation lost the
  subrule's `.made` ([#7730](https://github.com/tokuhirom/mutsu/issues/7730),
  #7735) — the whole of `Template::Classic`;
- a resolved `Sub` value invoking the wrong closure
  ([#7729](https://github.com/tokuhirom/mutsu/issues/7729), #7782) — the four
  `Template::Jinja2` files that used to *abort* with a stack overflow now run to
  completion and report ordinary TAP failures.

The measurement discipline paid as reliably as the reduction discipline. Both
2026-09-09 runs found recorded rows that no longer held — one of them recorded
the previous day, one of them (`Template::HAML` at 39/83) never reproducible at
all. Re-measuring before quoting a row was not ceremony; it was the step that
kept the work aimed at real bugs.

## What is left, and where it lives now

`Template::HAML` (31/85) and `Template::Jinja2` (8/24) are still short of raku,
but neither is blocked any more: both dists load, and every remaining failure is
ordinary per-feature compatibility work — a grammar action's result missing, a
codegen divergence, a filter gap. That is not a battery blocker, it is the
normal compatibility backlog, and it does not need a deep ticket of its own to
sit in.

`docs/batteries/templates.md` carries the full table, the per-row first
observed failure, and the reproduction procedure. The bundling decision that
this survey was blocking is now unblocked and tracked separately.
